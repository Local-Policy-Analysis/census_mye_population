# Review of census robustness

# Population banter 2 July 2022

# Explorative analysis looking at the differences between the ONS 
# MYE 2020 and ONS Census 2021 population estimates

# Move order - import data
# Coerce so format of age structure of MYE is comparable with Census data
# Create long data series
# Plot class-factored comparisons and pop pyramids

library(readxl)
library(tidyverse)
library(janitor)
library(plotly)

snpp_2018 <- read_excel("input/table2.xls", sheet = "Persons", skip = 5) %>%
clean_names() %>% 
filter(age_group == "All ages") %>%
select(ons_code = code, population_2021 = x2021)

# Import MYE
mye_2020 <- read_excel("input/ukpopestimatesmid2020on2021geography.xls", sheet = "MYE2 - Persons", skip = 7) %>%
clean_names() %>%
select(ons_code = code, authority = name, geography, population_2020 = all_ages) %>% 
filter(geography %in% c("Unitary Authority", "Metropolitan District", "County", "London Borough", 'Non-metropolitan District' )) %>% 
# Just England LAs
filter(str_detect(ons_code, "^E")) 

# Census
census <- read_excel("input/census2021firstresultsenglandwales1.xlsx", sheet = "P01", skip = 6) %>%
clean_names() %>%
select(ons_code = area_code_note_2, population_census = all_persons)

# Master dataframe containing all three sets
master_population <- mye_2020 %>% 
left_join(snpp_2018) %>% 
left_join(census) 

analysis <- master_population %>%
    mutate(
    snpp_abs_accuracy = population_2021 - population_census,
    mye_abs_accuracy = population_2020 - population_census, 
    snpp_relative_error = snpp_abs_accuracy/population_census, 
    mye_relative_error = mye_abs_accuracy/population_census
    )
    
analysis_long <- analysis %>% 
    pivot_longer(contains("error"))


# TEST PLOTS - EXPLORATORY 
analysis_long %>% 
ggplot(aes(y = value, x = name, color = name), label = authority) +
geom_jitter()

analysis %>% 
ggplot(aes(y = mye_relative_error, x = geography, color = geography), label = authority) +
geom_jitter()

analysis %>% arrange(-mye_relative_error) %>%
select(authority, population_2020, population_census, mye_abs_accuracy, snpp_abs_accuracy)

# Bar charts 
analysis %>% group_by(geography) %>% 
summarise(across(where(is.numeric), sum, na.rm=TRUE)) %>% 
    mutate(
    snpp_abs_accuracy = population_2021 - population_census,
    mye_abs_accuracy = population_2020 - population_census, 
    snpp_relative_error = snpp_abs_accuracy/population_census, 
    mye_relative_error = mye_abs_accuracy/population_census
    ) %>% 
    ggplot(aes(x = geography, y = mye_relative_error, fill = mye_relative_error)) +
    geom_col()

# So what are the results?

# Data tells us:
# We know that some error is to be expected
# However, there are systematic patterns in London

# Why?
# Could this be an undercount? Unlikely the census would be insulated from the impacts of the pandemic
# Oxford and Cambridge likely artefacts of their student population - probably expected
# Could it be that commuting areas/home counties (parents houses of exmigrating younger residents) are picking up London's population?

#### Age population structure

# Put into age groups
# set up cut-off values 
breaks <- c(0,5,10,15,20,25,30,35,40,45,50,55,60, 65, 70, 75,80, 85, 90, 91)
# specify interval/bin labels
tags <- c("[0-4)","[5-9)", "[10-14)", "[15-19)", "[20-24)", "[25-29)","[30-34)", "[35-39)","[40-44)", "[45-49)","[50-54)", "[55-59)", "[60-64)", "[65-69)", "[70-74)","[75-79)", "[80-84)","[85-89)", "[90+)")

mye_age_binned <- 
read_excel("input/ukpopestimatesmid2020on2021geography.xls", sheet = "MYE2 - Females", skip = 7) %>%
rename_with(~ paste0("females_", .), -1:-4) %>% select(Code, contains("female")) %>%
left_join( 
read_excel("input/ukpopestimatesmid2020on2021geography.xls", sheet = "MYE2 - Males", skip = 7) %>%
rename_with(~ paste0("males_", .), -1:-4) %>% select(Code, contains("male"))
) %>%
clean_names() %>%
  pivot_longer(
    cols = contains("males"),
    names_to = c("sex", "age"),
    names_sep = "_",
    values_to = "population"
  ) %>%
  select(ons_code = code, age, sex, population) %>%
    type_convert() %>% mutate(sex = as.factor(str_replace(sex, "males", "male"))) %>%
  mutate(age_binned = cut(age, breaks = breaks, include.lowest = TRUE, right = FALSE, labels = tags)) %>%
  group_by(ons_code, sex, age_binned) %>%
  summarise(mye_pop_binned = sum(population))


# Will TBA - needs single year of age for resolution 

census_age_profile <- read_excel("input/census2021firstresultsenglandwales1.xlsx", sheet = "P03", skip = 7) %>%
clean_names() %>% 
  pivot_longer(
    cols = contains("aged"),
    names_to = c("sex", "age"),
    names_sep = "aged_",
    values_to = "population"
  ) %>%
  separate(age, "age") %>% 
  mutate(sex = as.factor(str_replace(sex, "males_", "male"))) %>%
  type_convert() %>% 
    mutate(age_binned = cut(age, breaks = breaks, include.lowest = TRUE, right = FALSE, labels = tags)) %>%
    select(ons_code = area_code_note_2, authority = area_name, sex, age_binned, census_pop_binned = population)

#
master_bin <- census_age_profile %>% 
left_join(mye_age_binned) %>% 
mutate(diff = mye_pop_binned - census_pop_binned)


# Long version for plotting
master_bin_long <- master_bin %>% 
pivot_longer(c(census_pop_binned, mye_pop_binned), values_to = "population", names_to = "pop_source") 

# Set a geography (can be England/Region/LA)

authority_filter <- "England" # Change this to an LTLA and then run below code

mye_bin_filtered <- master_bin_long %>%
filter(pop_source == "mye_pop_binned") %>%
filter(authority == authority_filter) %>%
mutate(
    population = ifelse(sex == "male", population*(-1),
                        population*1))

# Make population pyramid
p <- master_bin_long %>%
 filter(authority == authority_filter) %>%
filter(pop_source == "census_pop_binned") %>%
mutate(
    population = ifelse(sex == "male", population*(-1),
                        population*1)) %>%
    mutate(sex = ifelse(sex == "female", "Female - Census 2021", "Male - Census 2021")) %>%
    ggplot(aes(x = age_binned,y = population, fill = sex)) + 
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_fill_brewer(type = "seq", palette = 18) +
    theme_minimal() +
    geom_hline(yintercept = 0) +
    geom_bar(data = mye_bin_filtered, 
    aes(x = age_binned, y = population), colour = "black", fill = alpha("white", 0), stat = "identity") +
    labs(x = "Age category", 
              y = "Population",
              title = paste0(authority_filter, " - Census 2021 age sex population pyramid"),
              subtitle = "Outline = 2020 Mid-year population estimates") +
    scale_y_continuous(labels = scales::comma) +
    labs(fill = "Outline  = 2020 MYE population") +
    theme(legend.title=element_text(size=9))

print(p) 
ggplotly(p) # NB plotly does not render with outline 

ggsave(paste0("output/",authority_filter,".png"))
