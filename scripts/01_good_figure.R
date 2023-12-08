# PACKAGES ----

library(tidyverse)

#_____________________________----

# IMPORTING DATA ----

covid <- read_csv ("data/covid_example_data (1).csv") # data imported as 'covid'

#_____________________________----

# DATA CLEANING ----

covid <- janitor::clean_names(covid) # clean the column names, remove extra spaces

covid <- rename(covid, "age" = "case_age",
                "hospitalised"="hospitalized",) # using rename from dplyr to make variables concise

covid %>% 
  distinct() %>%
  duplicated() %>% 
  sum() # removing the 1 duplicate observation

## Selecting Data ----

hospital_covid <- select(.data = covid, age, confirmed_case, hospitalised, died_covid) # selected relevant variables to create figure data set

hospital_covid <- hospital_covid %>% drop_na(age) # drop NA from age - removed 48 observations

