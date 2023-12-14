# PACKAGES ----


library(tidyverse) # tidy data packages
library(janitor) # cleans variable names
library(lubridate) # makes sure dates are processed properly
library(rstatix)
library(patchwork)


#__________________________----


# IMPORTING DATA ----
covid <- read_csv ("data/covid_example_data (1).csv")

head(covid) # check the data has loaded, prints first 10 rows of data frame

#__________________________----


# CHECKING DATA----

# checking variable names
colnames(covid)


#__________________________----


# CLEANING DATA ----

# renaming variables to conform to snake_case
# new names chosen to have as few matches in their first characters as possible while still be descriptive
covid <- dplyr::rename(covid,
                          "pid" = "PID",
                          "case_reported" = "reprt_creationdt_FALSE", 
                          "dob" = "case_dob_FALSE",
                          "age" = "case_age",
                          "gender" = "case_gender",
                          "race" = "case_race",
                          "ethnicity" = "case_eth",
                          "zip_code" = "case_zip",
                          "sym_status" = "Contact_id",
                          "date_sym_start" = "sym_startdt_FALSE",
                          "fever" = "sym_fever",
                          "fever_self_report" = "sym_subjfever",
                          "myalgia" = "sym_myalgia",
                          "taste_smell" = "sym_losstastesmell",
                          "sorethroat" = "sym_sorethroat",
                          "cough" = "sym_cough",
                          "headache" = "sym_headache",
                          "date_sym_resolved" = "sym_resolveddt_FALSE",
                          "lives_with_others" = "contact_household",
                          "hospitalized_status" = "hospitalized",
                          "admission_date" = "hosp_admidt_FALSE",
                          "discharge_date" = "hosp_dischdt_FALSE",
                          "died_of_covid" = "died_covid",
                          "date_of_death" = "died_dt_FALSE",
                          "covid_confirmed" = "confirmed_case",
                          "positive_pcr_date" = "pos_sampledt_FALSE")

# checking that all variables were renamed
colnames(covid)

# looking for input errors by looking at a summary of min/max values
summary(covid)
# !!!there are input errors in age, therefore we will compare age and the difference between date of case reported and dob 

# checking for distinct observations in hospitalized_status
covid %>% distinct(hospitalized_status) 

# checking for distinct observations in covid_confirmed
covid %>% distinct(covid_confirmed)

# making new data frame with data to be used
new_covid <- covid %>%
  filter(hospitalized_status == "Yes", # keep data only of hospitalised patients with confirmed case of covid
         covid_confirmed == "Yes") %>% 
  select(pid,  # selecting the variables to be used
         case_reported,
         dob,
         age,
         hospitalized_status, 
         admission_date, 
         discharge_date)

# Summary of missing values
colSums(is.na(new_covid))

# Remove rows with missing values
new_covid <- new_covid %>% 
  drop_na()

# Check missing values again
colSums(is.na(new_covid))

# Check for duplicates in pid
new_covid$pid %>% 
  duplicated() %>% 
  sum() 

# change dates to an appropriate format
new_covid <- new_covid %>% 
  mutate(case_reported = lubridate::dmy(case_reported),
         dob = lubridate::dmy(dob),
         admission_date = lubridate::dmy(admission_date), 
         discharge_date = lubridate::dmy(discharge_date))

# Making two new variables
new_covid <- new_covid %>%
  mutate(new_age_years = (admission_date - dob)/lubridate::dyears(1), # Age at admission
         duration_hosp_days = discharge_date - admission_date) # Duration of hospitalisation

# Checking variable types
glimpse(new_covid)

# Changing difftime variables to num
new_covid$duration_hosp_days <- as.numeric(new_covid$duration_hosp_days)

# Checking variable type
glimpse(new_covid)

# Summary to check for any extreme values
summary(new_covid)
# !!!Some values are too high and some are bellow zero, where they should not be

# Removing any values less than 0
new_covid <- new_covid %>%
  filter(new_covid$new_age_years >= 0)

# A scatter plot to visually check data
new_covid %>% ggplot(
  aes(x = new_age_years,
      y = duration_hosp_days)) +
  geom_point()
# !!!There are input errors in the original data for admission and/or discharge dates

# Removing input errors
new_covid <- new_covid %>%
  filter(new_covid$duration_hosp_days >= 0, # all values less than 0
         new_covid$duration_hosp_days < 2000) # values more than 2000

# Summary to check for any more input errors
summary(new_covid)

# A scatter plot to visually check data
new_covid %>% ggplot(
  aes(x = new_age_years,
      y = duration_hosp_days)) +
  geom_point()