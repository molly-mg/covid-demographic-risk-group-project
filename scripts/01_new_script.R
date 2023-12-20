# is there a relationship between age and duration of hospitalisations? if there is a relationship, does it change for people who died?s

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

# checking for distinct observations in hospitalized_status
covid %>% distinct(hospitalized_status) 

# checking for distinct observations in covid_confirmed
covid %>% distinct(covid_confirmed)

covid %>% distinct(died_of_covid)

# making new data frame with data to be used
new_covid <- covid %>%
  filter(hospitalized_status == "Yes", # keep data only of hospitalised patients with confirmed case of covid
         covid_confirmed == "Yes",
         died_of_covid != "Under Review") %>% 
  select(pid,  # selecting the variables to be used
         case_reported,
         dob,
         age,
         hospitalized_status, 
         admission_date, 
         discharge_date,
         died_of_covid)

# Check for duplicates in pid
new_covid$pid %>% 
  duplicated() %>% 
  sum() 

# Summary of missing values
colSums(is.na(new_covid))

# Remove rows with missing values
new_covid <- new_covid %>% 
  drop_na()

# Check missing values again
colSums(is.na(new_covid))

# Checking variable types
glimpse(new_covid)

# change dates to an appropriate format
new_covid <- new_covid %>% 
  mutate(case_reported = lubridate::dmy(case_reported),
         dob = lubridate::dmy(dob),
         admission_date = lubridate::dmy(admission_date), 
         discharge_date = lubridate::dmy(discharge_date))

# Checking variable types
glimpse(new_covid)

# Making a new variable
new_covid <- new_covid %>%
  mutate(duration_hosp_days = discharge_date - admission_date) # Duration of hospitalisation

# Checking variable types
glimpse(new_covid)

# looking for input errors by looking at a summary of to find any extreme values
summary(new_covid)

# Removing extreme values
new_covid <- new_covid %>%
  filter(new_covid$duration_hosp_days >= 0, # all values less than 0
         new_covid$duration_hosp_days < 2000) # values more than 2000

covid_colours <- c("#117733", "#882255")

new_covid %>% ggplot(
  aes(x = age,
      y = duration_hosp_days)) +
  geom_point(aes(colour = died_of_covid), alpha = 0.5) +
  geom_smooth(aes(colour = died_of_covid), method = "gam", se = FALSE) + 
  scale_colour_manual(values = covid_colours, aesthetics = "colour") + 
  geom_smooth(method = "lm", se = FALSE, fill = "#332288")

# older people were hospitalised for longer than youger people
# hospitalised older people, that died of covid, died sooner than younger people
# no hospitalised people under 20(???), including children, died
# most children hospitalised were under 5