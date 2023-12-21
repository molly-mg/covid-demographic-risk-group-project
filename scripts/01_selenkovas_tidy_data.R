# PACKAGES ----


library(tidyverse) # tidy data packages
library(janitor) # cleans variable names
library(lubridate) # makes sure dates are processed properly
library(colorBlindness)

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


# WRANGLING DATA----


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

# Making two new variables
new_covid <- new_covid %>%
  mutate(new_age_years = (admission_date - dob)/lubridate::dyears(1), # Age at admission
         duration_hosp_days = discharge_date - admission_date) # Duration of hospitalisation

# Checking variable types
glimpse(new_covid)

# looking for input errors by looking at a summary of to find any extreme values
summary(new_covid)

# Removing extreme values
new_covid <- new_covid %>%
  filter(new_covid$duration_hosp_days >= 0, # all values less than 0
         new_covid$duration_hosp_days < 2000,
         new_covid$new_age_years >= 0)

covid_colours <- c("#117733", "#882255")

new_covid %>% ggplot(
  aes(x = new_age_years,
      y = duration_hosp_days)) +
  geom_point(aes(fill = died_of_covid),
             shape = 21,
             colour = "black",
             size = 2,
             stroke = 0.3,
             alpha = 0.5,
             position = position_jitter(w = 0, h = 0.1)) +
  geom_smooth(aes(colour = died_of_covid), 
              method = "gam", 
              se = FALSE,
              linewidth = 5) + 
  scale_colour_manual(values = covid_colours, 
                      aesthetics = c("colour", "fill")) + 
  geom_smooth(method = "lm", 
              se = FALSE, 
              colour = "#332288",
              linewidth = 3,
              lty = "dashed") + 
  labs(x = "Age",
       y = "Duration of stay",
       title = "Hospital stay of patients with COVID-19") + 
  theme_minimal()

colorBlindness::cvdPlot()
