### BANKS 

###20_11_2023

#Packages -----
library(usethis)
library(tidyverse) #contains dyplr
#library(knitr)
library(janitor) # cleans variable names
library(lubridate) # make sure dates are processed properly
library(colorBlindness)
library(patchwork)
library(ggplot2)
#library(gt)

# IMPORT DATA ----
covid <- read_csv ("data/covid_example_data (1).csv")

# CLEAN DATA ----
covid <- janitor::clean_names(covid) # clean the column names

#Checking for duplicates ----

covid %>% 
  duplicated() %>% # produces a list of T/F statements for duplicated or not
  sum() 
#shows one potential duplicated row

covid[!duplicated(covid$pid),]
#Used this to see if they could remove the duplicate based on the pid

## Check what is in the data ----
colnames(covid)

head(covid)


####Keep relevant data in different table ----
symptom_data <- select(.data = covid, case_age, sym_myalgia, sym_cough, 
                       sym_fever, sym_subjfever, sym_losstastesmell,
                       sym_sorethroat, sym_headache)
head(symptom_data)

tail(symptom_data)
#Use to check that it has the relevant data.


## Create table of data of people who did have symptoms
symptom_data_yes <- filter(.data = symptom_data, sym_myalgia == "Yes",
                           sym_cough == "Yes", 
                           sym_fever == "Yes", 
                           sym_subjfever == "Yes", 
                           sym_losstastesmell == "Yes",
                           sym_sorethroat == "Yes", 
                           sym_headache == "Yes")

head(symptom_data_yes) # Check to see if the right data is included


#Create graph ----
sym_box_plot <- ggplot(aes(x=))

