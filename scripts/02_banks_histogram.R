### BANKS 

###04_12_2023

#Packages -----
library(usethis)
library(tidyverse) #contains dyplr
library(janitor) # cleans variable names
library(lubridate) # make sure dates are processed properly
library(colorBlindness)
library(patchwork)
library(ggplot2)


# Import data ----
covid <- read_csv("data/covid_example_data (1).csv")

# Clean data ----
covid <- janitor::clean_names(covid) # clean the column names

#Checking for duplicates ----

covid %>% 
  duplicated() %>% # produces a list of T/F statements for duplicated or not
  sum() 
#shows one potential duplicated row

covid[!duplicated(covid$pid),] # remove repated pids

## Remove Na from pid column ----

covid <- covid %>%
  drop_na(pid)


## Rename variables names ----
covid <- rename(covid,
                "age"="case_age")

colnames(covid) # check to makes sure case_age was renamed




## Rename texts in the table ----

covid <- covid %>%
  mutate(sym_myalgia = case_when(sym_myalgia == "Yes" ~ "Yes",
                                 sym_myalgia == "YES" ~ "Yes",
                                 sym_myalgia == "No"~ "No"))




## Look at what is highest ----



covid %>% 
  group_by(sym_fever)%>%
  summarise(n=n())

covid %>% 
  group_by(sym_subjfever)%>%
  summarise(n=n())

covid %>% 
  group_by(sym_myalgia)%>%
  summarise(n=n())

covid %>% 
  group_by(sym_losstastesmell)%>%
  summarise(n=n())

covid %>% 
  group_by(sym_sorethroat)%>%
  summarise(n=n())


covid %>% 
  group_by(sym_cough)%>%
  summarise(n=n())

covid %>% 
  group_by(sym_headache)%>%
  summarise(n=n())

# From this it is possible to see that cough, headache, 
# fever, myagalia have the most people with symptoms.


## Create tables with the symptoms only. ----

filter_cough_sym <- filter(.data = covid, sym_cough == "Yes")

filter_headache_sym <- filter(.data = covid, sym_headache == "Yes")

filter_fever_sym <- filter(.data = covid, sym_fever == "Yes")

filter_mygalia_sym <- filter(.data = covid, sym_myalgia == "Yes")



