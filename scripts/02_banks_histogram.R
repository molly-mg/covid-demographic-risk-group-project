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





