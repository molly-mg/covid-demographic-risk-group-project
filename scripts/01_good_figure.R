# PACKAGES ----
library(tidyverse)
library(dplyr)
library(ggridges)
#__________________________----

# IMPORT DATA ----
source('scripts/clean_data.R')
# Data is imported as covid

#_________________________----

# DATA CLEANING ----

colnames(covid) # visualise all variables in the dataset

covid <- rename(covid, "gender" = "case_gender", 
                   "age" = "case_age",
                   "hospitalised"="hospitalized",) # using rename from dplyr to make variables concise

#_________________________----

# DATA EXPLORATION ----

select(.data = covid, 
       age, gender, died_covid, hospitalised) # the variables you want to select

hospital_covid <- select(.data = covid, hospitalised, age, gender, died_covid) # selecting data of interest

#__________________________----

## EXPLORING THE VARIABLES ----

## Counting NA ----

hospital_covid %>%                                    # Count NA by group
  dplyr::summarize(count_na = sum(is.na(age)))

hospital_covid %>%                                    # Count NA by group
  dplyr::summarize(count_na = sum(is.na(died_covid)))

hospital_covid %>%                                    # Count NA by group
  dplyr::summarize(count_na = sum(is.na(hospitalised)))

hospital_covid %>%                                    # Count NA by group
  dplyr::summarize(count_na = sum(is.na(gender)))

hospital_covid <- hospital_covid %>% drop_na(hospitalised)

hospital_covid <- hospital_covid %>% drop_na(died_covid)
#____________________________----


# DATA VISUALISATION ----

covid %>% 
  group_by(hospitalised) %>% 
  summarise(n = n()) # quick visualisations

hospital_covid %>% 
  group_by(gender) %>% 
  summarise(n = n())

covid%>%
  filter(hospitalised == "Yes") %>%
  ggplot()+
  geom_bar(aes(x=age))

covid%>%
  filter(died_covid == "Yes") %>%
  ggplot()+
  geom_bar(aes(x=age))

hospital_covid %>% 
  ggplot(aes(age))+
  geom_histogram(bins=50, 
                 aes(y=..density..,
                     fill=hospitalised), 
                 position = "identity",
                 colour="black") # visualising hospitalisation by age

hospital_covid %>% 
  ggplot(aes(age))+
  geom_histogram(bins=50, 
                 aes(y=..density..,
                     fill=died_covid), 
                 position = "identity",
                 colour="black") # visualising hospitalisation by died from covid

filter(.data = hospital_covid, hospitalised == "Yes") %>%
  ggplot(aes(age))+
  geom_histogram(bins=50, 
                 aes(y=..density..,
                     fill=hospitalised), 
                 position = "identity",
                 colour="black") # visualising only invivduals who were hospitalised


filter(.data = hospital_covid, hospitalised == "Yes") %>%
ggplot(aes(age))+
  geom_histogram(bins=50, 
                 aes(y=..density..,
                     fill=died_covid), 
                 position = "identity",
                 colour="black") # visualising hospitalised individuals who died from covid


filter(.data = hospital_covid, hospitalised == "Yes", died_covid == "Under Review") %>%
  ggplot(aes(age))+
  geom_histogram(bins=50, 
                 aes(y=..density..,
                     fill=died_covid), 
                 position = "identity",
                 colour="black") # visualising data where covid death is under review

filter(.data = hospital_covid, hospitalised == "Yes", died_covid != "Under Review") %>%
  ggplot(aes(age))+
  geom_histogram(bins=50, 
                 aes(y=..density..,
                     fill=died_covid), 
                 position = "identity",
                 colour="black") # visualising data where covid death does not include data 'under review'


#____________________________----

# EXPLORING DISTRIBUTIONS ----



