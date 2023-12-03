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

covid <- rename(covid, "age" = "case_age",
                   "hospitalised"="hospitalized",) # using rename from dplyr to make variables concise

#_________________________----

# DATA EXPLORATION ----

select(.data = covid, 
       age, died_covid, hospitalised) # the variables you want to select

hospital_covid <- select(.data = covid, confirmed_case, hospitalised, age, died_covid) # selecting data of interest

#__________________________----

## EXPLORING THE VARIABLES ----

## Counting NA ----

hospital_covid %>%                                    # Count NA by group
  dplyr::summarize(count_na = sum(is.na(age)))

hospital_covid %>%                                    # Count NA by group
  dplyr::summarize(count_na = sum(is.na(died_covid)))

hospital_covid %>%                                    # Count NA by group
  dplyr::summarize(count_na = sum(is.na(hospitalised)))

hospital_covid <- hospital_covid %>% drop_na(hospitalised) # removing na hospitalised data - likely to be 'no' but cant be sure

hospital_covid <- hospital_covid %>% drop_na(died_covid) # removing na from died of covid

hospital_covid <- hospital_covid %>% drop_na(confirmed_case) # removing na from died of covid



#____________________________----


# DATA VISUALISATION ----

covid %>% 
  group_by(hospitalised) %>% 
  summarise(n = n()) # quick visualisations


covid%>%
  filter(hospitalised == "Yes") %>%
  ggplot()+
  geom_bar(aes(x=age))

covid%>%
  filter(died_covid == "Yes") %>%
  ggplot()+
  geom_bar(aes(x=age))


filter(.data = hospital_covid, hospitalised == "Yes") %>%
  ggplot(aes(age))+
  geom_histogram(bins=50, 
                 aes(y=..density..,
                     fill=hospitalised), 
                 position = "identity",
                 colour="black") # visualising only individuals who were hospitalised

filter(.data = hospital_covid) %>%
  ggplot(aes(age))+
  geom_histogram(bins=50, 
                 aes(y=..density..,
                     fill=died_covid), 
                 position = "identity",
                 colour="black") # visualising whether individuals died from covid


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


filter(.data = hospital_covid, hospitalised == "Yes", confirmed_case == "No") %>%
  summarise(count = n()) # 2 individuals who were hospitalised and did not have a confirmed case of covid








#____________________________----

# EXPLORING DISTRIBUTIONS ----



