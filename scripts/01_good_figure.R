# PACKAGES ----
library(tidyverse)
library(dplyr)
#__________________________----

# IMPORT DATA ----
source('scripts/clean_data.R')
# Data is imported as covid

#_________________________----

# DATA EXPLORATION

colnames(covid) # visualise all variables in the dataset

covid <- rename(covid, "gender" = "case_gender", 
                   "age" = "case_age",
                   "hospitalised"="hospitalized",) # using rename from dplyr to make variables concise

select(.data = covid, 
       age, gender, died_covid, hospitalised) # the variables you want to select

hospital_covid <- select(.data = covid, hospitalised, age, gender, died_covid) # selecting data of interest

## EXPLORING THE VARIABLES ----


covid %>% 
  group_by(hospitalised) %>% 
  summarise(n = n())

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













summarize(
  group_by(filter(
    covid, is.na(case_age) == FALSE, is.na(hospitalized) == FALSE
  ), died, hospitalized),
  count=n(),
  avg_age=mean((case_age))
)

hopsital_covid <- select(.data = covid, case_age, died, died_covid, hospitalized)

ggplot(data = covid, aes(x = hospitalized, y = case_age)) +
  geom_boxplot(aes(fill = hospitalized),
               alpha = 0.7, 
               width = 0.5, # change width of boxplot
               show.legend = FALSE)
glimpse(covid)

covid %>% 
  group_by(case_age) %>% 
  summarise(n = n())
  print(n = 80)

prob_obs_age <- covid %>% 
    group_by(case_age) %>% 
    summarise(n = n()) %>% 
    mutate(prob_obs = n/sum(n))

covid %>% 
  ggplot()+
  geom_bar(aes(x=case_age))

covid %>% 
  group_by(case_gender) %>% 
  summarise(n = n())

covid %>% 
  summarise(
    mean_case_age = mean(case_age, na.rm=TRUE))