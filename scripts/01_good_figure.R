# PACKAGES ----
library(tidyverse)
library(dplyr)
library(ggridges)
library(car)
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

age_summary <- hospital_covid %>% 
  summarise(mean_age = mean(age, na.rm=T), 
            sd = sd(age, na.rm = T),
            median_age = median(age, na.rm=T), 
            iqr = IQR(age, na.rm = T)) # looking at the statistical parameters of the age data

age_summary

hospital_covid %>% 
  ggplot()+
  geom_histogram(aes(x=age),
                 alpha=0.8,
                 bins = 10,
                 fill="grey",
                 colour="black")+
  geom_vline(data=age_summary,
             aes(xintercept=mean_age),
             colour="red",
             linetype="dashed")+
  geom_vline(data=age_summary,
             aes(xintercept=median_age),
             colour="blue",
             linetype="dashed")+
  labs(x = "age (years)",
       y = "count")+
  theme_classic() # looking at mean and median within the age data

hospital_covid %>% 
  pull(age) %>% 
  car::qqPlot() # qqplot to assess deviation from idealised distribution

# ___________________----


# STARTING TO FORM PLOTS ----

filter(.data = hospital_covid, confirmed_case != "NA") %>%
  ggplot(aes(y = confirmed_case,
             x = age,
             fill = confirmed_case))+ # plot 1 confirmed cases
  geom_density_ridges()

filter(.data = hospital_covid) %>%
  ggplot(aes(y = hospitalised,
             x = age,
             fill = hospitalised))+ # plot 2 hospitalised
  geom_density_ridges() 

filter(.data = hospital_covid, died_covid != "Under Review") %>%
  ggplot(aes(y = died_covid,
             x = age,
             fill = died_covid))+ # plot 3 died from covid
  geom_density_ridges() 
