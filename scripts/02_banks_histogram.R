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


covid <- covid %>%
  # clean the column names, remove extra spaces
  janitor::clean_names() %>% 
  # Renaming variables
  rename( "age" = "case_age",
          "hospitalised"="hospitalized",
  ) %>% 
  # Remove anyone younger than zero???
  filter(age >= 0) %>%
  # Remove duplicates
  distinct()

## Rename variables names ----


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

#______________________________________----
# Create tables with the symptoms only. ----

filter_cough_sym <- filter(.data = covid, sym_cough == "Yes")

filter_headache_sym <- filter(.data = covid, sym_headache == "Yes")

filter_fever_sym <- filter(.data = covid, sym_fever == "Yes")

filter_mygalia_sym <- filter(.data = covid, sym_myalgia == "Yes")

# Mean and median of age ----
#Need to calculate this to form the vline on the histograms

covid_age_cough_calculation <- filter_cough_sym %>% 
  summarise(mean_age_cough = mean(age, na.rm=T), 
            median_age_cough=median(age, na.rm=T))

#covid_age_cough_calculation

covid_age_headache_calculation <- filter_headache_sym %>% 
  summarise(mean_age_headache = mean(age, na.rm=T), 
            median_age_headache=median(age, na.rm=T))

#covid_age_mygalia_calculation

covid_age_fever_calculation <- filter_fever_sym %>% 
  summarise(mean_age_fever = mean(age, na.rm=T), 
            median_age_fever = median(age, na.rm=T))

covid_age_mygalia_calculation <- filter_mygalia_sym %>% 
  summarise(mean_age_mygalia = mean(age, na.rm=T), 
            median_age_mygalia = median(age, na.rm=T))


covid_age_cough_calculation

# Histogram ----

### Cough ----

his_cough <- filter_cough_sym %>%
  ggplot()+
  geom_histogram(aes(x=age),
                 bins=10,
                 alpha = 0.7,
                 fill = "#88CCEE",
                 color = "darkgrey")+
  geom_vline(data=covid_age_cough_calculation,
             aes(xintercept=mean_age_cough),
             color="black",
             linetype = "longdash")+
  geom_vline(data=covid_age_cough_calculation,
             aes(xintercept=median_age_cough),
             colour="darkorange",
             linetype = "longdash")+
  ylim(0,6000)+
  labs(y="Count",
       x= "Age (years)",
       title = "Cough")+
  theme_minimal()+
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5))

plot(his_cough)

## Headache ----

his_headache <- filter_headache_sym %>%
  ggplot()+
  geom_histogram(aes(x=age),
                 bins=10,
                 alpha = 0.7,
                 fill = "#117733",
                 color = "darkgrey")+
  geom_vline(data=covid_age_headache_calculation,
             aes(xintercept=mean_age_headache),
             color="black",
             linetype = "longdash")+
  geom_vline(data=covid_age_headache_calculation,
             aes(xintercept=median_age_headache),
             colour="darkorange",
             linetype = "longdash")+
  ylim(0,6000)+
  labs(y="Count",
       x= "Age (years)",
       title="Headache")+
  theme_minimal()+
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5))

plot(his_headache)

colorBlindness::cvdPlot()


## Fever ----


his_fever <- filter_fever_sym %>%
  ggplot()+
  geom_histogram(aes(x=age),
                 bins=10,
                 alpha = 0.7,
                 fill = "#882255",
                 color = "lightgrey")+
  geom_vline(data=covid_age_fever_calculation,
             aes(xintercept=mean_age_fever),
             color="black",
             linetype = "longdash")+
  geom_vline(data=covid_age_fever_calculation,
             aes(xintercept=median_age_fever),
             colour="darkorange",
             linetype = "longdash")+
  ylim(0,6000)+
  labs(y="Count",
       x= "Age (years)",
       title="Fever")+
  theme_minimal()+
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5))

plot(his_fever)


## Mygalia ----

his_mygalia <- filter_mygalia_sym %>%
  ggplot()+
  geom_histogram(aes(x=age),
                 bins=10,
                 alpha = 0.7,
                 fill = "#332288",
                 color = "lightgrey")+
  geom_vline(data=covid_age_mygalia_calculation,
             aes(xintercept=mean_age_mygalia),
             color="black",
             linetype = "longdash")+
  geom_vline(data=covid_age_mygalia_calculation,
             aes(xintercept=median_age_mygalia),
             colour="darkorange",
             linetype = "longdash")+
  ylim(0,6000)+
  labs(y="Count",
       x= "Age (years)",
       title= "Mygalia")+
  theme_minimal()+
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5))

plot(his_mygalia)


joint_plot_sym_hist <- (his_cough|his_headache)/(his_fever|his_mygalia)

joint_plot_sym_hist

joint_plot_sym_hist_title <- joint_plot_sym_hist + plot_annotation(
  title = "Number of people with a covid symptom by age"
)



## The graphs show a general pattern of more younger people displaying the four most common symptoms of covid.
## Headache symptoms display a greater number of under 35s getting it.
## The mean age for headache was 37.8 and a median of 35. This is around a year or two younger than the other symptoms.
## Cough shows the highest mean, with the average person getting this symptom being 41. 
## Coughing is also the most common symptom so people with a greater risk from covid should avoid those who are coughing.
## Older people should be aware of these common symptoms as the largest reason for younger people displaying them is that there are more of them getting covid.
## Identifying these common diseases soon after a person has been exposed to covid could be used to help young and more vulnerable people seek different treatment before requiring hospital.
## Or at the very least be prepared for the chance that they might have it.
