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


filter_mygalia_sym <- filter(.data = symptom_data, sym_myalgia == "Yes")

#Create graph ----

### Mygalia ------
sym_box_plot_mygalia <- filter_mygalia_sym %>%
  ggplot(aes(x= sym_myalgia,
             y= case_age,
             ))+
  geom_violin(alpha = 0.15,
              colour = "#332288",
              fill = "#332288")+
  geom_boxplot(width = 0.2,
               alpha = 0.7,
               colour = "#332288",
               fill = "#332288")+
  coord_flip()+ # flips to make the graph horizontal
  theme_minimal()+
  theme(axis.text.y=element_blank() # removes the labelled yes value of y axis
        )

plot(sym_box_plot_mygalia)


filter_cough_sym <- filter(.data = symptom_data, sym_cough == "Yes")

sym_box_plot_cough <- filter_cough_sym %>%
  ggplot(aes(x= sym_cough,
             y= case_age,
  ))+
  geom_violin(alpha = 0.15,
              colour = "#88CCEE",
              fill = "#88CCEE")+
  geom_boxplot(width = 0.2,
               alpha = 0.7,
               colour = "#88CCEE",
               fill = "#88CCEE")+
  coord_flip()+ # flips to make the graph horizontal
  theme_minimal()+
  theme(axis.text.y=element_blank() # removes the labelled yes value of y axis
  )

plot(sym_box_plot_cough)


### Fever -----
filter_fever_sym <- filter(.data = symptom_data, sym_fever == "Yes")

sym_box_plot_fever <- filter_fever_sym %>%
  ggplot(aes(x= sym_fever,
             y= case_age,
  ))+
  geom_violin(alpha = 0.15,
              colour = "#44AA99",
              fill = "#44AA99")+
  geom_boxplot(width = 0.2,
               alpha = 0.7,
               colour = "#44AA99",
               fill = "#44AA99")+
  coord_flip()+ # flips to make the graph horizontal
  theme_minimal()+
  theme(axis.text.y=element_blank() # removes the labelled yes value of y axis
  )

plot(sym_box_plot_fever)




sym_three_combined <- sym_box_plot_mygalia / sym_box_plot_cough / sym_box_plot_fever
plot(sym_three_combined)
