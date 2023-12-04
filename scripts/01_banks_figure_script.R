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
  ylim(0,110)+
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
  ylim(0,110)+
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
  ylim(0,110)+
  theme_minimal()+
  theme(axis.text.y=element_blank() # removes the labelled yes value of y axis
  )

plot(sym_box_plot_fever)


### Subjfever ----

filter_subjfever_sym <- filter(.data = symptom_data, sym_subjfever == "Yes")

sym_box_plot_subjfever <- filter_subjfever_sym %>%
  ggplot(aes(x= sym_subjfever,
             y= case_age,
  ))+
  geom_violin(alpha = 0.15,
              colour = "#117733",
              fill = "#117733")+
  geom_boxplot(width = 0.2,
               alpha = 0.7,
               colour = "#117733",
               fill = "#117733")+
  coord_flip()+ # flips to make the graph horizontal
  ylim(0,110)+
  theme_minimal()+
  theme(axis.text.y=element_blank() # removes the labelled yes value of y axis
  )

plot(sym_box_plot_subjfever)

### Loss of taste and smell ----

filter_losstastesmell_sym <- filter(.data = symptom_data, sym_losstastesmell == "Yes")

sym_box_plot_losstastesmell <- filter_losstastesmell_sym %>%
  ggplot(aes(x= sym_losstastesmell,
             y= case_age,
  ))+
  geom_violin(alpha = 0.15,
              colour = "#999933",
              fill = "#999933")+
  geom_boxplot(width = 0.2,
               alpha = 0.7,
               colour = "#999933",
               fill = "#999933")+
  coord_flip()+ # flips to make the graph horizontal
  ylim(0,110)+
  theme_minimal()+
  theme(axis.text.y=element_blank() # removes the labelled yes value of y axis
  )

plot(sym_box_plot_losstastesmell)

### Sorethroat ----

filter_sorethroat_sym <- filter(.data = symptom_data, sym_sorethroat == "Yes")

sym_box_plot_sorethroat <- filter_sorethroat_sym %>%
  ggplot(aes(x= sym_sorethroat,
             y= case_age,
  ))+
  geom_violin(alpha = 0.15,
              colour = "#DDCC77",
              fill = "#DDCC77")+
  geom_boxplot(width = 0.2,
               alpha = 0.7,
               colour = "#DDCC77",
               fill = "#DDCC77")+
  coord_flip()+ # flips to make the graph horizontal
  ylim(0,110)+
  theme_minimal()+
  theme(axis.text.y=element_blank() # removes the labelled yes value of y axis
  )

plot(sym_box_plot_sorethroat)

### Headache ----

filter_headache_sym <- filter(.data = symptom_data, sym_headache == "Yes")

sym_box_plot_headache <- filter_headache_sym %>%
  ggplot(aes(x= sym_headache,
             y= case_age,
  ))+
  geom_violin(alpha = 0.15,
              colour = "#CC6677",
              fill = "#CC6677")+
  geom_boxplot(width = 0.2,
               alpha = 0.7,
               colour = "#CC6677",
               fill = "#CC6677")+
  coord_flip()+ # flips to make the graph horizontal
  ylim(0,110)+
  theme_minimal()+
  theme(axis.text.y=element_blank() # removes the labelled yes value of y axis
  )

plot(sym_box_plot_headache)


### No symptoms

symptom_data_no <- filter(.data = symptom_data, sym_myalgia == "No",
                           sym_cough == "No", 
                           sym_fever == "No", 
                           sym_subjfever == "No", 
                           sym_losstastesmell == "No",
                           sym_sorethroat == "No", 
                           sym_headache == "No")


sym_box_plot_none <- symptom_data_no %>%
  ggplot(aes(x= sym_headache,
             y= case_age,
  ))+
  geom_violin(alpha = 0.15,
              colour = "#882255",
              fill = "#882255")+
  geom_boxplot(width = 0.2,
               alpha = 0.7,
               colour = "#882255",
               fill = "#882255")+
  coord_flip()+ # flips to make the graph horizontal
  ylim(0,110)+
  theme_minimal()+
  theme(axis.text.y=element_blank() # removes the labelled yes value of y axis
  )

plot(sym_box_plot_none)





sym_six_combined <- sym_box_plot_mygalia / sym_box_plot_cough / sym_box_plot_fever / sym_box_plot_subjfever/sym_box_plot_losstastesmell/ sym_box_plot_sorethroat
plot(sym_six_combined)


