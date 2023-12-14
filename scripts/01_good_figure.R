# PACKAGES ----

library(tidyverse)
library(ggplot2)
library(colorBlindness)

#_____________________________----

# IMPORTING DATA ----

covid <- read_csv ("data/covid_example_data (1).csv") # data imported as 'covid'

#_____________________________----

# DATA CLEANING ----

covid <- covid %>%
  # clean the column names, remove extra spaces
  janitor::clean_names() %>% 
  # Renaming variables
  rename( "age" = "case_age",
          "hospitalised"="hospitalized",
          ) %>% 
  # Remove anyone younger than zero and older than the oldest person ever
  filter(age >= 0, age <= 122,) %>%
  # Remove duplicates
  distinct()

#_____________________________----

## Selecting Data ----

outbreak_covid <- select(.data = covid,
                         age, confirmed_case,
                         hospitalised, died_covid) # selected relevant variables to create figure data set
# drop NA here rather than earlier to preserve as much data as can
outbreak_covid <- outbreak_covid %>% drop_na(age) # drop NA from age - removed 48 observations

# FIGURE ----

outbreak_covid_data <- outbreak_covid %>%
  filter(confirmed_case == "Yes") # creating a new object from which only contains confirmed cases

## Generating three plots from one dataset

confirmed_case_covid_data <- outbreak_covid_data %>%
  mutate(plot = "Confirmed Cases") %>%
  mutate(order = 1) %>%
  within(graph_outcome <- confirmed_case)

hospitalised_covid_data <- outbreak_covid_data %>%
  filter (hospitalised == "Yes") %>%
  mutate(plot = "Hospitalisations") %>%
  mutate(order = 2) %>%
  within(graph_outcome <- hospitalised)

died_covid_data <- outbreak_covid_data %>%
  filter (hospitalised == "Yes", died_covid == "Yes") %>%
  mutate(plot = "Deaths") %>%
  mutate(order = 3) %>%
  within(graph_outcome <- died_covid)

# Combine the three dataframes
outbreak_figure_data <- rbind(
  confirmed_case_covid_data, hospitalised_covid_data, died_covid_data) # combining the data into one plot using rbind
  
# Order the plots
outbreak_figure_data <- outbreak_figure_data %>%
  mutate(plot=fct_reorder(plot, age, .fun=mean, .desc=TRUE))

## Creating violin plot ----

# use of colour to convey severity
outbreak_covid_data_colours <- c("#083b19", "#117733", "#b7d6c1")

age_based_covid <- outbreak_figure_data %>%
  
  ggplot(aes(x=age,
             y = plot, colour=plot, fill=plot, )) +
  
  geom_boxplot(alpha = 0.5,
               width = 0.25,
               show.legend = FALSE,
               outlier.shape=NA)+
  
  geom_violin(alpha = 0.2,
              width = 1,
              show.legend = FALSE)+
  
  scale_colour_manual(values=outbreak_covid_data_colours,
                      aesthetics = c("colour", "fill")) +
  
  theme(legend.position = "none") +
  
  labs(x = "Age",
       y = "",
       title= "Age-Based Covid Risk",
       subtitle= "Confirmed Case, Hospitalisation and Death as a result of Covid") +
  theme_classic()

plot(age_based_covid)


