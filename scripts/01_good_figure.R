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

## Ridges ----

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


# bar chart ----

hospital_covid %>% # bar charts to assess count
  ggplot()+
  geom_bar(aes(x=age)) # looking at distribution of entire data set


filter(.data = hospital_covid, hospitalised == "Yes") %>%
  ggplot()+
  geom_bar(aes(x=age)) # dsitribution of individuals who were hospitalised


filter(.data = hospital_covid, hospitalised == "Yes", died_covid == "Yes") %>%
  ggplot()+
  geom_bar(aes(x=age))# hospitalised and died from covid


filter(.data = hospital_covid, confirmed_case == "Yes", hospitalised != "NA", died_covid != "Under Review") %>%
  ggplot()+
  geom_bar(aes(x=age))# people who had covid and either were or were not hospitalised and did or did not die


hospital_covid %>% # comapring the counts of each outcome of each variable
  ggplot(aes(x=confirmed_case, fill=confirmed_case))+
  geom_bar(position=position_dodge())

hospital_covid %>% 
  ggplot(aes(x=hospitalised, fill=hospitalised))+
  geom_bar(position=position_dodge())

hospital_covid %>% 
  ggplot(aes(x=died_covid, fill=died_covid))+
  geom_bar(position=position_dodge())


## more geoms ----

hospital_covid %>%
ggplot(aes(x = hospitalised, y = age)) +
  geom_jitter(aes(color = hospitalised),
              width = 0.1, 
              alpha = 0.7, 
              show.legend = FALSE)

## density plots

filter(.data = hospital_covid, confirmed_case != "NA") %>%
ggplot(aes(x=age, y=..density..)) + 
  geom_density(aes(fill=hospitalised), position="stack")












## Poking about

confirmed_covid <- hospital_covid %>%
  filter(confirmed_case == "Yes")



cc_df <- confirmed_covid %>%
  mutate(graph = "confirmed_case") %>%
  within(graph_outcome <- confirmed_case)

hosp_df <- confirmed_covid %>%
  mutate(graph = "hosp") %>%
  within(graph_outcome <- hospitalised)

dead_df <- confirmed_covid %>%
  mutate(graph = "dead") %>%
  within(graph_outcome <- died_covid)

graph_data <- rbind(cc_df, hosp_df) %>%
  rbind(dead_df) %>% drop_na
  
  
graph_data <- graph_data %>%
  filter(graph_outcome != "Under Review")


graph_data  %>% ggplot(aes(x = age, y = graph)) + 
  geom_density_ridges(aes(fill = graph_outcome),
                      colour = "blue",
                      alpha = 0.8,
                      bandwidth = 2,
                      scale=1,
                      panel_scaling=TRUE)
  
## https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html
dead_df %>%
  group_by(graph_outcome) %>%
  summarise(count=n())
