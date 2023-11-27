# PACKAGES ----
library(tidyverse)
#__________________________----

# IMPORT DATA
source('scripts/clean_data.R')
# Data is imported as covid

colnames(covid)

#covid <- na.omit(covid)


#covid <- covid %>% 
#  mutate(hospitalized = recode(hospitalized, "No" = 0, "Yes" = 1)





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

