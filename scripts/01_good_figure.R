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



