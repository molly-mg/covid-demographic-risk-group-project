# PACKAGES
library(janitor)

# IMPORT DATA ----
covid <- read_csv ("data/covid_example_data (1).csv")

# CLEAN DATA ----
covid <- janitor::clean_names(covid) # clean the column names
