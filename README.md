# Autumn Assignment BIO 5023Y

Summative group assignment for module BIO 5023Y - Data Science for Biologists.

## Data
The data is a fake version of a covid linelist a data format for collecting case numbers and information for epidemiology. This data was originally provided by Batra, Neale et al. (2021), The Epidemiologist R Handbook. https://zenodo.org/badge/doi/10.5281/zenodo.4752646.svg

## Our project
A report into a recent covid outbreak for the purposes of informing health policy. The data is in linelist format.
We have chosen to focus on age as the key factor against other variables.

## 'How to' guide
Scripts for each final figure can be found in the 'figures' folder. 
The data visualisation folder contains experimental data visualisation. 
The final Rmarkdown file is contained within the project folder.
Final Rmarkdown file can be knit to view the full report.

## Packages
* usethis
* tidyverse 
* janitor
* lubridate
* colorBlindness
* patchwork
* ggplot2
* rstatix
* car

## Variables

| Variable| Definition|
|----|----|
| PID| Personal Identification - a unique identifier code assigned to each person that allows case tracking while preserving patient data confidentiality|
| reprt_creationdt_FALSE| Date of entry for case|
| case_dob_FALSE| Date of birth|
| case_age| Age of person|
| case_gender| Gender of person|
| case_race| U.S. census bureau social definition of race for person|
| case_eth| U.S. census bureau social definition of ethnicity for person|
| case_zip| zip code of person's known address|
| Contact_id| Symptomatic status of person|
| sym_startdt_FALSE| Date of first symptoms (if known)|
| sym_fever| Person has/had fever symptoms
| sym_myalgia| Person has/had myalgia symptoms
| sym_loststastesmell| Person has/had a loss of taste or smell
| sym_sorethroat| Person has/had a sore throat
| sym_headache| Person has/had a headache
| sym_resolveddt_FALSE| Person symptoms now resolved
| contact_household| Person has household contacts (someone who stays more than 5 nights per week)
| hospitalized| Person admitted to hospital
| hosp_admidt_FALSE| Date of hospital admission
| hosp_dischdt_FALSE| Date of hospital discharge
| died| Person died Y/N
| died_covid| Covid on death certificate Y/N
| died_dt_FALSE| Date of death
| confirmed_case| Confirmed case of COVID-19
| pos_sampledt_FALSE| Date of positive PCR test