# LOADING CLEAND DATA AND FUNCTIONS----
# Is there a relationship between duration of stay and age?

# importing cleaned data script
source("scripts/01_selenkovas_figure_script.R")


#__________________________----


# FINDING STATISTICS----

stats_covid <- tibble(median_age = median(.data = new_covid, new_covid$new_age_years))

under_median_age <- tibble(filter(select(new_covid, new_age_years, duration_hosp_days), new_covid$new_age_years < stats_covid$median_age))

over_median_age <- tibble(filter(select(new_covid, new_age_years, duration_hosp_days), new_covid$new_age_years > stats_covid$median_age))

stats_covid <- mutate(stats_covid, 
                      average_stay_under = mean(under_median_age$duration_hosp_days), 
                      average_stay_over = mean((over_median_age$duration_hosp_days)))

stay_per_age <- new_covid %>% ggplot(aes(x = new_age_years, 
                                         y = duration_hosp_days)) + 
  geom_point() + 
  geom_smooth() +
  theme_minimal()

patients_per_age <- new_covid %>% ggplot(aes(x = new_age_years)) +
  geom_histogram(binwidth = 1) +
  geom_density(aes(y = after_stat(count), 
                   fill = "#882255")) + 
  geom_vline(data = stats_covid,
             aes(xintercept = median_age), 
             colour="darkorange",
             linetype = "longdash") + 
  theme_minimal()
  

hospitalisation_figure <- 
  stay_per_age + patients_per_age + plot_layout(guides = "collect", ncol = 1)

