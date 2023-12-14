# LOADING CLEAND DATA AND FUNCTIONS----


# importing cleaned data script
source("scripts/01_selenkovas_figure_script.R")


#__________________________----

age_summary <- new_covid$age %>% mutate(mean_age = mean(age))

new_covid %>% ggplot(
  aes(x = new_age_years,
      y = duration_hosp_days)) +
  geom_point() +
  geom_smooth() + 
  coord_flip() +
  geom_boxplot(aes(y = new_age_years, fill = hospitalized_status), 
               alpha = 0.2)


new_covid %>% ggplot(
  aes(x = new_age_years,
      y = duration_hosp_days)) +
  geom_point() +
  geom_smooth() + 
  ylim(0,14) +
  geom_boxplot(aes(y = new_age_years, fill = hospitalized_status), 
               alpha = 0.2)



new_covid %>% 
  rstatix::cor_test(new_age_years, duration_hosp_days)
# cor coefficient is positive but low; this means the linear correlation is weak 

new_covid %>% select(new_age_years, 
                    duration_hosp_days) %>% 
  mutate(rank_age = dense_rank((new_age_years)), 
         rank_duration_hosp_days = dense_rank((duration_hosp_days))) %>% 
  head()

new_covid %>% 
  rstatix::cor_test(new_age_years, duration_hosp_days, method="spearman")