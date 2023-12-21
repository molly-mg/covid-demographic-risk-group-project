# LOAD R OBJECTS AND FUNCTIONS ----
source("scripts/01_selenkovas_tidy_data.R")
# importing tidied data and functions
#__________________________----

minimum_age_death <- new_covid %>% 
  filter(died_of_covid == "Yes")

covid_colours <- c("#117733", "#882255")

new_covid %>% ggplot(
  aes(x = new_age_years,
      y = duration_hosp_days)) + 
  geom_vline(xintercept = min(minimum_age_death$new_age_years),
             linetype = 2,
             color = "darkorange") + 
  geom_point(aes(fill = died_of_covid),
             shape = 21,
             colour = "black",
             size = 1.75,
             stroke = 0.3,
             alpha = 0.6,
             position = position_jitter(w = 0, h = 0.2),
             show.legend = FALSE) +
  geom_smooth(aes(colour = died_of_covid), 
              method = "gam", 
              se = FALSE,
              linewidth = 3) + 
  scale_colour_manual(values = covid_colours, 
                      aesthetics = c("colour", "fill")) + 
  geom_smooth(method = "lm", 
              se = FALSE, 
              colour = "#332288",
              linewidth = 1.5,
              lty = "dashed") + 
  geom_label(aes(x = 10, y = 20,
                label = "There were no deaths \n of hospitalised children under 17"),
            size = 2.5,
            colour = "black",
            fill = alpha(c("white"), 0.5)) + 
  guides(colour = guide_legend(title = "Did the patient die?")) + 
  labs(x = "Age",
       y = "Duration of stay (days)",
       title = "Duration of hospitalisation of patients with COVID-19",
       subtitle = "Effects of patient's age on the duration and outcomes of their hospitalisation") +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black", 
                                 size = 0.5, linetype = "solid"),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12),
        legend.position = c(0.9, 0.9),
        legend.background = element_rect(colour = "black")) + 
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 120, by = 10)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 140, by = 14))

# older people were hospitalised for longer than youger people
# hospitalised older people, that died of covid, died sooner than younger people
# no hospitalised people under 20(???), including children, died
# most children hospitalised were under 5