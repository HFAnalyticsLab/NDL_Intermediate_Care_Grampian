
source("setup.R")

cohort_scales <- readRDS("data/cohort_scales.RDS")



# plot of quarterly admissions

# population #####

extract_pop <- readRDS("data/extract_pop.RDS") %>%
  filter(HEALTH_BOARD == "Grampian") %>%
  filter(between(YEAR, 2018, 2023)) %>%
  filter(AGE >= 50)

# overall pop for each year

pop_by_year <- extract_pop %>%
  group_by(year = YEAR) %>%
  summarise(pop = sum(POPULATION))

# plot data

plot_data <- cohort_scales %>%
  group_by(year_quarter = floor_date(hosp_start_final_dt, "quarter"), step) %>%
  summarise(n = n()) %>%
  group_by(year_quarter) %>%
  mutate(prop = (n / sum(n))) %>%
  bind_rows(cohort_scales %>%
    group_by(year_quarter = floor_date(hosp_start_final_dt, "quarter")) %>%
    summarise(n = n(), step = "Total")) %>%
  mutate(year = year(year_quarter)) %>%
  left_join(pop_by_year) %>%
  mutate(rate = (n / pop) * 1000)

quarter_admin_plot <- plot_data %>%
  filter(step == "Total") %>%
  ggplot(aes(x = year_quarter, y = n)) +
  geom_path(linewidth = 1.5) +
  ylim(0, NA) +
  theme_intcare() +
  xlab("") +
  ylab("Number of quarterly admissions") +
  annotate_intcare()


#saveRDS(quarter_admin_plot, "presentations/quarter_ad.RDS")


quarterly_step_plot <- plot_data %>%
  #filter(step != "Total") %>%
  ggplot(aes(x = year_quarter, y = n, colour = step)) +
  geom_path(linewidth = 1.5) +
  ylim(0, NA) +
  theme_intcare() +
  theme(legend.position = "none") +
  xlab("") +
  ylab("Number of quarterly admissions") +
  scale_colour_viridis(discrete = TRUE) +
  #annotate_intcare() +
  facet_wrap(vars(step), scales = "free_y") 

#saveRDS(quarterly_step_plot, "presentations/quarterly_step.RDS")


quarterly_step_plot_rate <- plot_data %>%
  #filter(step != "Total") %>%
  ggplot(aes(x = year_quarter, y = rate, colour = step)) +
  geom_path(linewidth = 1.5) +
  ylim(0, NA) +
  theme_intcare() +
  theme(legend.position = "none") +
  xlab("") +
  ylab("Rate per 1000 of quarterly admissions") +
  scale_colour_viridis(discrete = TRUE) +
  #annotate_intcare() +
  facet_wrap(vars(step), scales = "free_y") 

#saveRDS(quarterly_step_plot_rate, "presentations/quarterly_step_rate.RDS")



quarterly_step_prop <- plot_data %>%
  filter(step != "Total") %>%
  ggplot(aes(x = year_quarter, y = prop, colour = step)) +
  geom_path(linewidth = 1.5) +
  ylim(0, NA) +
  theme_intcare() +
  xlab("") +
  ylab("Proportion of admissions") +
  scale_colour_viridis(discrete = TRUE) 
  #annotate_intcare()
  
#saveRDS(quarterly_step_prop, "presentations/quarterly_step_prop.RDS")

# output

plot_data <- plot_data %>%
  mutate(year_quarter = as.character(year_quarter))

write_csv(plot_data, file = "for_release/21_03_2024/step_by_quarter.csv")