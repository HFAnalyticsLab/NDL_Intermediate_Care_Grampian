cohort_scales <- readRDS("data/cohort_scales.RDS")
source("setup.R")


# plot of quarterly admissions


# population #####

extract_pop <- readRDS("data/extract_pop.RDS") %>%
  filter(HEALTH_BOARD == "Grampian") %>%
  filter(between(YEAR, 2018, 2023)) %>%
  filter(AGE >= 50)

# overall pop for each year 

pop_by_year <- extract_pop %>%
  group_by(year=YEAR) %>%
  summarise(pop = sum(POPULATION))

# plot data
cohort_scales <- cohort_scales %>%
  mutate(type_of_hosp = case_when(hospital_desc == "Hospital at Home" ~ "Hospital at Home",
                          hospital_desc == "Rosewell House" ~ "Rosewell House",
                          .default = "Community Hospital")) 



plot_data <- cohort_scales %>%
   group_by(year_half = floor_date(hosp_start_final_dt, "halfyear"), step, type_of_hosp) %>%
   summarise(n = n()) %>%
   group_by(year_half) %>%
   mutate(prop = (n / sum(n))) %>%
   ungroup() %>%
   bind_rows(cohort_scales %>%
   group_by(year_half = floor_date(hosp_start_final_dt, "halfyear"), type_of_hosp) %>%
   summarise(n = n(), step = "Total")) %>%
   mutate(year=year(year_half)) %>%
   left_join(pop_by_year) %>%
   mutate(rate = (n / pop)*1000) %>%
   ungroup()
   

plot_data_temp <-  plot_data %>%
   filter(step == "Total") %>%
   group_by(year_half) %>%
   mutate(prop = n / sum(n)) %>%
   filter(type_of_hosp == "Rosewell House")
   

plot_data_temp <-  plot_data_temp %>%
   bind_rows(plot_data_temp %>%
               mutate(step = "Step down"))
   

plot_data <- plot_data %>%
  filter(type_of_hosp != "Rosewell House") %>%
  bind_rows(plot_data_temp)


check <- plot_data %>% 
  filter(step != "Total") %>%
  group_by(year_half) %>%
  mutate(check = sum(prop)) %>%
  ungroup()

stopifnot(check$check==1)    

time_ih_plot <- plot_data %>%
  filter(step == "Total") %>%
ggplot(aes(x=year_half, y=n)) +
   geom_path() +
   geom_point() +
   ylim(0,NA) +
   theme_intcare() +
   xlab("") +
   ylab("Number of admissions") +
   annotate_intcare() +
   facet_wrap(vars(type_of_hosp), ncol = 3)

time_ih_plot

#saveRDS(time_ih_plot, "presentations/quarter_ih.RDS")



time_step_type_plot <- plot_data %>%
  ggplot(aes(x = year_half, y = n, colour = step)) +
  geom_path(linewidth = 1.5) +
  ylim(0, NA) +
  theme_intcare() +
  theme(legend.position = "none") +
  xlab("") +
  ylab("Number of half yearly admissions") +
  scale_colour_viridis(discrete = TRUE) +
  #annotate_intcare() +
  facet_grid(vars(step), vars(type_of_hosp), scales = "free_y")

#saveRDS(time_step_type_plot, "presentations/time_step_type.RDS")


time_step_type_rate_plot <- plot_data %>%
  ggplot(aes(x = year_half, y = rate, colour = step)) +
  geom_path(linewidth = 1.5) +
  ylim(0, NA) +
  theme_intcare() +
  theme(legend.position = "none") +
  xlab("") +
  ylab("Rate per 1000 of half yearly admissions") +
  scale_colour_viridis(discrete = TRUE) +
  #annotate_intcare() +
  facet_grid(vars(step), vars(type_of_hosp), scales = "free_y")

#saveRDS(time_step_type_rate_plot, "presentations/time_step_type_rate.RDS")



time_step_type_prop_plot <- plot_data %>%
  filter(step != "Total") %>%
  ggplot(aes(x = year_half, y = prop, colour = step)) +
  geom_path(linewidth = 1.5) +
  ylim(0, NA) +
  theme_intcare() +
  xlab("") +
  ylab("Proportion of admissions") +
  scale_colour_viridis(discrete = TRUE) +
  #annotate_intcare()
  facet_grid(vars(step), vars(type_of_hosp), scales = "free_y")

# saveRDS(time_step_type_prop_plot, "presentations/time_step_type_prop.RDS")

# output

plot_data <- plot_data %>%
  mutate(year_half = as.character(year_half))

write_csv(plot_data, file = "for_release/21_03_2024/step_type_by_halfyear.csv")

# overall pop for each year 

pop_by_year <- extract_pop %>%
  group_by(year=YEAR, COUNCIL_AREA_2019) %>%
  summarise(pop = sum(POPULATION))

# plot data

cohort_scales <- cohort_scales %>%
   mutate(la = case_when(hospital_desc == "Hospital at Home" ~ "Aberdeen City",
                         hospital_desc == "Rosewell House" ~ "Aberdeen City",
                         hospital_desc == "Seafield Hospital" ~ "Moray",
                         hospital_desc == "Turner Memorial Hospital" ~ "Moray",
                         hospital_desc == "Fleming Hospital" ~ "Moray",
                         hospital_desc == "Stephen Cottage Hospital" ~ "Moray",
                         .default = "Aberdeenshire")) 

plot_data <- cohort_scales %>%
   group_by(year_half = floor_date(hosp_start_final_dt, "halfyear"), step, la) %>%
   summarise(n = n()) %>%
   group_by(year_half) %>%
   mutate(prop = (n / sum(n))) %>%
   bind_rows(cohort_scales %>%
   group_by(year_half = floor_date(hosp_start_final_dt, "halfyear"), la) %>%
   summarise(n = n(), step = "Total")) %>%
   mutate(year=year(year_half)) %>%
   left_join(pop_by_year, join_by(year, la == COUNCIL_AREA_2019)) %>%
   mutate(rate = (n / pop)*1000) %>%
   ungroup()
  



plot_data %>%
  filter(step == "Total") %>%
ggplot(aes(x=year_half, y=n)) +
   geom_path(linewidth = 1.5) +
   ylim(0,NA) +
   theme_intcare() +
   xlab("") +
   ylab("Number of admissions") +
   facet_wrap(vars(la), ncol = 3) +
   annotate_intcare()

plot_data %>%
  filter(step != "Total") %>%
ggplot(aes(x=year_half, y=n, colour = step)) +
   geom_path(linewidth = 1.5) +
   ylim(0,NA) +
   theme_intcare() +
   xlab("") +
   ylab("Number of admissions") +
   facet_wrap(vars(la), ncol = 3) +
   annotate_intcare() +
   scale_colour_viridis(discrete = TRUE)

time_step_la_plot <- plot_data %>%
  ggplot(aes(x = year_half, y = n, colour = step)) +
  geom_path(linewidth = 1.5) +
  ylim(0, NA) +
  theme_intcare() +
  theme(legend.position = "none") +
  xlab("") +
  ylab("Number of half yearly admissions") +
  scale_colour_viridis(discrete = TRUE) +
  #annotate_intcare() +
  facet_grid(vars(step), vars(la), scales = "free_y")


# saveRDS(time_step_la_plot, "presentations/time_step_la.RDS")


time_step_la_rate_plot <- plot_data %>%
  ggplot(aes(x = year_half, y = rate, colour = step)) +
  geom_path(linewidth = 1.5) +
  ylim(0, NA) +
  theme_intcare() +
  theme(legend.position = "none") +
  xlab("") +
  ylab("Rate per 1000 of half yearly admissions") +
  scale_colour_viridis(discrete = TRUE) +
  #annotate_intcare() +
  facet_grid(vars(step), vars(la), scales = "free_y")

# saveRDS(time_step_la_rate_plot, "presentations/time_step_la_rate.RDS")



time_step_la_prop_plot <- plot_data %>%
  filter(step != "Total") %>%
  ggplot(aes(x = year_half, y = prop, colour = step)) +
  geom_path(linewidth = 1.5) +
  ylim(0, NA) +
  theme_intcare() +
  theme(legend.position = "none") +
  xlab("") +
  ylab("Proportion of admissions") +
  scale_colour_viridis(discrete = TRUE) +
  #annotate_intcare()
  facet_grid(vars(step), vars(la), scales = "free_y")

# saveRDS(time_step_la_prop_plot, "presentations/time_step_la_prop.RDS")

# output

plot_data <- plot_data %>%
  mutate(year_half = as.character(year_half))

write_csv(plot_data, file = "for_release/21_03_2024/step_la_by_halfyear.csv")