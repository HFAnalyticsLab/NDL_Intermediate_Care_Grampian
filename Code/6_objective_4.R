source("setup.R")

cohort_scales <- readRDS("data/cohort_scales.RDS")


# Use 2018-2023 grampian mid year population as standard 

extract_pop <- readRDS("data/extract_pop.RDS") %>%
  filter(HEALTH_BOARD == "Grampian") %>%
  filter(between(YEAR, 2018, 2023)) %>%
  filter(AGE >= 50) %>%
  clean_names()

year_pop <- extract_pop %>%
  group_by(sex = gender, age = age_band_5year, year) %>%
  summarise(pop=sum(population)) %>%
  ungroup()

#check overall pop makes sense

year_pop %>%
  group_by(year) %>%
  summarise(sum(pop))

#### because we have two halve years going to average pop for two years

years <- year_pop %>%
  distinct(start=year) %>%
  mutate(end=lead(start)) %>%
  slice_head(n=5)

year_pop <-  map2(years$start, years$end, ~year_pop %>%
  filter(between(year, .x, .y)) %>%
  group_by(sex, age) %>%
  summarise(pop = sum(pop)/2, year=glue("{.x}_{.y}"))) %>%
  list_rbind()
   

std_pop <- year_pop %>%
  group_by(sex, age) %>%
  summarise(pop_std = sum(pop))



# get 5 year age bands and add to data

age_cats <- extract_pop %>%
  select(age, age_band_5year) %>%
  distinct()

range(cohort_scales$age_on_admission)
max_age <- max(cohort_scales$age_on_admission)

age_cats <- age_cats %>%
  add_row(age = 91:max_age, age_band_5year = "90+")


cohort_scales <- cohort_scales %>%
  left_join(age_cats, join_by(age_on_admission == age))

# check 

cohort_scales %>%
  group_by(age_band_5year) %>%
  summarise(min(age_on_admission), max(age_on_admission))

# add std pop

cohort_scales <- cohort_scales %>%
  left_join(std_pop, join_by(age_band_5year == age, sex))


# plot data
cohort_scales <- cohort_scales %>%
  mutate(type_of_hosp = case_when(hospital_desc == "Hospital at Home" ~ "Hospital at Home",
                          hospital_desc == "Rosewell House" ~ "Rosewell House",
                          .default = "Community Hospital")) 
plot_data <- cohort_scales %>%
   group_by(year_quarter = floor_date(hosp_start_final_dt, "quarter"), type_of_hosp) %>%
   summarise(elixhauser_score = median(elixhauser_score),
             polypharmacy_score = median(n_bnf_sub_chapters),
             out_patients_score = median(n_specialty),
             n = n()) %>%
   bind_rows(cohort_scales %>%
   group_by(year_quarter = floor_date(hosp_start_final_dt, "quarter")) %>%
   summarise(elixhauser_score = median(elixhauser_score), 
             polypharmacy_score = median(n_bnf_sub_chapters),
             out_patients_score = median(n_specialty),
             type_of_hosp = "Total",
             n = n())) %>%
   ungroup()
  



  

time_poly_type <- plot_data %>%
ggplot(aes(x=year_quarter, y=polypharmacy_score, colour = type_of_hosp)) +
   geom_path(linewidth = 1.5 ) +
   theme_intcare() +
   xlab("") +
   ylab("Median number of different BNF sub chapters)") +
   facet_wrap(vars(type_of_hosp)) 
   #annotate_intcare()

#saveRDS(time_poly_type, "presentations/time_poly_type.RDS")


time_out_type <- plot_data %>%
ggplot(aes(x=year_quarter, y=out_patients_score, colour = type_of_hosp)) +
   geom_path(linewidth = 1.5) +
   theme_intcare() +
   xlab("") +
   ylab("Median number of different specialties seen") +
   #annotate_intcare() +
   facet_wrap(vars(type_of_hosp))

#saveRDS(time_out_type, "presentations/time_out_type.RDS")


time_multi_type <- plot_data %>%
ggplot(aes(x=year_quarter, y=elixhauser_score, colour = type_of_hosp)) +
   geom_path(linewidth = 1.5) +
   theme_intcare() +
   xlab("") +
   ylab("Median Elixhauser Score") +
   annotate_intcare() +
   facet_wrap(vars(type_of_hosp))

#saveRDS(time_multi_type, "presentations/time_multi_type.RDS")

# output

plot_data <- plot_data %>%
  mutate(year_quarter = as.character(year_quarter))

write_csv(plot_data, file = "for_release/21_03_2024/health_type_by_quarter.csv")
