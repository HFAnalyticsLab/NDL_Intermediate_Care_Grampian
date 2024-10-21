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


# age and sex rate

cohort_scales <- cohort_scales %>%
  mutate(year = case_when(between(hosp_start_final_date, ymd(start), ymd(end)) ~ "1",
                          between(hosp_start_final_date, ymd(start), ymd(end)) ~ "2",
                          between(hosp_start_final_date, ymd(start), ymd(end)) ~ "3",
                          between(hosp_start_final_date, ymd(start), ymd(end)) ~ "4",
                          between(hosp_start_final_date, ymd(start), ymd(end)) ~ "5"))

age_sex_rates <-  cohort_scales %>%
  group_by(age_band_5year, year, sex) %>%
  summarise(n = n()) %>%
  ungroup()

age_sex_rates <- age_sex_rates %>%
  left_join(year_pop, join_by(age_band_5year == age, year, sex)) %>%
  mutate(rate = (n / pop)*100000)

age_sex_rates %>%
  ggplot(aes(x = age_band_5year, y = rate, group = year, colour = year)) +
  geom_path() +
  facet_wrap(vars(sex))


# age

age_rates <- cohort_scales %>%
  group_by(age_band_5year, year) %>%
  summarise(n = n()) %>%
  ungroup()

year_pop_age <- year_pop %>%
  group_by(age, year) %>%
  summarise(pop = sum(pop)) %>%
  ungroup()

age_rates <- age_rates %>%
  left_join(year_pop_age, join_by(age_band_5year == age, year)) %>%
  mutate(rate = (n / pop)*1000)


# sex

sex_rates <- cohort_scales %>%
  group_by(sex, year) %>%
  summarise(n = n()) %>%
  ungroup()

year_pop_sex <- year_pop %>%
  group_by(sex, year) %>%
  summarise(pop = sum(pop)) %>%
  ungroup()

sex_rates <- sex_rates %>%
  left_join(year_pop_sex, join_by(sex, year)) %>%
  mutate(rate = (n / pop)*1000)




# standardised annual rates std by age sex 

age_sex_rates <- age_sex_rates %>%
  left_join(std_pop, join_by(age_band_5year == age, sex))


year_dsr <- age_sex_rates %>%
  group_by(year) %>%
  phe_dsr(x = n,
          n = pop,
          stdpop = pop_std,
          stdpoptype = "field",
          multiplier = 1000)



  
# manual check

age_sex_rates %>%
  group_by(year) %>%
  summarise(sum(rate*(pop_std/sum(pop_std))))

# by type of hospital

age_sex_type_rates <-  cohort_scales %>%
   mutate(type_of_hosp = case_when(hospital_desc == "Hospital at Home" ~ "Hospital at Home",
                        hospital_desc == "Rosewell House" ~ "Rosewell House",
                      .default = "Community Hospital"))   %>%
  group_by(age_band_5year, year, sex, type_of_hosp) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  complete(age_band_5year, year, sex, type_of_hosp,
           fill = list(n = 0))

age_sex_type_rates <- age_sex_type_rates %>%
  left_join(year_pop, join_by(age_band_5year == age, year, sex))

age_sex_type_rates <- age_sex_type_rates %>%
  left_join(std_pop, join_by(age_band_5year == age, sex))


year_type_dsr <- age_sex_type_rates %>%
  group_by(year, type_of_hosp) %>%
  phe_dsr(x = n,
          n = pop,
          stdpop = pop_std,
          stdpoptype = "field",
          multiplier = 1000)

# plots

# dsr

year_type_dsr <- year_type_dsr %>%
  bind_rows(year_dsr %>%
              mutate(type_of_hosp = "Total"))

year_dsr_plot <- year_type_dsr %>%
  ggplot(aes(y = year, x = value, xmin = lowercl, xmax = uppercl, colour = year)) +
  geom_point() +
  geom_pointrange() +
  xlab("Admission rate per 1000") +
  ylab("") +
  theme_intcare() +
  theme(legend.position = "none") +
  facet_wrap(vars(type_of_hosp))

#saveRDS(year_dsr_plot, "presentations/year_dsr_plot.RDS")

# output

write_csv(year_type_dsr, file = "for_release/21_03_2024/year_type_dsr.csv")

# sex

sex_rates_plot <- sex_rates %>%
  ggplot(aes(x = rate, y = year, colour= sex)) +
  geom_point(size = 3) +
  #facet_wrap(vars(year)) +
  theme_intcare() +
  #theme(legend.position = "none") +
  scale_colour_viridis(discrete = TRUE) +
  ylab(" ") +
  xlab("Rate per 1000")

#saveRDS(sex_rates_plot, "presentations/sex_rates_plot.RDS")

write_csv(sex_rates, file = "for_release/21_03_2024/year_sex_rates.csv")



# age

age_rates_plot <- age_rates %>%
  ggplot(aes(x = rate, y = year, colour= age_band_5year)) +
  geom_point(size = 3) +
  #facet_wrap(vars(year)) +
  theme_intcare() +
  #theme(legend.position = "none") +
  scale_colour_viridis(discrete = TRUE) +
  ylab(" ") +
  xlab("Rate per 1000")

#saveRDS(age_rates_plot, "presentations/age_rates_plot.RDS")


write_csv(age_rates, file = "for_release/21_03_2024/year_age_rates.csv")

#Deprivation

#download.file("https://www.nrscotland.gov.uk/files//statistics/population-estimates/special-area-2011-dz/simd/simd-21-tab3.xlsx", 
#              "dep_file.xlsx", mode = "wb")

# 2021 latest year

years_dep <-  c("2018", "2019", "2020", "2021")

pop_dep <- map(years_dep, ~ read_excel("dep_file.xlsx", sheet = .x, skip = 3) %>%
  clean_names() %>%
  filter(health_board_area == "Grampian") %>%
  filter(sex != "Persons") %>%
  select(-(age_0:age_49))) %>%
  set_names(years_dep) %>%
  list_rbind(names_to = "year") %>%
  mutate(across(c(simd_decile, year), ~as.numeric(.x))) %>%
  mutate(simd_5 = case_when(between(simd_decile, 1,2) ~ 1,
                            between(simd_decile, 3,4) ~ 2,
                            between(simd_decile, 5,6) ~ 3,
                            between(simd_decile, 7,8) ~ 4,
                            between(simd_decile, 9,10) ~ 5)) %>%
  pivot_longer(cols = age_50:age_90_and_over, 
               names_to = "age",
               names_prefix = "age_",
               values_to = "pop") %>%
  mutate(age = ifelse(age == "90_and_over", "90", age)) %>%
  mutate(age = as.numeric(age)) %>%
  mutate(sex = case_when(sex == "Females" ~ "Female",
                         sex == "Males" ~ "Male"))


pop_dep_2022 <- pop_dep %>%
  filter(year == 2021) %>%
  mutate(year = 2022)

pop_dep_2023 <- pop_dep %>%
  filter(year == 2021) %>%
  mutate(year = 2023)                        


pop_dep <- bind_rows(pop_dep, pop_dep_2022, pop_dep_2023)


pop_dep <- pop_dep %>% 
  left_join(age_cats, by = join_by(age)) %>%
  group_by(year, age_band_5year, sex, simd_5) %>%
  summarise(pop = sum(pop))

pop_dep <-  map2(years$start, years$end, ~pop_dep %>%
  filter(between(year, .x, .y)) %>%
  group_by(age_band_5year, sex, simd_5) %>%
  summarise(pop = sum(pop)/2, year=glue("{.x}_{.y}"))) %>%
  list_rbind() %>%
  ungroup()

# age_sex_simd rate

 cohort_scales <- cohort_scales %>%
 mutate(simd_5 = national_simd_2020_quintile_at_adm)
  
 age_sex_simd_rates <- cohort_scales %>% 
  filter(!is.na(national_simd_2020_quintile_at_adm)) %>%
  group_by(age_band_5year, year, sex, simd_5) %>%
  summarise(n = n()) %>%
  ungroup()


# standardised annual rates std by age sex 

age_sex_simd_rates <- age_sex_simd_rates %>%
  complete(age_band_5year, year, sex, simd_5) %>%
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  left_join(std_pop, join_by(age_band_5year == age, sex))


age_sex_simd_rates <- age_sex_simd_rates %>%
  left_join(pop_dep, join_by(age_band_5year, year, sex, simd_5))


year_simd_dsr <- age_sex_simd_rates %>%
  group_by(year, simd_5) %>%
  phe_dsr(x = n,
          n = pop,
          stdpop = pop_std,
          stdpoptype = "field")


simd_dsr <- age_sex_simd_rates %>%
  group_by(simd_5) %>%
  phe_dsr(x = n,
          n = pop,
          stdpop = pop_std,
          stdpoptype = "field")

# rates
simd_rates <- cohort_scales %>% 
  filter(!is.na(national_simd_2020_quintile_at_adm)) %>%
  group_by(year, simd_5) %>%
  summarise(n = n()) %>%
  ungroup()

year_pop_dep <- pop_dep %>%
  group_by(simd_5, year) %>%
  summarise(pop = sum(pop)) %>%
  ungroup()

simd_rates <- simd_rates %>%
  left_join(year_pop_dep, join_by(simd_5, year)) %>%
  mutate(rate = (n / pop)*1000) %>%
  mutate(simd_5 = rev(simd_5))

simd_rates_plot <- simd_rates %>%
  ggplot(aes(x = rate, y = year)) +
  geom_text(aes(label = simd_5)) +
  #facet_wrap(vars(year)) +
  theme_intcare() +
  theme(legend.position = "none") +
  scale_colour_viridis(discrete = TRUE) +
  ylab(" ") +
  xlab("Rate per 1000")

#saveRDS(simd_rates_plot, "presentations/simd_rates_plot.RDS")

# output

write_csv(simd_rates, file = "for_release/21_03_2024/year_simd_rates.csv")


# rural

# national not by health board

# download.file("https://www.nrscotland.gov.uk/files//statistics/population-estimates/special-area-2011-dz/urban-rural/urban-rural-21-tab-1.# xlsx", "rural_file.xlsx", mode = "wb")

# 2021 latest year

years_rural <-  c("2018", "2019", "2020", "2021")

pop_rural <- map(years_dep, ~ read_excel("rural_file.xlsx", sheet = .x, skip = 3) %>%
  clean_names() %>%
  filter(sex != "Persons") %>%
  select(-(age_0:age_49))) %>%
  set_names(years_rural) %>%
  list_rbind(names_to = "year") %>%
  mutate(year = as.numeric(year)) %>%
  pivot_longer(cols = age_50:age_90_and_over, 
               names_to = "age",
               names_prefix = "age_",
               values_to = "pop") %>%
  mutate(age = ifelse(age == "90_and_over", "90", age)) %>%
  mutate(age = as.numeric(age)) %>%
  mutate(sex = case_when(sex == "Females" ~ "Female",
                         sex == "Males" ~ "Male"))


pop_rural_2022 <- pop_rural %>%
  filter(year == 2021) %>%
  mutate(year = 2022)

pop_rural_2023 <- pop_rural %>%
  filter(year == 2021) %>%
  mutate(year = 2023)                        


pop_rural <- bind_rows(pop_rural, pop_rural_2022, pop_rural_2023)


pop_rural <- pop_rural %>% 
  left_join(age_cats, join_by(age)) %>%
  group_by(year, age_band_5year, sex, urban_rural_class_name) %>%
  summarise(pop = sum(pop)) %>%
  ungroup()

pop_rural <-  map2(years$start, years$end, ~pop_rural %>%
  filter(between(year, .x, .y)) %>%
  group_by(age_band_5year, sex, urban_rural_class_name) %>%
  summarise(pop = sum(pop)/2, year=glue("{.x}_{.y}"))) %>%
  list_rbind() %>%
  ungroup()

# age_sex_rural rate

cohort_scales <- cohort_scales %>%
  mutate(urban_rural_class_name = case_when(urban_rural6fold2020 == 1 ~ "Accessible Rural Areas",
                                            urban_rural6fold2020 == 2 ~ "Accessible Small Towns",
                                            urban_rural6fold2020 == 3 ~ "Large Urban Areas",
                                            urban_rural6fold2020 == 4 ~ "Other Urban Areas", 
                                            urban_rural6fold2020 == 5 ~ "Remote Rural Areas",
                                            urban_rural6fold2020 == 6 ~ "Remote Small Towns"))

age_sex_rural_rates <- cohort_scales %>%
  filter(!is.na(urban_rural6fold2020)) %>%
  group_by(age_band_5year, year, sex, urban_rural_class_name) %>%
  summarise(n = n()) %>%
  ungroup() 



# standardised annual rates std by age sex 

age_sex_rural_rates <- age_sex_rural_rates %>%
  complete(age_band_5year, year, sex, urban_rural_class_name) %>%
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  left_join(std_pop, join_by(age_band_5year == age, sex))


age_sex_rural_rates <- age_sex_rural_rates %>%
  left_join(pop_rural, join_by(age_band_5year, year, sex, urban_rural_class_name))


year_rural_dsr <- age_sex_rural_rates %>%
  group_by(year, urban_rural_class_name) %>%
  phe_dsr(x = n,
          n = pop,
          stdpop = pop_std,
          stdpoptype = "field")


rural_dsr <- age_sex_rural_rates %>%
  group_by(urban_rural_class_name) %>%
  phe_dsr(x = n,
          n = pop,
          stdpop = pop_std,
          stdpoptype = "field")



# admission rates by rural and age

age_sex_rural_rates %>%
  group_by(age_band_5year, urban_rural_class_name) %>%
  summarise(pop = sum(pop), n = sum(n), rate = (n /pop) *100000) %>%
  ungroup() %>%
  ggplot(aes(x = age_band_5year, y = rate,  colour = factor(urban_rural_class_name), group = urban_rural_class_name)) +
  geom_path() 

# rates , as national pop estimates going to proportion gramps pop by 
# factors given at https://www.gov.scot/binaries/content/documents/govscot/publications/advice-and-guidance/2022/05/scottish-government-urban-rural-classification-2020/documents/scottish-government-urban-rural-classification-2020-mid-year-2020-population-estimates-tables/scottish-government-urban-rural-classification-2020-mid-year-2020-population-estimates-tables/govscot%3Adocument/scottish-government-urban-rural-classification-2020-mid-year-2020-population-estimates-tables.xlsx

rural_rates <- cohort_scales %>% 
  filter(!is.na(urban_rural_class_name)) %>%
  group_by(year, urban_rural_class_name) %>%
  summarise(n = n()) %>%
  ungroup()

year_pop_rural <- year_pop %>%
  group_by(year) %>%
  summarise(pop = sum(pop)) %>%
  ungroup()

rural_rates <- rural_rates %>%
  left_join(year_pop_rural) %>%
  mutate(pop = case_when(urban_rural_class_name == "Large Urban Areas" ~ pop*0.377,	
                   urban_rural_class_name == "Other Urban Areas" ~ pop*0.179,	
                   urban_rural_class_name == "Accessible Small Towns" ~ pop*0.116,	
                   urban_rural_class_name == "Remote Small Towns" ~ pop*0.037,
                   urban_rural_class_name == "Accessible Rural Areas" ~ pop*0.221,
                   urban_rural_class_name == "Remote Rural Areas" ~pop*0.07)) %>%
  mutate(rate = (n / pop)*1000)

rural_rates_plot <- rural_rates %>%
  ggplot(aes(x = rate, y = year, colour= factor(urban_rural_class_name))) +
  geom_point(size = 3) +
  #facet_wrap(vars(year)) +
  theme_intcare() +
  #theme(legend.position = "none") +
  scale_colour_viridis(discrete = TRUE) +
  ylab(" ") +
  xlab("Rate per 1000")

# saveRDS(rural_rates_plot, "presentations/rural_rates_plot.RDS")

# output

write_csv(rural_rates, file = "for_release/21_03_2024/year_rural_rates.csv")