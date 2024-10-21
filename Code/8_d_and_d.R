source("setup.R")
library(ggsurvfit)
library(patchwork)

cohort_scales <- readRDS("data/cohort_scales.RDS")

# median age by quarter

plot_data <- cohort_scales %>%
  group_by(year_quarter = floor_date(hosp_start_final_dt, "quarter"), step) %>%
  summarise(median_age = round(median(age_on_admission), 0), 
            prop_80_and_over_n = sum(age_on_admission > 79),
            prop_80_and_over = mean(age_on_admission > 79)) %>%
  bind_rows(cohort_scales %>%
      group_by(year_quarter = floor_date(hosp_start_final_dt, "quarter")) %>%
            summarise(median_age = round(median(age_on_admission), 0), 
                        prop_80_and_over_n = sum(age_on_admission > 79),
                        prop_80_and_over = mean(age_on_admission > 79),
                        step = "All"))

plot_data <- plot_data %>%
  mutate(year_quarter = as.character(year_quarter))
  
write_csv(plot_data, file = "for_release/21_03_2024/age_by_step_quarter.csv")


# los

plot_data <- cohort_scales %>%
  group_by(year_quarter = floor_date(hosp_start_final_dt, "quarter"), step) %>%
  summarise(median_los = round(median(hosp_los_days), 0), 
            prop_42days_and_over_n = sum(hosp_los_days > 41),
            prop_42days_and_over = mean(hosp_los_days > 41)) %>%
  filter(step != "Routine elective (step up)")

plot_data <- plot_data %>%
  mutate(year_quarter = as.character(year_quarter))

write_csv(plot_data, file = "for_release/21_03_2024/los_by_step_quarter.csv")


plot_data <- cohort_scales %>%
  mutate(type_of_hosp = case_when(hospital_desc == "Hospital at Home" ~ "Hospital at Home",
                                  hospital_desc == "Rosewell House" ~ "Rosewell House",
                                  .default = "Community Hospital")) %>% 
  filter(step != "Routine elective (step up)") %>%
  mutate(excl = ifelse(step == "Step up" & type_of_hosp == "Rosewell House", 1, 0)) %>%
  filter(excl == 0) %>%
  group_by(year_half = floor_date(hosp_start_final_dt, "half"), step, type_of_hosp) %>%
  summarise(median_los = round(median(hosp_los_days), 0),
            n()) %>%
  bind_rows(cohort_scales %>%
              mutate(type_of_hosp = case_when(hospital_desc == "Hospital at Home" ~ "Hospital at Home",
                                            hospital_desc == "Rosewell House" ~ "Rosewell House",
                                            .default = "Community Hospital")) %>% 
              filter(step != "Routine elective (step up)") %>%
              mutate(excl = ifelse(step == "Step up" & type_of_hosp == "Rosewell House", 1, 0)) %>%
              filter(excl == 0) %>%
              group_by(year_half = floor_date(hosp_start_final_dt, "half"), type_of_hosp) %>%
              summarise(median_los = round(median(hosp_los_days), 0),
                        n(),
              step = "Total")) %>%
  bind_rows(cohort_scales %>%
              mutate(type_of_hosp = case_when(hospital_desc == "Hospital at Home" ~ "Hospital at Home",
                                              hospital_desc == "Rosewell House" ~ "Rosewell House",
                                              .default = "Community Hospital")) %>% 
              filter(step != "Routine elective (step up)") %>%
              mutate(excl = ifelse(step == "Step up" & type_of_hosp == "Rosewell House", 1, 0)) %>%
              filter(excl == 0) %>%
              group_by(year_half = floor_date(hosp_start_final_dt, "half"), step) %>%
              summarise(median_los = round(median(hosp_los_days), 0),
                        n(), type_of_hosp = "Total")) %>%
  bind_rows(cohort_scales %>%
              mutate(type_of_hosp = case_when(hospital_desc == "Hospital at Home" ~ "Hospital at Home",
                                              hospital_desc == "Rosewell House" ~ "Rosewell House",
                                              .default = "Community Hospital")) %>% 
              filter(step != "Routine elective (step up)") %>%
              mutate(excl = ifelse(step == "Step up" & type_of_hosp == "Rosewell House", 1, 0)) %>%
              filter(excl == 0) %>%
              group_by(year_half = floor_date(hosp_start_final_dt, "half")) %>%
              summarise(median_los = round(median(hosp_los_days), 0),
                        n(), step = "Total", type_of_hosp = "Total"))

plot_data <- plot_data %>%
  mutate(year_half = as.character(year_half))

 
write_csv(plot_data, file = "for_release/28_03_2024/los_by_step_type_half.csv")










# specialty

plot_data <- cohort_scales %>%
  group_by(year_quarter = floor_date(hosp_start_final_dt, "quarter")) %>%
  summarise(specialty_no = 
            sum(admission_nat_spec_desc =="GP Other than Obstetrics"), 
            specialty_prop = 
            mean(admission_nat_spec_desc =="GP Other than Obstetrics"),
            specialty = "GP Other than Obstetrics") %>%
  bind_rows(cohort_scales %>%
      group_by(year_quarter = floor_date(hosp_start_final_dt, "quarter")) %>%
      summarise(specialty_no = 
      sum(admission_nat_spec_desc =="Geriatric Medicine"), 
       specialty_prop = 
      mean(admission_nat_spec_desc =="Geriatric Medicine"),
      specialty = "Geriatric Medicine"))

plot_data <- plot_data %>%
  mutate(year_quarter = as.character(year_quarter))

write_csv(plot_data, file = "for_release/21_03_2024/specialty_by_quarter.csv")

# dementia_and_delirium


plot_data <- cohort_scales %>%
  filter(step != "Routine elective (step up)") %>%
  group_by(year_quarter = floor_date(hosp_start_final_dt, "quarter"), step) %>%
  summarise(dementia_and_delirium_prop = mean(dementia_and_delirium), 
            dementia_and_delirium_n = sum(dementia_and_delirium)) %>%
  ungroup()

plot_data <- plot_data %>%
  mutate(year_quarter = as.character(year_quarter))

write_csv(plot_data, file = "for_release/21_03_2024/d_and_d_by_step_quarter.csv")

# time to readmission 

# survival curve 

# now  multi cause 

# is death after last readmssion 
# then death to 0 

cohort_scales <- cohort_scales %>%
  mutate(died_2 = 
           ifelse(died == 1 & deceased_date2 > ymd("2023-06-30"), 0, died))

cohort_scales %>%
  count(died_2, readmitted)

# 
cohort_scales <- cohort_scales %>%
  mutate(readmission_time_2 = 
           case_when(died_2 == 0 & readmitted == 0 ~ readmission_time,
                     died_2 == 0 & readmitted == 1 ~ readmission_time,
                     died_2 == 1 & readmitted == 0 ~ time_to_death,
                     died_2 ==1 & readmitted == 1 ~ pmin(readmission_time, time_to_death))) %>%
  mutate(readmission_days_2 = time_length(readmission_time_2, unit = "days")) %>%
  mutate(readmission_days_2 = 
           ifelse(readmission_days_2 < 0, 0, readmission_days_2))

cohort_scales <- cohort_scales %>%
  mutate(first_event = case_when(died_2 == 0 & readmitted == 0 ~ 0,
                                 died_2 == 1 & readmitted == 0 ~ 1,
                                 died_2 == 0 & readmitted == 1 &
                                   resource == "emergency" ~ 2, 
                                 died_2 == 0 & readmitted == 1 &
                                   resource == "inpatient" ~ 3,
                                 died_2 == 1 & readmitted == 1 & readmission_time <= time_to_death &
                                   resource == "emergency" ~ 2, 
                                 died_2 == 1 & readmitted == 1 & readmission_time <= time_to_death &
                                   resource == "inpatient" ~ 3,
                                 died_2 == 1 & readmitted == 1 & readmission_time > time_to_death ~ 1
  ))

d_cohort_scales <- cohort_scales %>%
  distinct(da_sh611_paadm_admno_uid, step, .keep_all = TRUE)

death <- survfit2(Surv(readmission_days_2, factor(first_event)) ~ step, data = d_cohort_scales) %>%
  ggcuminc(outcome = c("1"))  +
  scale_ggsurvfit(x_scales = list(breaks = c(0, 30, 60, 90), limits = c(NA, 90))) +
  ggtitle("Death") +
  xlab("Days")

emer <- survfit2(Surv(readmission_days_2, factor(first_event)) ~ step, data = d_cohort_scales) %>%
  ggcuminc(outcome = c("2"))  +
  scale_ggsurvfit(x_scales = list(breaks = c(0, 30, 60, 90), limits = c(NA, 90))) +
  ggtitle("Emergency readmission") +
  xlab("Days")

inpat <- survfit2(Surv(readmission_days_2, factor(first_event)) ~ step, data = d_cohort_scales) %>%
  ggcuminc(outcome = c("3"))  +
  scale_ggsurvfit(x_scales = list(breaks = c(0, 30, 60, 90), limits = c(NA, 90))) +
  ggtitle("Inpatient readmission") +
  xlab("Days")

all_plot <- death + emer + inpat +
  plot_layout(ncol = 3, guides = "collect") &
  theme(legend.position = "bottom")

readmission_summary <- bind_rows("death" = death$data, "emergency" = emer$data,
                                 "inpatient" = inpat$data, .id = "source") %>%
  filter(time == 90) %>%
  select(strata, source, "cumulative_incidence" = estimate, 
         conf.low, conf.high,
        "cumulative_n" = cum.event)

write_csv(readmission_summary, file = "for_release/25_03_2024/readmission_by_step.csv")

