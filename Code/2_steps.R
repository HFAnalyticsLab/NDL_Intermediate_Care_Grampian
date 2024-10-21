#' ---
#' title: "Step up or down"
#' format: html
#' ---
#' 
#' ## Definitions
#' 
#' Our analysis plan does not specifically define step down or up but, from the related consultation, step down could be seen as intermediate beds being filled with longer term patients coming from other Grampian hospitals and these are limiting access to these community hospitals for new step up admissions. We have taken a sequential three stage process in trying to define the step of admissions to community hospitals (that by definition are all inpatient admissions).
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
#| label: setup
source("setup.R")


#' 
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
#| label: overview


#  chi rename for pharmacy, probably move this to 1_cohort.qmd

#cohort_phar <- readRDS("data/cohort_phar.RDS") %>%
#rename(chi = pat_chi_number)
#saveRDS(cohort_phar, "data/cohort_phar.RDS")


# check numbers of people

files_list <- list.files(path = "data", pattern = "cohort_*", full.names = TRUE)
files_names <- str_sub(files_list, 6, -5)


dfs_list <- map(files_list, ~ readRDS(.x))

names(dfs_list) <- files_names

# updates names of new dash ids


# admissions and transfers

# number of people

map(dfs_list, ~ .x %>% 
  clean_names() %>%   
  group_by(dash611_study_num) %>%
  n_groups()) %>%
  enframe() %>%
  unnest(cols = c(value))


# add some variables to hospital movements

cohort_all_hc <- dfs_list$cohort_all_hc %>%
  left_join(
    bind_rows(
      "inpatient" = select(dfs_list$cohort_adm,
        adm_date_time = adm_date_time,
        sex = gender, everything()
      ),
      "emergency" = select(dfs_list$cohort_emer,
        da_sh611_paadm_admno_uid,
        dash611_study_num, adm_date_time = admission_date_time, age_on_admission,
        sex = gender, attend_desc, discharge_condition_desc,
        departure_destination_desc
      ), .id = "source"
    ),
    join_by(dash611_study_num, da_sh611_paadm_admno_uid)
  )

#  variables to identify intermediate care hospitals and the 5 year period
# in age is needed as even though selected on age already obviously as going back in time 
# some people would have being younger 
# than 50 in the analysis period and had an admission to int hospital
cohort_all_hc <- cohort_all_hc %>%
  mutate(int_hosp = hospital_desc %in% ih_names) %>%
  mutate(analysis_period = between(hosp_start_final_date, ymd(start), ymd(end))) %>%
  mutate(in_age = age_on_admission >= 50) %>%
  group_by(da_sh611_paadm_admno_uid) %>%
  arrange(hosp_start_final_dt, hosp_end_final_dt, .by_group = TRUE) %>%
  mutate(int_hosp_adm = any(int_hosp)) %>%
  mutate(no_of_hosp = n()) %>%
  mutate(order_of_hosp = row_number()) %>%
  ungroup() %>%
  mutate(int_hosp_is_adm_hosp = hospital_desc == admission_hospital) %>%
  mutate(int_hosp_is_disch_hosp = hospital_desc == discharge_hospital)


# age distribution

cohort_all_hc %>%
  filter(int_hosp == TRUE & analysis_period == TRUE & in_age == TRUE) %>%
  ggplot(aes(x = age_on_admission, y = hospital_desc)) +
  geom_density_ridges()

# step up and down is it first hospital

admitted_hosp_nos <- cohort_all_hc %>%
  filter(int_hosp == TRUE & analysis_period == TRUE & in_age == TRUE) %>%
  count(int_hosp_is_adm_hosp) %>%
  mutate(total = sum(n), pc = (n / total) * 100)


# day cases
cohort_all_hc %>%
  filter(int_hosp == TRUE & analysis_period == TRUE & in_age == TRUE) %>%
  count(day_case_check)


# length of stay in that hosp and los in admission

cohort_all_hc <- cohort_all_hc %>%
  mutate(hosp_los_days = time_length(interval(hosp_start_final_dt, hosp_end_final_dt), unit = "days")) %>%
  group_by(da_sh611_paadm_admno_uid) %>%
  mutate(adm_los_days = sum(hosp_los_days)) %>%
  ungroup()

# the negative is odd

cohort_all_hc %>%
  filter(int_hosp == TRUE & analysis_period == TRUE & in_age == TRUE) %>%
  summarise(min(hosp_los_days), mean(hosp_los_days), max(hosp_los_days))

#' 
#' ### Stage 1
#' 
#' Stage 1 looks at within admission transfers between hospitals. Over a third of our cohort did not start their admission in the community hospital, in other words they had stepped down or across (across if they moved from one community hospital to another). Perhaps most clearly these are patients who may be occupying beds that could be used for new admissions. It may be that we stopped there with our definition as this is a clear split but the treatment of hospital at home as nearly always a new admission causes some problems as we suspect many may be step downs. We try to tackle this in stage 3
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
#| label: stage 1
# within an admission did they move hospital and what was the last hospital if so they are a step down or step across
# otherwise not sure and on to step 2

cohort_all_hc <- cohort_all_hc %>%
  group_by(da_sh611_paadm_admno_uid) %>%
  arrange(hosp_start_final_dt, hosp_end_final_dt, .by_group = TRUE) %>%
  mutate(prev_hosp = lag(hospital_desc)) %>%
  mutate(prev_hosp_int = lag(int_hosp)) %>%
  ungroup()

cohort_all_hc <- cohort_all_hc %>%
  mutate(step_1 = case_when(
    int_hosp == TRUE & analysis_period == TRUE & prev_hosp_int == TRUE & in_age == TRUE
    ~ "Step down (across within adm)",
    int_hosp == TRUE & analysis_period == TRUE & prev_hosp_int == FALSE & in_age == TRUE
    ~ "Step down (within adm)",
    int_hosp == TRUE & analysis_period == TRUE & order_of_hosp == 1 & in_age == TRUE
    ~ "first"
  ))

admitted_hosp_nos <- cohort_all_hc %>%
  filter(int_hosp == TRUE & analysis_period == TRUE & in_age == TRUE) %>%
  count(int_hosp_is_adm_hosp, step_1) %>%
  mutate(total = sum(n), pc = (n / total) * 100)




#  check if any step across and to same hospital

cohort_all_hc %>%
  filter(step_1 != "first" | !is.na(step_1)) %>%
  filter(hospital_desc == prev_hosp) %>%
  summarise(n())

#  check admission hospital equals first hospital

test1 <- cohort_all_hc %>%
  filter(int_hosp == TRUE & analysis_period == TRUE & in_age == TRUE) %>%
  filter(order_of_hosp==1) %>%
  filter(hospital_desc == admission_hospital) %>%
  summarise(n())




# there are some that int hospital was the first admitted but also step down so check this by
# the number of hospitals in the admission. They all had 3 or more so are admissions where you start in int hosp,
# transfer to non int and then back to int hospital

cohort_all_hc %>%
  count(step_1, int_hosp_is_adm_hosp, no_of_hosp)

# age distribution

cohort_all_hc %>%
  filter(int_hosp == TRUE & analysis_period == TRUE & in_age == TRUE) %>%
  ggplot(aes(x = age_on_admission, y = step_1)) +
  geom_density_ridges()

# los distribution
cohort_all_hc %>%
  filter(int_hosp == TRUE & analysis_period == TRUE & in_age == TRUE) %>%
  ggplot(aes(x = hosp_los_days, y = step_1)) +
  geom_density_ridges(rel_min_height = 0.01)

cohort_all_hc %>%
  filter(int_hosp == TRUE & analysis_period == TRUE & in_age == TRUE) %>%
  group_by(step_1) %>%
  mutate(less_than_day = hosp_los_days < 1) %>%
  summarise(mean(less_than_day), min(hosp_los_days), mean(hosp_los_days), max(hosp_los_days))


cohort_all_hc %>%
  filter(int_hosp == TRUE & analysis_period == TRUE & in_age == TRUE) %>%
  group_by(step_1) %>%
  count(no_of_hosp, int_hosp_is_adm_hosp)

#' 
#' ### Stage 2
#' 
#' In stage 2 we defined those coming into the community hospital for routine elective care from the waiting list as "step up (from waitlist)". Usually day cases, these admissions may occupy a bed that could be taken by step down patients perhaps.
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
#| label: stage 2

# Going to code routine elective as step up as from waiting list


cohort_all_hc %>%
  filter(int_hosp == TRUE & analysis_period == TRUE & in_age == TRUE) %>%
  filter(step_1 == "first") %>%
  count(admeth_desc)

cohort_all_hc <- cohort_all_hc %>%
  mutate(step_2 = case_when(
    int_hosp == TRUE & analysis_period == TRUE & in_age == TRUE & step_1 == "first" & admeth_desc ==
      "Routine elective (i.e. from waiting list as planned, excludes planned transfers)"
    ~ "Routine elective (step up)",
    .default = step_1
  ))




#' 
#' ### Stage 3
#' 
#' While there are variables that give insight into the route a patient took to be admitted to the community hospital, none is clear cut in telling you if they stepped down immediately from a previous admission. As mentioned before this is pertinent for hospital at home admissions that are treated as new admissions whereas they may well be step downs. Obviously hospital at home is not a physical hospital but will still be limited by staff availability and general resourcing so could still be affected by step down admissions. We explored whether those who were first admitted to a intermediate hospital setting had previously been discharged from an emergency or inpatient admission in the last 7 days. Seven days is an arbitrary cut-off but one is needed.
#' 
#' #### Consolidation
#' 
#' We end up with 7 categories that while not a problem is perhaps too many for general analysis. To simplify we combined step downs and across in their categories and also combined all discharged in last 7 days. This gave four categories.
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
#| label: stage 3
# step3  last admission within 7 days
# group by chi
# filter any chi with a first
# sort by start date
# count group size
# select the first row and its lag (via lead as in is next row first)



stage_3 <- cohort_all_hc %>%
  group_by(dash611_study_num) %>%
  filter(any(step_2 == "first")) %>%
  arrange(hosp_start_final_dt, hosp_end_final_dt, .by_group = TRUE) %>%
  mutate(n = n()) %>%
  filter(step_2 == "first" | lead(step_2 == "first")) %>%
  ungroup() %>% # this is speed up as interval lag slow by group
  mutate(hosp_fin_start_gaps = interval(lag(hosp_end_final_dt), hosp_start_final_dt)) %>%
  mutate(hosp_fin_start_gaps_days = round(time_length(hosp_fin_start_gaps, unit = "days"), digits = 2)) %>%
  group_by(dash611_study_num) %>%
  mutate(across(hosp_fin_start_gaps:hosp_fin_start_gaps_days, ~ ifelse(row_number() == 1, NA, .x))) %>%
  mutate(prev_hosp_3 = lag(hospital_desc)) %>%
  mutate(prev_source_3 = lag(source)) %>%
  mutate(prev_hosp_int_3 = prev_hosp_3 %in% ih_names) %>%
  ungroup() %>%
  filter(step_2 == "first")

# remember case_when is seq so next doesn't replace previous
# to easy sensitivity analysis going to split into various categories and can exclude 



stage_3 <- stage_3 %>%
  mutate(step_3 = case_when(
      hospital_desc == prev_hosp_3 
      ~ "Step up (readmission)",
      hosp_fin_start_gaps_days <= 0 
    ~ "Step down (no admission gap)",
      hosp_fin_start_gaps_days <= 0 
    ~ "Step down (no admission gap)",
      hosp_fin_start_gaps_days <= 1  
    ~ "Step down (within 24 hours)",
      hosp_fin_start_gaps_days <= 2  
    ~ "Step up (within 48 hours)", 
      hosp_fin_start_gaps_days <= 3  
    ~ "Step up (within 72 hours)",
      hosp_fin_start_gaps_days <= 7  
    ~ "Step up (within a week)",
      hosp_fin_start_gaps_days > 7  
    ~ "Step up (more than a week)", 
      is.na(hosp_fin_start_gaps_days)  
    ~ "Step up (no previous)",
    .default = step_2))


stage_3 %>%
  filter(hospital_desc == prev_hosp_3) %>%
  count(step_3) 
  


## join back up in 



cohort_steps <- cohort_all_hc %>%
  filter(analysis_period == TRUE & int_hosp == TRUE & in_age == TRUE) %>%
  filter(step_2 != "first") %>%
  bind_rows(stage_3) %>%
  mutate(step_3 = ifelse(is.na(step_3), step_2, step_3))
 




cohort_steps %>%
  filter(hosp_los_days < 50) %>%
  ggplot(aes(x = hosp_los_days, y = step_3)) +
  geom_density_ridges()


cohort_steps <- cohort_steps %>%
  mutate(step = case_when(
    str_sub(step_3, 1, 6) == "Step d" ~ "Step down",
    str_sub(step_3, 1, 6) == "Step u" ~ "Step up",
   .default = step_3
  ))


cohort_steps %>%
 count(step, step_3, step_2, step_1)


saveRDS(cohort_steps, "data/cohort_steps.RDS")

