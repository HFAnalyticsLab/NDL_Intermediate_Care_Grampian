#' ---
#' title: "Scales"
#' format: html
#' date: today
#' execute: 
#'   echo: false
#'   warning: false
#' ---
#' 
#' ## Scales
## ------------------------------------------------------------------------------------------------------------------------------------
#| label: setup
source("setup.R")

#' 
#' ### Scale 1 - multimorbidity from a 5-year lookback at diagnostic codes from inpatient admissions
#' 
#' We use the `comorbidity` package in R to calculate the Elixhauser comorbidity score.
#' 
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
#| label: multi morbidity

# open cohort_steps derived from 2_steps.qmd
# open cohort_adm a flat file for all admissions in last ten years for the cohort from 1_cohort.qmd

cohort_steps <- readRDS("data/cohort_steps.RDS")
cohort_adm <- readRDS("data/cohort_adm.RDS")

# create a long format (i.e one icd code per row) file

diagnosis_10_yrs <- cohort_adm %>%
  mutate(admission_date = date(adm_date_time)) %>%
  select(
    dash611_study_num, all_diagnoses_on_admission,
    admission_date
  ) %>%
  separate_longer_delim(all_diagnoses_on_admission, delim = ";")

# join the long format to each of the unique admissions
# for the cohort in the analysis period
# this works because we join by person (chi),
# so each admission for that person gets a set of admissions
# that can be filtered to 5 years before


diagnosis_5yrs_b4 <- cohort_steps %>%
  distinct(da_sh611_paadm_admno_uid, .keep_all = TRUE) %>%
  mutate(admission_date = date(adm_date_time)) %>%
  mutate(look_back_date = admission_date %m-% years(5)) %>%
  select(dash611_study_num, da_sh611_paadm_admno_uid, admission_date, look_back_date) %>%
  left_join(
    diagnosis_10_yrs,
    join_by(dash611_study_num, admission_date >= admission_date)
  ) %>%
  filter(admission_date.y >= look_back_date) %>%
  mutate(check = time_length(interval(admission_date.x, admission_date.y), unit = "years"))

# check five years or less ago

stopifnot(diagnosis_5yrs_b4$check <= 5)

# everyone has at least one diagnosis


n_distinct(diagnosis_5yrs_b4$da_sh611_paadm_admno_uid)
n_distinct(cohort_steps$da_sh611_paadm_admno_uid)

# ok comorbidity

diagnosis_5yrs_b4_comorb <- diagnosis_5yrs_b4 %>%
  select(-admission_date.y) %>%
  rename(admission_date = admission_date.x) %>%
  comorbidity(
    .,
    id = "da_sh611_paadm_admno_uid",
    code = "all_diagnoses_on_admission",
    map = "elixhauser_icd10_quan",
    assign0 = TRUE,
    labelled = TRUE,
    tidy.codes = TRUE
  ) %>%
  mutate(elixhauser_score = score(., weights = "swiss", assign0 = TRUE))

# add multimorbidity intos steps

cohort_scales <- cohort_steps %>%
  left_join(diagnosis_5yrs_b4_comorb)

nrow(is.na(cohort_scales$elixhauser_score))

# can be negative

cohort_scales %>%
  ggplot(aes(elixhauser_score)) +
  geom_histogram(binwidth = 1)




#' 
#' 
#' #### Scale 2  routine care from a 5-year lookback at out-patient clinic types and frequency of attendances in the TrakCare record.
#' 
#' Count number of appointments and then count  by type of   specialties
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
#| label: outpatient

# appointments

outpatient_10_yrs <- readRDS("data/cohort_op.RDS") %>%
  filter(attendance_status_desc == "Attended") %>%
  mutate(appointment_date = date(appt_date_time)) %>%
  select(dash611_study_num, appointment_date, specialty_national_desc)

outpatient_5yrs_b4 <- cohort_scales %>%
  distinct(da_sh611_paadm_admno_uid, .keep_all = TRUE) %>%
  mutate(admission_date = date(adm_date_time)) %>%
  mutate(look_back_date = admission_date %m-% years(5)) %>%
  select(dash611_study_num, da_sh611_paadm_admno_uid, admission_date, look_back_date) %>%
  left_join(
    outpatient_10_yrs,
    join_by(dash611_study_num, admission_date >= appointment_date)
  ) %>%
  filter(appointment_date >= look_back_date) %>%
  mutate(check = time_length(interval(appointment_date, admission_date), unit = "years"))

# number of appointments

outpatient_5yrs_b4_count <- outpatient_5yrs_b4 %>%
  count(da_sh611_paadm_admno_uid, name = "n_outpatients")


cohort_scales <- cohort_scales %>%
  left_join(outpatient_5yrs_b4_count) %>%
  mutate(n_outpatients = if_else(is.na(n_outpatients), 0, n_outpatients))

# number of specialties

outpatient_5yrs_b4_spec <- outpatient_5yrs_b4 %>%
  select(da_sh611_paadm_admno_uid, specialty_national_desc) %>%
  filter(!is.na(specialty_national_desc)) %>%
  distinct(da_sh611_paadm_admno_uid, specialty_national_desc) %>%
  group_by(da_sh611_paadm_admno_uid) %>%
  summarise(n_specialty = n()) %>%
  ungroup() 
  
cohort_scales <- cohort_scales %>%
  left_join(outpatient_5yrs_b4_spec) %>%
  mutate(n_specialty = if_else(is.na(n_specialty), 0, n_specialty))

saveRDS(cohort_scales, "data/cohort_scales.RDS")

  
  
  #mutate(specialty_national_desc = str_to_lower(specialty_national_desc)) %>%
  #mutate(specialty_national_desc = str_replace_all(specialty_national_desc, " & ", "_")) %>%
  #mutate(specialty_national_desc = str_replace_all(specialty_national_desc, " and ", "_")) %>%
  #mutate(specialty_national_desc = str_remove_all(specialty_national_desc, "[:punct:]")) %>%
  #mutate(specialty_national_desc = str_replace_all(specialty_national_desc, " ", "_")) %>


#top20 <- outpatient_5yrs_b4_spec %>%
#  summarise(across(starts_with("out"), ~ mean(.x > 0) * 100)) %>%
#  pivot_longer(everything(),
#    names_to = "specialty_national_desc",
#    values_to = "pc"
#  ) %>%
#  arrange(desc(pc)) %>%
#  slice_head(n = 20)

#outpatient_5yrs_b4_spec <- outpatient_5yrs_b4_spec %>%
#  select(paadm_admno, contains(top20$specialty_national_desc))


#' 
#' 
#' 
#' #### Scale 3 Polypharmacy from a 1-year lookback at prescribing in the PIS record
#' 
## ------------------------------------------------------------------------------------------------------------------------------------
#| label: polypharmacy

cohort_phar <- readRDS("data/cohort_phar.RDS")

pharmacy_10_yrs <- cohort_phar %>%
  select(
    dash611_study_num, pi_bnf_item_code,
    paid_date
  ) %>%
  mutate(bnf_chapter = str_sub(pi_bnf_item_code, 1, 2)) %>%
  mutate(bnf_sub_chapter = str_sub(pi_bnf_item_code, 1, 4)) 

# limitation is that paid end of the month

pharmacy_1yrs_b4 <- cohort_scales %>%
  distinct(da_sh611_paadm_admno_uid, .keep_all = TRUE) %>%
  mutate(admission_date = as_date(adm_date_time)) %>%
  mutate(look_back_date = admission_date %m-% years(1)) %>%
  select(dash611_study_num, da_sh611_paadm_admno_uid, admission_date, look_back_date) %>%
  left_join(
    pharmacy_10_yrs,
    join_by(dash611_study_num, admission_date >= paid_date)
  ) %>%
  filter(paid_date >= look_back_date) %>%
  distinct() %>%
  mutate(check = time_length(interval(paid_date, admission_date), unit = "years"))

# check one years or less ago (leap year so 1.01)

stopifnot(pharmacy_1yrs_b4$check <= 1.01)

# no_prescribed_under_each_chapter

pharmacy_1yrs_b4_1 <- pharmacy_1yrs_b4 %>%
  select(da_sh611_paadm_admno_uid, bnf_chapter) %>%
  filter(!is.na(bnf_chapter)) %>%
  distinct(da_sh611_paadm_admno_uid, bnf_chapter) %>%
  group_by(da_sh611_paadm_admno_uid) %>%
  summarise(n_bnf_chapters = n()) %>%
  ungroup()

# no prescribed under each sub chapter

pharmacy_1yrs_b4_2 <- pharmacy_1yrs_b4 %>%
  select(da_sh611_paadm_admno_uid, bnf_sub_chapter) %>%
  filter(!is.na(bnf_sub_chapter)) %>%
  distinct(da_sh611_paadm_admno_uid, bnf_sub_chapter) %>%
  group_by(da_sh611_paadm_admno_uid) %>%
  summarise(n_bnf_sub_chapters = n()) %>%
  ungroup()


# add to file

cohort_scales <- cohort_scales %>%
  left_join(pharmacy_1yrs_b4_1) %>%
  mutate(n_bnf_chapters = if_else(is.na(n_bnf_chapters), 0, n_bnf_chapters))

cohort_scales <- cohort_scales %>%
  left_join(pharmacy_1yrs_b4_2) %>%
  mutate(n_bnf_sub_chapters = if_else(is.na(n_bnf_sub_chapters), 0, n_bnf_sub_chapters))





#' 
#' 
## ------------------------------------------------------------------------------------------------------------------------------------

# readmission

# combine emergency and inpatient admissions


cohort_adm <- readRDS("data/cohort_adm.RDS") %>%
  select(dash611_study_num, adm_date_time = adm_date_time, da_sh611_paadm_admno_uid) 

cohort_emer <- readRDS("data/cohort_emer.RDS") %>%
  select(dash611_study_num, adm_date_time = admission_date_time, da_sh611_paadm_admno_uid) 

cohort_adm <- bind_rows("inpatient" = cohort_adm, "emergency" = cohort_emer,
                        .id = "resource")
  
# join discharges to later admissions 

admission_after_discharge <- cohort_scales %>%
  distinct(da_sh611_paadm_admno_uid, .keep_all = TRUE) %>%
  select(dash611_study_num, da_sh611_paadm_admno_uid, discharge_date_time) %>%
  mutate(discharge_date = as_date(discharge_date_time)) %>%
  left_join(
    cohort_adm,
    join_by(dash611_study_num, discharge_date_time <= adm_date_time)
  ) %>%
  filter(da_sh611_paadm_admno_uid.x != da_sh611_paadm_admno_uid.y) %>% 
  mutate(adm_date = as_date(adm_date_time)) %>%
  mutate(readmitted = ifelse(is.na(adm_date), 0, 1)) %>%
  mutate(readmission_time = 
           interval(discharge_date, adm_date)) %>%
  mutate(readmission_days = 
           time_length(readmission_time, unit = "days"))


first_admission_after_discharge <- admission_after_discharge %>%
  rename(re_da_sh611_paadm_admno_uid = da_sh611_paadm_admno_uid.y,
         da_sh611_paadm_admno_uid = da_sh611_paadm_admno_uid.x) %>%
  group_by(da_sh611_paadm_admno_uid) %>%
  arrange(readmission_days, adm_date_time, .by_group = TRUE) %>%
  slice_head() %>%
  ungroup() %>%
  select(dash611_study_num, da_sh611_paadm_admno_uid, starts_with("re") )


cohort_scales <- cohort_scales %>%
  left_join(first_admission_after_discharge, join_by(da_sh611_paadm_admno_uid, dash611_study_num)) %>%
  mutate(readmitted = ifelse(is.na(readmitted), 0 , readmitted))


cohort_scales <- cohort_scales %>%
  mutate(died = ifelse(is.na(deceased_date), 0, 1)) %>%
  mutate(deceased_date2 = if_else(is.na(deceased_date), max(deceased_date, na.rm = T), deceased_date)) %>%
  mutate(time_to_death = interval(date(discharge_date_time), deceased_date2)) %>%
  mutate(time_to_death_days = time_length(time_to_death, unit = "day")) %>%
  mutate(time_to_death_days =ifelse(time_to_death_days <=0, 0, time_to_death_days))


# dementia 

# https://bmjopen.bmj.com/content/9/6/e026759 for frailty

cohort_dem <- diagnosis_5yrs_b4 %>%
  mutate(icd10_3 = substr(all_diagnoses_on_admission, 1, 3)) %>%
  mutate(dementia_and_delirium = case_when(icd10_3 == "F00" ~ 1,
                                           icd10_3 == "F01" ~ 1,
                                           icd10_3 == "F02" ~ 1,
                                           icd10_3 == "F03" ~ 1,
                                           icd10_3 == "F05" ~ 1,
                                           icd10_3 == "G30" ~ 1,
                                           icd10_3 == "F04" ~ 1,
                                           icd10_3 == "R41" ~ 1,
                                           all_diagnoses_on_admission == "G31.1" ~ 1,
                                           all_diagnoses_on_admission == "G31.0" ~ 1,
                                           .default = 0)) %>%
  group_by(da_sh611_paadm_admno_uid) %>%
  summarise(dementia_and_delirium = max(dementia_and_delirium)) %>%
  ungroup()

cohort_scales <- cohort_scales %>%
  left_join(cohort_dem, join_by(da_sh611_paadm_admno_uid))



saveRDS(cohort_scales, "data/cohort_scales.RDS")




#' 
#' 
