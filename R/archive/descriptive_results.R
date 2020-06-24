#install packages
#install.packages("epitools")
#install.packages("dplyr")

#load packages
library(epitools)
library(dplyr)
library(gsheet)
library(stringr)

#Input
url <- 'https://docs.google.com/spreadsheets/d/1DE08OpdFC2eH8VwAV-xLmsFByGh2VvL0DRDjPLhyo9U/edit?usp=sharing'

#read data
dat <- gsheet2tbl(url)

### Descriptive statistics ###

#create datafile
dat_clean <- dat %>%
  filter(!is.na(author) & !author == "test" & !author == 0)
dat_clean$cohort <- paste(dat_clean$author, dat_clean$year, dat_clean$cohort_id, sep = "_")

#create descriptive tables
t_1 <- select(dat, author, year, sample_size, sample_size_pre, sample_size_post, sample_size_fu, 
              nationality, female_gender, age_m, age_sd, patient_group, minimum_pain_duration, pain_duration_months_m, 
              pain_duration_sd) %>%
  filter(!is.na(author) & !author == "test" & !author == 0)

t_2 <- select(dat, author, year, treatment_aim, treatment_modalities, healthcare_providers, in_out_patient, type_of_contact, group_size,
              mode_of_delivery_subinfo, setting, time_span, hours, minutes, tailoring, followup_sessions_provided, 
              followup_sessions_description) %>%
  filter(!is.na(author) & !author == "test" & !author == 0)

#calculate attrition

t_1$attrition_post <- round(100-((t_1$sample_size_post/t_1$sample_size_pre)*100), 2)
t_1$attrition_fu <- round(100-((t_1$sample_size_fu/t_1$sample_size_post)*100), 2)


#data study_design of included studies
dat_study <- dat_clean %>%
  filter(cohort_id == 1) %>%
  arrange(author)

table(dat_study$study_design)

#number of included cohorts

dat_clean %>%
  count(cohort)

#max fu
dat_clean$hrqol_fu1_t %>%
  summarize()


summary(dat_clean$sample_size_pre)

#obtain descriptives attrition
summary(t_1$attrition_post)
summary(t_1$attrition_fu)

#obtain descriptives nationality

table(t_1$nationality)


#descriptives sex
summary(t_1$female_gender)

#pain duration
t_1$pain_duration_months_m <- as.numeric(t_1$pain_duration_months_m)

summary(t_1$pain_duration_months_m)
!is.na(t_1$pain_duration_months_m)

#treatment components

t_2 <- t_2 %>%
  mutate(
    ed = str_detect(treatment_modalities, "education"),
    ex = str_detect(treatment_modalities, "exercise"),
    ga = str_detect(treatment_modalities, "graded activity"),
    ph = str_detect(treatment_modalities, "pharmacological treatment"),
    wo = str_detect(treatment_modalities, "workplace advice"),
    bt = str_detect(treatment_modalities, "behavioral therapy"),
    pm = str_detect(treatment_modalities, "pain management skills"),
    te = str_detect(treatment_modalities, "team meetings"),
    ba = str_detect(treatment_modalities, "body awareness therapy"),
    re = str_detect(treatment_modalities, "relaxation")
  )

treatment_mod_abb <- c("ed", "ex", "ga", "ph", "wo", "bt", "pm", "te", "ba", "re")

t_2[c(treatment_mod_abb)] <- 1*t_2[c(treatment_mod_abb)]
t_2[c(treatment_mod_abb)][is.na(t_2[c(treatment_mod_abb)])] <- 0

t_2 <- t_2 %>%
  mutate(
    phy = str_detect(healthcare_providers, "Physician"),
    psy = str_detect(healthcare_providers, "psychologist"),
    pt = str_detect(healthcare_providers, "physical therapist"),
    ot = str_detect(healthcare_providers, "occupational therapist"),
    nur = str_detect(healthcare_providers, "nurse"),
    swo = str_detect(healthcare_providers, "social worker")
  )

hcp_abb <- c("phy", "psy", "pt", "ot", "nur", "swo")

t_2[c(hcp_abb)] <- 1*t_2[c(hcp_abb)]
t_2[c(hcp_abb)][is.na(t_2[c(hcp_abb)])] <- 0


                          
