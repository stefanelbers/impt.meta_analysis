
##prepare study characteristics table ##

#obtain colnames

datcolnames <- colnames(dat_clean)
colnames <- as_tibble(datcolnames)

#create table

t_1 <- select(dat_clean, author, year, cohort_id, cohort_name, sample_size, sample_size_pre, sample_size_post, sample_size_fu, 
              nationality, female_gender, age_m, age_sd, patient_group, minimum_pain_duration, pain_duration_months_m, 
              pain_duration_sd)

t_2 <- select(dat_clean, author, year, cohort_id, treatment_aim, treatment_modalities, healthcare_providers, in_out_patient, type_of_contact, group_size,
              mode_of_delivery_subinfo, setting, time_span, hours, minutes, tailoring, followup_sessions_provided, 
              followup_sessions_description, other_healthcare_providers, other_procedures)

#recode cohort_id
t_1$cohort_id <- t_1$cohort_id %>% dplyr::recode("1" = "a", "2" = "b")
t_2$cohort_id <- t_2$cohort_id %>% dplyr::recode("1" = "a", "2" = "b")

#merge mean and sd
t_1$age_sd <- as.character(t_1$age_sd)
t_1$age_m <-as.character(t_1$age_m)
t_1$age_sd <- paste("(", t_1$age_sd, ")", sep = "")
t_1$age_m_sd <- paste(t_1$age_m, t_1$age_sd, sep = " ")
t_1$year <- paste("(", t_1$year, ")", sep="")
t_1$author_year <- paste(t_1$author, t_1$year, sep = " ")
t_2$year <- paste("(", t_2$year, ")", sep="")
t_2$author_year <- paste(t_2$author, t_2$year, sep = " ")



#calculate attrition

t_1$attrition_post <- round(100-((t_1$sample_size_post/t_1$sample_size_pre)*100), 2)
t_1$attrition_fu <- round(100-((t_1$sample_size_fu/t_1$sample_size_post)*100), 2)



#change column order
t1 <- select(t_1, author_year, cohort_id, cohort_name, sample_size, sample_size_pre, 
             sample_size_post, sample_size_fu, attrition_post, attrition_fu, 
             nationality, female_gender, age_m_sd, age_m, age_sd, patient_group, 
             minimum_pain_duration, pain_duration_months_m
)

#change column names
t1 <- t1 %>% 
  dplyr::rename(
    "study sample size" = "sample_size",
    "cohort sample size (pre)" = "sample_size_pre",
    "cohort sample size (post)" = "sample_size_post",
    "cohort sample size (final follow-up)" = "sample_size_fu",
    "attrition rate post intervention" = "attrition_post",
    "attrition rate at final follow-up" = "attrition_fu",
    "% females" = "female_gender",
    "patient group" = "patient_group",
    "minimum pain duration" = "minimum_pain_duration",
    "mean pain duration in months" = "pain_duration_months_m",
    "mean age (sd)" = "age_m_sd",
    "cohort name" = "cohort_name",
    "author (year)" = "author_year"
  ) 

t1 <- t1 %>% arrange(`author (year)`)

sample_cols <- c("study sample size", "cohort sample size (pre)", "cohort sample size (post)",
                 "cohort sample size (final follow-up)", "attrition rate post intervention",
                 "attrition rate at final follow-up")

patient_cols <- c("% females", "patient group", "minimum pain duration",
                  "mean pain duration in months", "mean age (sd)"
)


##prepare intervention characteristics table ##

#create unique variable for each treatment modality

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


t2 <- select(t_2, author_year, cohort_id, ed, ex, ga, bt, re, pm, ph, ba, wo, te, other_procedures, phy, psy, pt, ot, nur, swo,
             other_healthcare_providers, in_out_patient, type_of_contact, group_size, mode_of_delivery_subinfo, setting, 
             time_span, hours, minutes, tailoring, followup_sessions_provided, followup_sessions_description)

procedure_cols <- c("ed", "ex", "ga", "bt", "re", "pm", "ph", "ba", "wo", "te", "other_procedures")
hcp_cols <- c("phy", "psy", "pt", "ot", "nur", "swo", "other_healthcare_providers")
