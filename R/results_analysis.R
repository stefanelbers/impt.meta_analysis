# data analysis for results section #
#first load packages and data


dat_res <- dat_clean

## study characteristics.

#create author year variable
dat_res$year <- paste("(", dat_res$year, ")", sep="")
dat_res$author_year <- paste(dat_res$author, dat_res$year, sep = " ")

#obtain number of studies and cohorts
a <- table(dat_res$number_of_cohorts)
b <-a[names(a)==2]
n_cohort <- nrow(dat_res)
n_study <- (n_cohort-b/2)

print(n_cohort)
print(n_study)

#obtain study design
dat_res_study <- dat_res %>%
  dplyr::filter(cohort_id == 1)
  
dat_res_study %>% count(study_design)

#find median/range for final follow-up
summary(dat_res$`final follow-up`)

#find studies that were not included in the meta-analsyis
missing_meta <- filter(dat_res, is.na(dat_res$`final follow-up`)) 

table(dat_res$author_year, dat_res$`final follow-up`)

#analyze sample size pre

dat_n <- dat_res %>%
  select(author, year, cohort_id, sample_size, sample_size_pre, sample_size_post, sample_size_fu)

summary(dat_n$sample_size_pre)

#calculate attrition

dat_n$attrition_post <- round(100-((dat_n$sample_size_post/dat_n$sample_size_pre)*100), 2)
dat_n$attrition_fu <- round(100-((dat_n$sample_size_fu/dat_n$sample_size_post)*100), 2)

summary(dat_n$attrition_post)
summary(dat_n$attrition_fu)

#leave out the studies that performed a complete case analysis
#this is not yet complete. We have to reassess the data before completing this point.
possible_complete_case_post <- dat_n %>%
  na.omit() %>%
  dplyr::filter(attrition_post <= 0)

possible_complete_case_fu <- dat_n %>%
  na.omit() %>%
  dplyr::filter(attrition_fu <= 0)


dat_n$f <- dat_n$attrition_post+dat_n$attrition_fu

dat_n_f <- dat_n %>%
  dplyr::filter(dat_n$f > 0)

summary(dat_n_f$attrition_post)
summary(dat_n_f$attrition_fu)

#analyze risk of bias 
describe(dat_rob[, c(6:16)])

table(dat_rob[, c(6:16)])
summarytools::freq(dat_rob[,c(6:16)], order = "freq")



##patient characteristics

#nationality
unique(dat_res$nationality)
non_eu <- c("Iran", "Malaysia", "Canada", "United States", "Australia", "USA")
non_west <- c("Iran", "Malaysia")

dat_res$eu <- 1
dat_res$eu[dat_res$nationality %in% non_eu] <- 0
dat_res$west <- 1
dat_res$west[dat_res$nationality %in% non_west] <- 0

summarytools::freq(dat_res$eu)
summarytools::freq(dat_res$west)

#gender, age and pain duration
psych::describe(dat_res$female_gender)
psych::describe(dat_res$age_m)
psych::describe(dat_res$pain_duration_months_m)

##intervention characteristics
psych::describe(dat_res$time_span)
psych::describe(dat_res$hours)
summarytools::freq(dat_res$in_out_patient)   
summarytools::freq(dat_res$type_of_contact)
summarytools::freq(dat_res$setting)

#describe treatment modalities
dat_mod <- dat_res %>%
  select(author, year, id, treatment_modalities)

dat_mod <- dat_mod %>%
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

dat_mod[c(treatment_mod_abb)] <- 1*dat_mod[c(treatment_mod_abb)]
dat_mod[c(treatment_mod_abb)][is.na(dat_mod[c(treatment_mod_abb)])] <- 0

dat_mod$total <- rowSums(dat_mod[,c(5:14)])

summarytools::freq(dat_mod$ex)
summarytools::freq(dat_mod$ed)
summarytools::freq(dat_mod$ga)
summarytools::freq(dat_mod$ph)
summarytools::freq(dat_mod$wo)
summarytools::freq(dat_mod$bt)
summarytools::freq(dat_mod$pm)
summarytools::freq(dat_mod$te)
summarytools::freq(dat_mod$ba)
summarytools::freq(dat_mod$re)

psych::describe(dat_mod$total)

#describe involved hcps

dat_hcp <- dat_res %>%
  select(author, year, id, healthcare_providers, other_healthcare_providers)

dat_hcp <- dat_hcp %>%
  mutate(
    phy = str_detect(healthcare_providers, "Physician"),
    psy = str_detect(healthcare_providers, "psychologist"),
    pt = str_detect(healthcare_providers, "physical therapist"),
    ot = str_detect(healthcare_providers, "occupational therapist"),
    nur = str_detect(healthcare_providers, "nurse"),
    swo = str_detect(healthcare_providers, "social worker")
  )

hcp_abb <- c("phy", "psy", "pt", "ot", "nur", "swo")

dat_hcp[c(hcp_abb)] <- 1*dat_hcp[c(hcp_abb)]
dat_hcp[c(hcp_abb)][is.na(dat_hcp[c(hcp_abb)])] <- 0

dat_hcp$total <- rowSums(dat_hcp[,c(6:11)])

summarytools::freq(dat_hcp)


psych::describe(dat_hcp$total) 
#range is not correct: Eijk-Hustings is likely to have at least 3 hcp's; 
#Tavafian and Vendrig have additional HCPs in the 'other' section. The same is true for Olason
#therefore: range = 3-7

summarytools::freq(dat_hcp$phy)
summarytools::freq(dat_hcp$psy)
summarytools::freq(dat_hcp$pt)
summarytools::freq(dat_hcp$ot)
summarytools::freq(dat_hcp$nur)
summarytools::freq(dat_hcp$swo)


#calculate % of studies that include 'other' HCP professions

missing_hcp <- (sum(is.na(dat_hcp$other_healthcare_providers)))

total_cohorts <- nrow(dat_hcp)

#calculate percentage of studies that include other HCPs in their team
missing_hcp/total_cohorts*100


#describe follow-up and tailoring of treatment programs
dat_fu <- dat_res %>%
  select(author, year, id, followup_sessions_provided, followup_sessions_description, tailoring_mentioned_in_study, tailoring)

summarytools::freq(dat_fu$followup_sessions_provided)
summarytools::freq(dat_fu$tailoring)

