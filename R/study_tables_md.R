#Input
url <- 'https://docs.google.com/spreadsheets/d/1DE08OpdFC2eH8VwAV-xLmsFByGh2VvL0DRDjPLhyo9U/edit?usp=sharing'

#Load packages
#install.packages('gsheet')
#install.packages('dplyr')
#install.packages('DT')
#install.packages('reactable')
#install.packages('stringr')
#install.packages('finalfit')

#library(gsheet)
#library(dplyr)
#library(formattable)
#library(magrittr)
#library(tidyverse)
#library(tidyr)
#library(DT)
#library(reactable)
#library(stringr)
library(finalfit)

#read data
dat <- gsheet2tbl(url)

#obtain colnames

datcolnames <- colnames(dat)
colnames <- as_tibble(datcolnames)

#create table

t_1 <- select(dat, author, year, cohort_id, cohort_name, sample_size, sample_size_pre, sample_size_post, sample_size_fu, 
              nationality, female_gender, age_m, age_sd, patient_group, minimum_pain_duration, pain_duration_months_m, 
              pain_duration_sd) %>%
  filter(!is.na(author) & !author == "test" & !author == 0)

t_2 <- select(dat, author, year, cohort_id, treatment_aim, treatment_modalities, healthcare_providers, in_out_patient, type_of_contact, group_size,
              mode_of_delivery_subinfo, setting, time_span, hours, minutes, tailoring, followup_sessions_provided, 
              followup_sessions_description, other_healthcare_providers, other_procedures) %>%
  filter(!is.na(author) & !author == "test" & !author == 0)

#recode cohort_id
t_1$cohort_id <- t_1$cohort_id %>% dplyr::recode("1" = "a", "2" = "b")
t_2$cohort_id <- t_2$cohort_id %>% dplyr::recode("1" = "a", "2" = "b")

#merge mean and sd
as.character(t_1$age_sd)
as.character(t_1$age_m)
t_1$age_sd <- paste("(", t_1$age_sd, ")", sep = "")
t_1$age_m_sd <- paste(t_1$age_m, t_1$age_sd, sep = " ")
t_1$year <- paste("(", t_1$year, ")", sep="")
t_1$author_year <- paste(t_1$author, t_2$year, sep = " ")
t_2$year <- paste("(", t_2$year, ")", sep="")
t_2$author_year <- paste(t_2$author, t_2$year, sep = " ")
t_1$author_year <- paste(t_1$author, t_1$year, sep = " ")


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

#create table 1 with reactable
reactable(t1,
          columns = list(
            "author (year)" = colDef(minWidth = 105),
            cohort_id = colDef(maxWidth = 30, name = "ID"),
            "cohort name" = colDef(minWidth = 105),
            "study sample size" = colDef(format = colFormat(digits = 0))
          ),
          defaultColGroup = colGroup(headerClass = "header"),
          style = list(fontFamily = "Arial Narrow", fontSize = "14px"),
          columnGroups = list(
            colGroup(name = "sample size", columns = sample_cols),
            colGroup(name = "patient characteristics", columns = patient_cols)
          ),
          defaultColDef = colDef(
            header = function(value) gsub("_", " ", value, fixed = TRUE),
            cell = function(value) format(value),
            align = "left",
            minWidth = 70
            ),
          searchable = TRUE,
          showPageSizeOptions = TRUE
          )
          
#create t2 with reactable
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

reactable(t2,
          defaultSorted = "author_year",
          defaultSortOrder = "asc",
          defaultColGroup = colGroup(headerClass = "header"),
          style = list(fontFamily = "Arial Narrow", fontSize = "14px"),
          columnGroups = list(
            colGroup(name = "Treatment modalities", columns = procedure_cols),
            colGroup(name = "Healthcare providers", columns = hcp_cols)
            ),
          defaultColDef = colDef(
            header = function(value) gsub("_", " ", value, fixed = TRUE),
            cell = function(value) format(value, nsmall = 1),
            align = "left",
            minWidth = 70,
            headerStyle = list(background = "#f7f7f8")
            ),
          columns = list(
            author_year = colDef(minWidth = 105),
            other_procedures = colDef(minWidth = 200),
            other_healthcare_providers = colDef(minWidth = 110),
            followup_sessions_description = colDef(minWidth = 150),
            cohort_id = colDef(maxWidth = 30, name = "ID"),
            ed = colDef(maxWidth = 30, cell = function(value) {
              # Render as ✘ or ✓
              if (value == 0) "\u2718" else "\u2713"
              }, style = function(value){
                if (value == 0) {color <- '#ABB2B9'} else {color <- '#28B463'}
                list(background = color)
              }),
            ex = colDef(maxWidth = 30, cell = function(value) {
              # Render as ✘ or ✓
              if (value == 0) "\u2718" else "\u2713"
            }, style = function(value){
              if (value == 0) {color <- '#ABB2B9'} else {color <- '#28B463'}
              list(background = color)
              }),
            ga = colDef(maxWidth = 30, cell = function(value) {
              # Render as ✘ or ✓
              if (value == 0) "\u2718" else "\u2713"
            }, style = function(value){
              if (value == 0) {color <- '#ABB2B9'} else {color <- '#28B463'}
              list(background = color)
            }),
            ph = colDef(maxWidth = 30, cell = function(value) {
              # Render as ✘ or ✓
              if (value == 0) "\u2718" else "\u2713"
            }, style = function(value){
              if (value == 0) {color <- '#ABB2B9'} else {color <- '#28B463'}
              list(background = color)
            }),
            wo = colDef(maxWidth = 30, cell = function(value) {
              # Render as ✘ or ✓
              if (value == 0) "\u2718" else "\u2713"
            }, style = function(value){
              if (value == 0) {color <- '#ABB2B9'} else {color <- '#28B463'}
              list(background = color)
            }),
            bt = colDef(maxWidth = 30, cell = function(value) {
              # Render as ✘ or ✓
              if (value == 0) "\u2718" else "\u2713"
            }, style = function(value){
              if (value == 0) {color <- '#ABB2B9'} else {color <- '#28B463'}
              list(background = color)
            }),
            pm = colDef(maxWidth = 30, cell = function(value) {
              # Render as ✘ or ✓
              if (value == 0) "\u2718" else "\u2713"
            }, style = function(value){
              if (value == 0) {color <- '#ABB2B9'} else {color <- '#28B463'}
              list(background = color)
            }),
            ba = colDef(maxWidth = 30, cell = function(value) {
              # Render as ✘ or ✓
              if (value == 0) "\u2718" else "\u2713"
            }, style = function(value){
              if (value == 0) {color <- '#ABB2B9'} else {color <- '#28B463'}
              list(background = color)
            }),
            te = colDef(maxWidth = 30, cell = function(value) {
              # Render as ✘ or ✓
              if (value == 0) "\u2718" else "\u2713"
            }, style = function(value){
              if (value == 0) {color <- '#ABB2B9'} else {color <- '#28B463'}
              list(background = color)
            }),
            re = colDef(maxWidth = 30, cell = function(value) {
              # Render as ✘ or ✓
              if (value == 0) "\u2718" else "\u2713"
            }, style = function(value){
              if (value == 0) {color <- '#ABB2B9'} else {color <- '#28B463'}
              list(background = color)
            }),
            phy = colDef(maxWidth = 30, cell = function(value) {
              # Render as ✘ or ✓
              if (value == 0) "\u2718" else "\u2713"
            }, style = function(value){
              if (value == 0) {color <- '#ABB2B9'} else {color <- '#28B463'}
              list(background = color)
            }),
            psy = colDef(maxWidth = 30, cell = function(value) {
              # Render as ✘ or ✓
              if (value == 0) "\u2718" else "\u2713"
            }, style = function(value){
              if (value == 0) {color <- '#ABB2B9'} else {color <- '#28B463'}
              list(background = color)
            }),
            pt = colDef(maxWidth = 30, cell = function(value) {
              # Render as ✘ or ✓
              if (value == 0) "\u2718" else "\u2713"
            }, style = function(value){
              if (value == 0) {color <- '#ABB2B9'} else {color <- '#28B463'}
              list(background = color)
            }),
            ot = colDef(maxWidth = 30, cell = function(value) {
              # Render as ✘ or ✓
              if (value == 0) "\u2718" else "\u2713"
            }, style = function(value){
              if (value == 0) {color <- '#ABB2B9'} else {color <- '#28B463'}
              list(background = color)
            }),
            nur = colDef(maxWidth = 30, cell = function(value) {
              # Render as ✘ or ✓
              if (value == 0) "\u2718" else "\u2713"
            }, style = function(value){
              if (value == 0) {color <- '#ABB2B9'} else {color <- '#28B463'}
              list(background = color)
            }),
            swo = colDef(maxWidth = 30, cell = function(value) {
              # Render as ✘ or ✓
              if (value == 0) "\u2718" else "\u2713"
            }, style = function(value){
              if (value == 0) {color <- '#ABB2B9'} else {color <- '#28B463'}
              list(background = color)
            })
          ),
          bordered = FALSE,
          highlight = FALSE,
          striped = FALSE,
          searchable = TRUE,
          showPageSizeOptions = TRUE,
          onClick = "expand"
          )

# create t3: intervention characteristics for manuscript

#round treatment time to full hours
t_3 <- t_2

t_3$treatment_time <- t_3$hours
t_3$minutes[is.na(t_3$minutes) ] <- 0
t_3$tailoring[is.na(t_3$tailoring)] <- "lo"
t_3$tailoring[t_3$tailoring=="Low"] <- "lo"
t_3$tailoring[t_3$tailoring=="Medium"] <- "med"
t_3$tailoring[t_3$tailoring=="High"] <- "hi"
t_3$in_out_patient[t_3$in_out_patient=="outpatient"] <- "out"
t_3$in_out_patient[t_3$in_out_patient=="inpatient"] <- "in"
t_3$in_out_patient[t_3$in_out_patient=="inpatient and outpatient combined"] <- "mix"
t_3$settingt[t_3$setting=="treatment/rehabilitation center"] <- "rehab center"
t_3$followup_sessions_provided[is.na(t_3$followup_sessions_provided)] <- "no"
t_3$followup_sessions_provided[t_3$followup_sessions_provided=="No"] <- "no"
t_3$followup_sessions_provided[t_3$followup_sessions_provided=="Yes"] <- "yes"
t_3$author_year_id <- paste(t_3$author_year, t_3$cohort_id, sep = ": ")
t_3$treatment_modalities <- gsub("education", "ed", t_3$treatment_modalities)
t_3$treatment_modalities <- gsub("exercise", "ex", t_3$treatment_modalities)
t_3$treatment_modalities <- gsub("(cognitive) behavioral therapy", "bt", t_3$treatment_modalities)
t_3$treatment_modalities <- gsub("relaxation", "re", t_3$treatment_modalities)
t_3$treatment_modalities <- gsub("pain management skills", "pm", t_3$treatment_modalities)
t_3$treatment_modalities <- gsub("Other", "oth", t_3$treatment_modalities)
t_3$treatment_modalities <- gsub("graded activity", "ga", t_3$treatment_modalities)
t_3$treatment_modalities <- gsub("pharmacological treatment", "ph", t_3$treatment_modalities)
t_3$treatment_modalities <- gsub("body awareness therapy", "ba", t_3$treatment_modalities)
t_3$treatment_modalities <- gsub("workplace advice", "wo", t_3$treatment_modalities)
t_3$treatment_modalities <- gsub("team meetings", "te", t_3$treatment_modalities)



for (i in 1:dim(t_2)[1]){
  if (t_2$minutes[i] > 29){
    t_2$treatment_time[i] <- t_2$treatment_time[i] +1
    }
}



t3 <- select(t_3, author_year_id, treatment_aim, treatment_modalities, phy, psy, pt, ot, nur, swo, in_out_patient, type_of_contact, group_size,setting, 
             time_span, treatment_time, tailoring, followup_sessions_provided)



#summary statistics treatment modality

modality_desc <- t2[3:12]

for (i in seq_along(modality_desc)) {            # 2. sequence
  print(colnames(modality_desc[i]))
  print(summarytools::freq(modality_desc[i]))
}

modality_desc <- modality_desc %>% replace(is.na(.), 0) %>% mutate(sum = rowSums(.[1:10]))
summary(modality_desc$sum)
summarytools::freq(modality_desc$sum)

#summary statistics HCP
hcp_desc <- t2[14:19]

for (i in seq_along(hcp_desc)) {            # 2. sequence
  print(colnames(hcp_desc[i]))
  print(Freq(hcp_desc[i]))
}

hcp_desc <- hcp_desc %>% replace(is.na(.), 0) %>% mutate(sum = rowSums(.[1:6]))
summary(hcp_desc$sum)
Freq(hcp_desc$sum)

#summary statistics inpatient
Freq(t2$in_out_patient)
Freq(t2$setting)




#export to .txt

write.table(t1, file = "t1_export.txt", append = FALSE, sep = ";", dec = ".",
            row.names = TRUE, col.names = TRUE)

write.table(t2, file = "t2_export.txt", append = FALSE, sep = "\t", dec = ".",
            row.names = FALSE, col.names = TRUE)

write.table(t3, file = "t3_export.txt", append = FALSE, sep = "\t", dec = ".",
            row.names = FALSE, col.names = TRUE)
