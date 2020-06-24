#Input
url <- 'https://docs.google.com/spreadsheets/d/1DE08OpdFC2eH8VwAV-xLmsFByGh2VvL0DRDjPLhyo9U/edit?usp=sharing'

#Load packages
#install.packages('gsheet')
#install.packages('dplyr')
#install.packages('DT')
#install.packages('reactable')
#install.packages('stringr')

#library(gsheet)
#library(dplyr)
#library(formattable)
#library(magrittr)
#library(tidyverse)
#library(tidyr)
#library(DT)
#library(reactable)
#library(stringr)

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
              minimum_pain_duration, pain_duration_months_m, pain_duration_sd
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

sample_cols <- c("study sample size", "cohort sample size (pre)", "cohort sample size (post)",
                 "cohort sample size (final follow-up", "attrition rate post intervention",
                 "attrition rate at final follow-up")

patient_cols <- c("% females", "patient group", "minimum pain duration",
                  "mean pain duration in months", "mean age (sd)"
                  )

#create table 1 with reactable
reactable(t1,
          defaultColGroup = colGroup(headerClass = "header"),
          style = list(fontFamily = "Arial Narrow", fontSize = "14px"),
          columnGroups = list(
            colGroup(name = "sample size", columns = sample_cols),
            colGroup(name = "patient characteristics", columns = patient_cols)
          ),
          defaultColDef = colDef(
            header = function(value) gsub("_", " ", value, fixed = TRUE),
            cell = function(value) format(value, nsmall = 1),
            align = "left",
            minWidth = 70,
            headerStyle = list(background = "#f7f7f8")
          )
          )
          


#create t2 with reactable
#create unique variable for each treatment modality
t_2$ed <- 0  
t_2$ed[str_detect(t_2$treatment_modalities, "education")] <- 1

t_2$ex <- 0  
t_2$ex[str_detect(t_2$treatment_modalities, "exercise")] <- 1

t_2$ga <- 0  
t_2$ga[str_detect(t_2$treatment_modalities, "graded activity")] <- 1

t_2$ph <- 0  
t_2$ph[str_detect(t_2$treatment_modalities, "pharmacological treatment")] <- 1

t_2$wo <- 0  
t_2$wo[str_detect(t_2$treatment_modalities, "workplace advice")] <- 1

t_2$bt <- 0  
t_2$bt[str_detect(t_2$treatment_modalities, "(cognitive) behavioral therapy")] <- 1

t_2$pm <- 0  
t_2$pm[str_detect(t_2$treatment_modalities, "pain management skills")] <- 1

t_2$te <- 0  
t_2$te[str_detect(t_2$treatment_modalities, "team meetings")] <- 1

t_2$ba <- 0  
t_2$ba[str_detect(t_2$treatment_modalities, "body awareness therapy")] <- 1

t_2$re <- 0  
t_2$re[str_detect(t_2$treatment_modalities, "relaxation")] <- 1

#create unique variable for each hcp
t_2$phy <- 0  
t_2$phy[str_detect(t_2$healthcare_providers, "Physician")] <- 1

t_2$psy <- 0  
t_2$psy[str_detect(t_2$healthcare_providers, "psychologist")] <- 1

t_2$pt <- 0  
t_2$pt[str_detect(t_2$healthcare_providers, "physical therapist")] <- 1

t_2$ot <- 0  
t_2$ot[str_detect(t_2$healthcare_providers, "nurse")] <- 1

t_2$nur <- 0  
t_2$nur[str_detect(t_2$healthcare_providers, "nurse")] <- 1

t_2$swo <- 0  
t_2$swo[str_detect(t_2$healthcare_providers, "social worker")] <- 1




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







#export to excel
write.table(t_1)
write.table(t_1, file = "table_patient.txt", quote = TRUE, sep = ";",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")

write.table(t_2)
write.table(t_2, file = "table_intervention.txt", quote = TRUE, sep = ";",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")