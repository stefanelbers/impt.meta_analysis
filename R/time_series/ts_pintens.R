#Input
url <- 'https://docs.google.com/spreadsheets/d/1DE08OpdFC2eH8VwAV-xLmsFByGh2VvL0DRDjPLhyo9U/edit?usp=sharing'

#read data
dat <- gsheet2tbl(url)

#create time_series datafile
dat_clean <- dat %>%
  filter(!is.na(author) & !author == "test" & !author == 0)
dat_clean$cohort <- paste(dat_clean$author, dat_clean$year, dat_clean$cohort_id, sep = "_")


#create input value
input <- "pintens"

#input <- as.data.frame(c("pintens", "pf"))  
#for (row in 1:nrow(input)) { 

if (input == "pintens") { 
  
  dat_pintens <- dat_clean %>%
    filter(measurements_pain_intensity == "Yes")  
  
  ts_m <- select (dat_pintens, cohort, author, year, cohort_id, pintens_pre_m, pintens_post_m, pintens_fu1_m,
                  pintens_fu2_m, pintens_fu3_m, measurements_pain_intensity, pintens_name_measurement_instrument) %>%
    rename(instrument_name = pintens_name_measurement_instrument, 
           pre_m = pintens_pre_m,
           post_m = pintens_post_m,
           fu1_m = pintens_fu1_m,
           fu2_m = pintens_fu2_m,
           fu3_m = pintens_fu3_m,
           instrument_present = measurements_pain_intensity)
  
  ts_sd <- select (dat_pintens, cohort, pintens_pre_sd, pintens_post_sd, pintens_fu1_sd,
                   pintens_fu2_sd, pintens_fu3_sd) %>%
    rename( pre_sd = pintens_pre_sd,
            post_sd = pintens_post_sd,
            fu1_sd = pintens_fu1_sd,
            fu2_sd = pintens_fu2_sd,
            fu3_sd = pintens_fu3_sd)
  
  ts_t <- select (dat_pintens, cohort, pintens_fu1_t, pintens_fu2_t, pintens_fu3_t) %>%
    rename( fu1_t = pintens_fu1_t,
            fu2_t = pintens_fu2_t,
            fu3_t = pintens_fu3_t)
}  

ts_t$pre_t = -20
ts_t$post_t = 0

#change SD columns to numeric
cols.num <- c("pre_sd","post_sd", "fu1_sd", "fu2_sd", "fu3_sd")
ts_sd[cols.num] <- sapply(ts_sd[cols.num],as.numeric)
#sapply(ts_sd, class)


#convert to long dataset
# The arguments to gather():
# - data: Data object
# - key: Name of new key column (made from names of data columns)
# - value: Name of new value column
# - ...: Names of source columns that contain values
# - factor_key: Treat the new key column as a factor (instead of character vector)
ts_m_long <- pivot_longer(ts_m, 
                          cols = c("pre_m", "post_m", "fu1_m", "fu2_m", "fu2_m", "fu3_m"),
                          names_to = "timepoint",
                          values_to = "m")

ts_sd_long <- pivot_longer(ts_sd, 
                           cols = c("pre_sd", "post_sd", "fu1_sd", "fu2_sd", "fu2_sd", "fu3_sd"),
                           names_to = "timepoint",
                           values_to = "sd")


ts_t_long <- pivot_longer(ts_t,
                          cols = c("pre_t", "post_t", "fu1_t", "fu2_t", "fu3_t"),
                          names_to = "timepoint",
                          values_to = "t")


#merge long datasets
ts_sd_long$timepoint <-  gsub("\\_sd*$","",ts_sd_long$timepoint)
ts_t_long$timepoint <- gsub("\\_t*$","",ts_t_long$timepoint)
ts_m_long$timepoint <- gsub("\\_m*$","",ts_m_long$timepoint)

ts_sdt_long <- merge(ts_sd_long, ts_t_long, c("cohort", "timepoint"))
ts_long <- merge(ts_sdt_long, ts_m_long, c("cohort", "timepoint"))

unique(ts_long$instrument_name)

#create max_score column

ts_long$max_scale = ts_long$instrument_name
ts_long$max_scale <- recode(ts_long$max_scale, 
                            "VAS" = 100, "VAS (0-100)" = 100,"PRI" = 78, "MPI: pain severity" = 7,
                            "NRS" = 10, "NPRS" = 10, "NRS (0-10)" = 10, "VAS (0-10)"= 10, "NRS (0-100)" = 100,
                            "Likert pain intensity" = 6, "RDQ" = 24, "QBPDS" = 100, "LBPRS" = 30, "DRI" = 1200,
                            "DRI (0-100)" = 100, "MPI: pain interference" = 6, "ODI" = 100, "ODI (0-1)" = 1, "PDI" = 70, "RMDQ" = 24,
                            "DPQ: Daily activities" = 100, "SF-36 subscale Physical Functioning" = 100, "NHP: PA" = 100,
                            "HFAQ" = 100, "MPI: GA" = 6, "Norfunk (0-3)" = 3, "ADS (german scale of CES-D)" =60, "HADS-D" =21,
                            "DASS" = 42, "BDI-II" =63, "Zung" =100,"BDI" =63, "depression index (DEPS)" =30, "SCL90-D" = 52,
                            "HADS-A"= 21, "VAS Anxiety (0-100)" = 100, "SCL90-A" = 40, "STAI" = 80,
                            "NHP: Emotional reactions" = 100, "SF-36: mental health" = 100,"DPQ: anxiety/depression" = 100,
                            "SCL-90: Hostility" = 24, "PSEQ" = 60, "DPQ: social life" = 100, "SF-36: social functioning" = 100, 
                            "SIP" = 9608, "QBPRS" = 100
)

#create reverse scoring variable
#binary variable: do higher scores indicate better functioning? 0=no; 1=yes.
measReverse <- c("FIQ", "NHP", "NHP: PA", "Norfunk (0-3)", "RDQ", "QBPDS",
                 "DRI (0-100)", "PDI", "ODI (0-1)", "MPI: pain interference",
                 "RMDQ", "ODI", "QBPRS", "DPQ: Daily activities", "LBPRS",
                 "DRI", "SIP", "ADS (german scale of CES-D)", "HADS-D",
                 "BDI", "SCL90-D", "Depression index (DEPS)", "DASS", "BDI-II",
                 "Zung", "HADS-A", "VAS Anxiety (0-100)", "SCL90-A", "STAI", 
                 "NHP: Emotional reactions", "DPQ: anxiety/depression",
                 "SCL-90: Hostility", "DPQ: social life", "VAS (0-10)", 
                 "NRS (0-10)", "VAS", "NRS", "Likert pain intensity", 
                 "MPI: pain severity", "PRI", "NPRS", "NRS (0-100)")

#questionnaires in the current dataset for which higher scores indicate increased functioning: 
#"EuroQol-5D-3L", "LiSat-11", "WHQOL-BREF", "HFAQ", "MPI: GA",
#"SF-36 subscale Physical Functioning", "RAND-36 subscale Physical Functioning",
#"SF-36: mental health", "PSEQ", "SF-36: social functioning", "German Life Satisfaction Questionnaire".

ts_long$rev_score = 0
logical <- ts_long$instrument_name %in% measReverse
ts_long$rev_score[logical] = 1


#reverse scoring
ts_long$rev_score_m <- (ts_long$max_scale)-ts_long$m 

#combining rev_score and normal score on contion of ts_long$rev_score

ts_long$m_final <- ts_long$m

for (row in 1:nrow(ts_long)) {
  if (ts_long[row, "rev_score"] == 1 & !is.na(ts_long[row, "rev_score"])) {
    # replace value by ts_long$rev_score_m
    ts_long[row, "m_final"] <- ts_long[row, "rev_score_m"]
  }
}


#standardize mean
ts_long$m_stand = (ts_long$m_final/ts_long$max_scale)*100



#create plot
p <- ggplot(ts_long, aes(x=t, y=m_stand, group=cohort, color=cohort, text = 
                           paste("Author: ", author,
                                 "<br>Year: ", year,
                                 "<br>Cohort ID: ", cohort_id,
                                 "<br>Instrument: ", instrument_name,
                                 "<br>Raw mean: ", m,
                                 "<br>Raw SD: ", sd,
                                 "<br>Standardized score ", m_stand
                           ))) +
  labs(y= "outcome standardized (0-100)") +
  geom_line(data=ts_long[!is.na(ts_long$m_stand),]) + 
  geom_point() +
  scale_x_continuous(name = "Time (months)",
                     breaks = c(-20, 0, 3, 6, 12, 24, 60, 120),
                     labels = c("pre", "post", "3m", "6m", "12m", "24m", "60m", "120m"),
                     limits = c(-20, 200))

ggplotly(p, tooltip = "text") %>%
  layout(
    xaxis = list(
      tickvals = c(-20, 0, 3, 6, 12, 24, 60, 120),
      ticktext = c("pre", "post", "3m", "6m", "12m", "24m", "60m", "120m"),
      ticklen = 5,
      tickwidth = 2,
      tickcolor = toRGB("blue"),
      range=c(-22, 26)
    ),
    yaxis = list(
      range=c(0, 100))
  ) 
