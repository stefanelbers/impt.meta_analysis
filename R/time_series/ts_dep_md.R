#subset dataset with measurements on pain_interference

dat_dep <- dat_clean %>%
  dplyr::filter(measurements_depression == "Yes")  

#create dataframe for mean values over time    
ts_m <- select (dat_dep, cohort, author, year, cohort_id, dep_pre_m, dep_post_m, dep_fu1_m,
                dep_fu2_m, dep_fu3_m, measurements_depression, dep_name_measurement_instrument) %>%
  rename(instrument_name = dep_name_measurement_instrument, 
         pre_m = dep_pre_m,
         post_m = dep_post_m,
         fu1_m = dep_fu1_m,
         fu2_m = dep_fu2_m,
         fu3_m = dep_fu3_m,
         instrument_present = measurements_depression)
 
#create dataframe for SD values over time and change columns to numeric   
ts_sd <- select (dat_dep, cohort, dep_pre_sd, dep_post_sd, dep_fu1_sd,
                 dep_fu2_sd, dep_fu3_sd) %>%
  rename( pre_sd = dep_pre_sd,
          post_sd = dep_post_sd,
          fu1_sd = dep_fu1_sd,
          fu2_sd = dep_fu2_sd,
          fu3_sd = dep_fu3_sd)

cols.num <- c("pre_sd","post_sd", "fu1_sd", "fu2_sd", "fu3_sd")
ts_sd[cols.num] <- sapply(ts_sd[cols.num],as.numeric)

#create dataframe for time and add pre/post values  
ts_t <- select (dat_dep, cohort, dep_fu1_t, dep_fu2_t, dep_fu3_t) %>%
  rename( fu1_t = dep_fu1_t,
          fu2_t = dep_fu2_t,
          fu3_t = dep_fu3_t)

ts_t$pre_t = -20
ts_t$post_t = 0

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

#create max_score column
ts_long$max_scale <- as.numeric(questionnaires$max_scale)[match(as.character(ts_long$instrument_name), as.character(questionnaires$q))]

#create reverse scoring variable
ts_long$rev_score <- questionnaires$reverse_scoring[match(as.character(ts_long$instrument_name), as.character(questionnaires$q))]

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
