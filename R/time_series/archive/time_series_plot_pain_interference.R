#Input
url <- 'https://docs.google.com/spreadsheets/d/1DE08OpdFC2eH8VwAV-xLmsFByGh2VvL0DRDjPLhyo9U/edit?usp=sharing'

#Install packages
#install.packages('gsheet')
#install.packages('dplyr')
#install.packages('tidyverse')
#install.packages('lessR')
install.packages("wesanderson")

#Load packages
library(gsheet)
library(dplyr)
library(formattable)
library(magrittr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(lessR)
library(plotly)
library(wesanderson)

#read data
dat <- gsheet2tbl(url)

#obtain colnames
datcolnames <- colnames(dat)
colnames <- as_tibble(datcolnames)


### PAIN INTENSITY ###


#create time_series datafile
ts <- select(dat, author, year, cohort_id, pintens_pre_m, pintens_pre_sd, pintens_post_m, pintens_post_sd,
             pintens_fu1_t, pintens_fu1_m, pintens_fu1_sd, pintens_fu2_t, pintens_fu2_m, 
             pintens_fu2_sd, pintens_fu3_t, pintens_fu3_m, pintens_fu3_sd, measurements_pain_intensity, 
             pintens_name_measurement_instrument) %>%
  filter(!is.na(author) & !author == "test" & !author == 0) %>%
  filter(measurements_pain_intensity == "Yes") %>%
  rename(time = pintens_fu1_t,
         instrument_name = pintens_name_measurement_instrument)

dat_clean <- dat %>%
  filter(!is.na(author) & !author == "test" & !author == 0)
  filter(measurements_pain_intensity == "Yes") 
  dat_clean$cohort <- paste(dat_clean$author, dat_clean$year, dat_clean$cohort_id, sep = "_")


ts_m <- select (dat_clean, cohort, author, year, cohort_id, pintens_pre_m, pintens_post_m, pintens_fu1_m,
                pintens_fu2_m, pintens_fu3_m, measurements_pain_intensity, pintens_name_measurement_instrument) %>%
  rename(instrument_name = pintens_name_measurement_instrument)

ts_sd <- select (dat_clean, cohort, pintens_pre_sd, pintens_post_sd, pintens_fu1_sd,
                 pintens_fu2_sd, pintens_fu3_sd)

ts_t <- select (dat_clean, cohort, pintens_fu1_t, pintens_fu2_t, pintens_fu3_t)
ts_t$pintens_pre_t = -20
ts_t$pintens_post_t = 0

#change SD columns to numeric
cols.num <- c("pintens_pre_sd","pintens_post_sd", "pintens_fu1_sd", "pintens_fu2_sd", "pintens_fu3_sd")
ts_sd[cols.num] <- sapply(ts_sd[cols.num],as.numeric)
sapply(ts_sd, class)


#convert to long dataset
# The arguments to gather():
# - data: Data object
# - key: Name of new key column (made from names of data columns)
# - value: Name of new value column
# - ...: Names of source columns that contain values
# - factor_key: Treat the new key column as a factor (instead of character vector)
ts_m_long <- pivot_longer(ts_m, 
                        cols = c("pintens_pre_m", "pintens_post_m", "pintens_fu1_m", "pintens_fu2_m", "pintens_fu2_m", "pintens_fu3_m"),
                        names_to = "timepoint",
                        values_to = "pintens_m")

ts_sd_long <- pivot_longer(ts_sd, 
                          cols = c("pintens_pre_sd", "pintens_post_sd", "pintens_fu1_sd", "pintens_fu2_sd", "pintens_fu2_sd", "pintens_fu3_sd"),
                          names_to = "timepoint",
                          values_to = "pintens_sd")



ts_t_long <- pivot_longer(ts_t,
                          cols = c("pintens_pre_t", "pintens_post_t", "pintens_fu1_t", "pintens_fu2_t", "pintens_fu2_t", "pintens_fu3_t"),
                          names_to = "timepoint",
                          values_to = "pintens_t")


#merge long datasets
ts_sd_long$timepoint <-  gsub("\\_sd*$","",ts_sd_long$timepoint)
ts_t_long$timepoint <- gsub("\\_t*$","",ts_t_long$timepoint)
ts_m_long$timepoint <- gsub("\\_m*$","",ts_m_long$timepoint)

ts_sdt_long <- merge(ts_sd_long, ts_t_long, c("cohort", "timepoint"))
ts_long <- merge(ts_sdt_long, ts_m_long, c("cohort", "timepoint"))


#create max_score column

ts_long$max_scale = ts_long$instrument_name
ts_long$max_scale <- recode(ts_long$max_scale, "VAS" = 100,
                            "PRI" = 78,
                            "MPI: pain severity" = 7,
                            "NRS" = 10,
                            "NPRS" = 10,
                            "NRS (0-10)" = 10,
                            "VAS (0-10)"= 10,
                            "Likert pain intensity" = 6)

ts_long$pintens_stand = (ts_long$pintens_m/ts_long$max_scale)*100

#create plot
p <- ggplot(ts_long, aes(x=pintens_t, y=pintens_stand, group=cohort, color=cohort, text = 
                           paste("Author: ", author,
                                 "<br>Year: ", year,
                                 "<br>Cohort ID: ", cohort_id,
                                 "<br>Instrument: ", instrument_name,
                                 "<br>Raw mean: ", pintens_m,
                                 "<br>Raw SD: ", pintens_sd,
                                 "<br>Standardized score ", pintens_stand
                                 ))) +
  labs(y= "Pain intensity (standardized 0-100)") +
  geom_line(data=ts_long[!is.na(ts_long$pintens_stand),]) + 
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

#create matching table
ts_present <- select(dat, author, year, number_of_cohorts, cohort_id, cohort_name, pintens_name_measurement_instrument, pintens_pre_m, 
                     pintens_pre_sd, pintens_post_m, pintens_post_sd, pintens_fu1_m, pintens_fu1_sd, 
                     pintens_fu2_m, pintens_fu2_sd, pintens_fu3_m, pintens_fu3_sd, measurements_pain_intensity) %>%
  filter(!is.na(author) & !author == "test" & !author == 0) %>%
  filter(measurements_pain_intensity == "Yes")

formattable(ts_present, 
            align = c("r","r")
            )


### PAIN INTERFERENCE ###
