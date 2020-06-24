#library
library(forestplot)
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
library(PRISMAstatement)
library(psych)
library(shiny)
library(metafor)
library(forestplot)
library(meta)
library(readxl)
library(DiagrammeR)
library(data.table)
library(htmltools)


#Input
url <- 'https://docs.google.com/spreadsheets/d/1DE08OpdFC2eH8VwAV-xLmsFByGh2VvL0DRDjPLhyo9U/edit?usp=sharing'

#read data
dat <- gsheet2tbl(url)

### PHYSICAL FUNCTIONING ###

#create time_series datafile
dat_clean <- dat %>%
  filter(!is.na(author) & !author == "test" & !author == 0)
dat_clean$cohort <- paste(dat_clean$author, dat_clean$year, dat_clean$cohort_id, sep = "_")


#data pain interference
dat_pinter <- dat_clean %>%
  filter(measurements_pain_interference == "Yes")

#create a data_frame that contains the pre-post contrasts for pain interference

dat_pinter_pp <- select(dat_pinter, cohort, author, year, cohort_id, cohort_name, pinter_name_measurement_instrument,
                        pinter_pre_n, pinter_pre_m, pinter_pre_sd, pinter_post_n, pinter_post_m, pinter_post_sd) %>%
  rename(measure = pinter_name_measurement_instrument,
         m_pre = pinter_pre_m,
         sd_pre = pinter_pre_sd,
         n_pre = pinter_pre_n,
         m_post = pinter_post_m,
         sd_post = pinter_post_sd,
         n_post = pinter_post_n)

dat_pinter_pp$outcome <- "pain interference"
dat_pinter_pp$contrast <- "pre-post"
dat_pinter_pp$fu_month <- 0


#test fp
data_fp <- as_tibble(dat_pinter_pp)

data_fp$ri <- .54
data_fp_meta <- escalc(measure="SMCR", m1i=m_post, m2i=m_pre, sd1i=sd_pre, ni=n_post, ri=ri, data=data_fp)
fp_meta <- metafor::summary.escalc(data_fp_meta)
fp_meta2 <- filter(fp_meta, contrast == "pre-post" & outcome == "pain interference")
res <- rma(yi, vi, data=fp_meta2)
fp_meta2$tabletext <- cbind(fp_meta2$author, fp_meta2$year, fp_meta2$n_pre, fp_meta2$measure, 
                            fp_meta2$fu_month)

forestplot(fp_meta2$tabletext, fp_meta2$yi, fp_meta2$ci.lb, fp_meta2$ci.ub,
           zero = 0,
           cex = 2,
           lineheight = "auto",
           xlab = "SMD",
           lwd.ci= 3,
           col = fpColors(text="black", lines="#4393C3", box="#2166AC"),
           boxsize = .25
)


