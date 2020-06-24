#install packages
install.packages("metafor")
install.packages("tidyverse")
install.packages("forestplot")
install.packages("ggplot2")
install.packages("meta")
install.packages("shiny")
install.packages("expss")
install.packages("DiagrammeR")
install.packages("PRISMAstatement")
install.packages("formattable")
install.packages("data.table")
install.packages('htmltools')
install.packages('webshot')
install.packages('rsconnect')

#load packages
library(metafor)
library(tidyverse)
library(forestplot)
library(meta)
library(shiny)
library(ggplot2)
library(expss)
library(readxl)
library(DiagrammeR)
library(PRISMAstatement)
library(formattable)
library(dplyr)
library(data.table)
library(htmltools)
library(webshot)


#step1: import dataset from excel file
data_hrqol <- read_excel("data.raw/20200123.r_data_forest_plot.xlsx")
data_hrqol <- as_tibble(data_hrqol)

#step 2: create Shinyapp
ui <- fluidPage(
  
  # App title ----
  headerPanel("Forest plot systematic review"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "r_value",
                  label = "R Correction Value:",
                  min = 0,
                  max = 1,
                  value = 0,
                  step = 0.1),
      
      selectInput(inputId = "outcome",
                  label = "select ouctome:",
                  choices = c("hr quality of life", "physical function", "pain interference", "depression", "anxiety",   
                              "self-efficacy", "social functioning", "pain intensity"),
                  selected = "pain interference"),
      
      selectInput(inputId = "contrast",
                  label = "select contrast:",
                  choices = c("pre-post", "post-fu", "pre-fu"),
                  selected = "pre-post")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput("forestplot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  
  output$forestplot <- renderPlot({
    
    data_hrqol$ri <- input$r_value
    data_hrqol_meta <- escalc(measure="SMCR", m1i=m_post, m2i=m_pre, sd1i=sd_pre, ni=n_post, ri=ri, data=data_hrqol)
    hrqol_meta <- metafor::summary.escalc(data_hrqol_meta)
    hrqol_meta2 <- filter(hrqol_meta, contrast == input$contrast & outcome == input$outcome)
    hrqol_meta2$tabletext <- cbind(hrqol_meta2$author, hrqol_meta2$year, hrqol_meta2$n_pre, hrqol_meta2$measure, 
                                   hrqol_meta2$fu_month)
    
    forestplot(hrqol_meta2$tabletext, hrqol_meta2$yi, hrqol_meta2$ci.lb, hrqol_meta2$ci.ub,
               zero = 0,
               cex = 2,
               lineheight = "auto",
               xlab = "SMD",
               lwd.ci= 3,
               col = fpColors(text="black", lines="#4393C3", box="#2166AC"),
               boxsize = .25
    )
  })
  
}

shinyApp(ui=ui, server=server)


#change Google form input to core outcome dataset.
#Input
url <- 'https://docs.google.com/spreadsheets/d/1DE08OpdFC2eH8VwAV-xLmsFByGh2VvL0DRDjPLhyo9U/edit?usp=sharing'

#Load packages
install.packages('gsheet')
install.packages('dplyr')
library(gsheet)
library(dplyr)

#read data
dat <- gsheet2tbl(url)


View(dat)

datcolnames <- colnames(dat)

dat1 <- data.frame(
  dplyr::select(dat,"Study ID", "Author", "Year", "Cohort", "Brief name of this intervention cohort" "Study design", "Sample size (pre)", "Sample size (post)", "Sample size (follow-up)", "Nationality", "Female gender",  "Age (years)", "SD Age", "Patient group", "Minimum pain duration", "Referral", "Pain duration (months)", "SD Pain duration", "Treatment aim", "Treatment modalities", "Healthcare providers", "In/outpatient", "Type of contact", "Setting", "Time span", "Hours", "Minutes", "Tailoring")
)
colnames(dat1) <- c("id", "author", "year", "cohort", "cohort_name", "design", "ss_pre", "ss_post", "ss_fu", "nationality", "%f", "age", "sd_age", "patient_group", "min_pain_duration", "referral", "mean_pain_duration", "sd_pain_duration", "treatment_aim", "treatment_modalities", "hcp", "in/outpatient", "contact", "setting", "time_span", "hours", "minutes", "tailoring")

#filter variables for outcome dataset.
#add to dat2: "Cohort", What is the name of the measurement instrument?", 
dat1 <- as_tibble(dat)
dat_pain_interference <- data.frame(
    id=dplyr::select(dat1,"Study ID"),
    author=dplyr::select(dat1,"Author"),
    year=dplyr::select(dat1,"Year")
    )
#cohort=dplyr::select(dat1, "cohort"),
#TO DO: iterate over outcome domain!!
outcome_domain()
  
  dplyr::select(dat1, "Study ID", "Author", "Year", "Brief name of this intervention cohort", "please list all outcomes from this study", 
                      "rev_scoring", "N (pre)", "Mean (pre)", "SD (pre)", "N (post)", "Mean (post)", "SD (post)")



Table1
variables <- c("Author","study design","sample size (pre)","sample size (post)","sample size (latest follow-up)","Population description (nationality)","%female","Mean age","SD age","patient inclusion criteria","Pain duration in months (baseline)")
tbl1 <- dat[, variables]

Table2
row_ind <- which(tbl1$Author == "Bendix")
regel <- tbl1[tbl1$Author == "Bendix",]


#outcome_names

# Mees Stefan HUckaton

library(readxl)


data <- read_excel("data.raw/20200123.r_data_forest_plot.xlsx")
# return: new dataframe with corrected stds and means.
correct_rev_score <- function(data) {
  # Create new copy of data
  rev_corrected_data <- data.frame(data)
  
  # Iterate over each row in data
  for (row in 1:nrow(data)) {
    if (data[row, "rev_scoring"] == 1) {
      # Replace pre by post
      rev_corrected_data[row, "m_pre"] <- data[row, "m_post"]
      rev_corrected_data[row, "sd_pre"] <- data[row, "sd_post"]
      rev_corrected_data[row, "n_pre"] <- data[row, "n_post"]
      
      # Replace post by pre
      rev_corrected_data[row, "m_post"] <- data[row, "m_pre"]
      rev_corrected_data[row, "sd_post"] <- data[row, "sd_pre"]
      rev_corrected_data[row, "n_post"] <- data[row, "n_pre"]
    }
  }
  return(rev_corrected_data)
}
corrected_data <- correct_rev_score(data)


maxscore_mapping <- read_excel("data.raw/range_instruments.xlsx")
broad_data <- read_excel("data.raw/format_core_dataset.xlsx")
# fetch the maxscore of a measurement_instrument
get_maxscore <- function(measurement, maxscore_mapping) {
  return(maxscore_mapping[tolower(maxscore_mapping$measure_instrument)==tolower(measurement), ]$maxscore)
}
append_long_row <- function(data, o, a, y, me, r, t, m, sd, n, max) {
  new_row <- data.frame(
    outcome = o, 
    author = a, 
    year = y, 
    measurement_instrument = me,
    rev_scoring = r, 
    time = t,
    m = m,
    sd = sd,
    n = n,
    maxscore = max
  )
  return(rbind(data, new_row))
}
# return: new dataframe for timeseries plot
get_long_data <- function(data, maxscore_mapping) {
  # initialize new empty dataframe
  long_data <- data.frame(
    outcome = character(0), 
    author = character(0), 
    year = numeric(0),
    measurement_instrument = character(0),
    rev_scoring = numeric(0),
    time = numeric(0), 
    m = numeric(0), 
    sd = numeric(0), 
    n = numeric(0), 
    maxscore = numeric(0)
  )
  for (row in 1: nrow(data)) {
    broad_row <- data[row, ]
    maxscore <- get_maxscore(broad_row$measurement_instrument, maxscore_mapping)
    
    # Add rows for outcome: hrqol
    long_data <- append_long_row(long_data, "hrqol", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring, 
                                 -1, broad_row$hrqol_pre_m, broad_row$hrqol_pre_sd, broad_row$hrqol_pre_n, maxscore)
    if (!is.na(broad_row$post_n)) {
      long_data <- append_long_row(long_data, "hrqol", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   0, broad_row$hrqol_post_m, broad_row$hrqol_post_sd, broad_row$hrqol_post_n, maxscore)
    }
    if (!is.na(broad_row$fu1_n)) {
      long_data <- append_long_row(long_data, "hrqol", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   broad_row$hrqol_fu1_t, broad_row$hrqol_fu1_m, broad_row$hrqol_fu1_sd, broad_row$hrqol_fu1_n, maxscore)
    }
    if (!is.na(broad_row$fu2_n)) {
      long_data <- append_long_row(long_data, "hrqol", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   broad_row$hrqol_fu2_t, broad_row$hrqol_fu2_m, broad_row$hrqol_fu2_sd, broad_row$hrqol_fu2_n, maxscore)
    }
    if (!is.na(broad_row$fu3_n)) {
      long_data <- append_long_row(long_data, "hrqol", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   broad_row$hrqol_fu3_t, broad_row$hrqol_fu3_m, broad_row$hrqol_fu3_sd, broad_row$hrqol_fu3_n, maxscore)
    }
    if (!is.na(broad_row$fu4_n)) {
      long_data <- append_long_row(long_data, "hrqol", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   broad_row$hrqol_fu4_t, broad_row$hrqol_fu4_m, broad_row$hrqol_fu4_sd, broad_row$hrqol_fu4_n, maxscore)
    }
    # Add rows for outcome: pf
    long_data <- append_long_row(long_data, "pf", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring, 
                                 -1, broad_row$pf_pre_m, broad_row$pf_pre_sd, broad_row$pf_pre_n, maxscore)
    if (!is.na(broad_row$post_n)) {
      long_data <- append_long_row(long_data, "pf", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   0, broad_row$pf_post_m, broad_row$pf_post_sd, broad_row$pf_post_n, maxscore)
    }
    if (!is.na(broad_row$fu1_n)) {
      long_data <- append_long_row(long_data, "pf", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   broad_row$pf_fu1_t, broad_row$pf_fu1_m, broad_row$pf_fu1_sd, broad_row$pf_fu1_n, maxscore)
    }
    if (!is.na(broad_row$fu2_n)) {
      long_data <- append_long_row(long_data, "pf", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   broad_row$pf_fu2_t, broad_row$pf_fu2_m, broad_row$pf_fu2_sd, broad_row$pf_fu2_n, maxscore)
    }
    if (!is.na(broad_row$fu3_n)) {
      long_data <- append_long_row(long_data, "pf", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   broad_row$pf_fu3_t, broad_row$pf_fu3_m, broad_row$pf_fu3_sd, broad_row$pf_fu3_n, maxscore)
    }
    if (!is.na(broad_row$fu4_n)) {
      long_data <- append_long_row(long_data, "pf", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   broad_row$pf_fu4_t, broad_row$pf_fu4_m, broad_row$pf_fu4_sd, broad_row$pf_fu4_n, maxscore)
    }
    # Add rows for outcome: pinter
    long_data <- append_long_row(long_data, "pinter", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring, 
                                 -1, broad_row$pinter_pre_m, broad_row$pinter_pre_sd, broad_row$pinter_pre_n, maxscore)
    if (!is.na(broad_row$post_n)) {
      long_data <- append_long_row(long_data, "pinter", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   0, broad_row$pinter_post_m, broad_row$pinter_post_sd, broad_row$pinter_post_n, maxscore)
    }
    if (!is.na(broad_row$fu1_n)) {
      long_data <- append_long_row(long_data, "pinter", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   broad_row$pinter_fu1_t, broad_row$pinter_fu1_m, broad_row$pinter_fu1_sd, broad_row$pinter_fu1_n, maxscore)
    }
    if (!is.na(broad_row$fu2_n)) {
      long_data <- append_long_row(long_data, "pinter", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   broad_row$pinter_fu2_t, broad_row$pinter_fu2_m, broad_row$pinter_fu2_sd, broad_row$pinter_fu2_n, maxscore)
    }
    if (!is.na(broad_row$fu3_n)) {
      long_data <- append_long_row(long_data, "pinter", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   broad_row$pinter_fu3_t, broad_row$pinter_fu3_m, broad_row$pinter_fu3_sd, broad_row$pinter_fu3_n, maxscore)
    }
    if (!is.na(broad_row$fu4_n)) {
      long_data <- append_long_row(long_data, "pinter", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   broad_row$pinter_fu4_t, broad_row$pinter_fu4_m, broad_row$pinter_fu4_sd, broad_row$pinter_fu4_n, maxscore)
    }
    # Add rows for outcome: dep
    long_data <- append_long_row(long_data, "dep", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring, 
                                 -1, broad_row$dep_pre_m, broad_row$dep_pre_sd, broad_row$dep_pre_n, maxscore)
    if (!is.na(broad_row$post_n)) {
      long_data <- append_long_row(long_data, "dep", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   0, broad_row$dep_post_m, broad_row$dep_post_sd, broad_row$dep_post_n, maxscore)
    }
    if (!is.na(broad_row$fu1_n)) {
      long_data <- append_long_row(long_data, "dep", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   broad_row$dep_fu1_t, broad_row$dep_fu1_m, broad_row$dep_fu1_sd, broad_row$dep_fu1_n, maxscore)
    }
    if (!is.na(broad_row$fu2_n)) {
      long_data <- append_long_row(long_data, "dep", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   broad_row$dep_fu2_t, broad_row$dep_fu2_m, broad_row$dep_fu2_sd, broad_row$dep_fu2_n, maxscore)
    }
    if (!is.na(broad_row$fu3_n)) {
      long_data <- append_long_row(long_data, "dep", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   broad_row$dep_fu3_t, broad_row$dep_fu3_m, broad_row$dep_fu3_sd, broad_row$dep_fu3_n, maxscore)
    }
    if (!is.na(broad_row$fu4_n)) {
      long_data <- append_long_row(long_data, "dep", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   broad_row$dep_fu4_t, broad_row$dep_fu4_m, broad_row$dep_fu4_sd, broad_row$dep_fu4_n, maxscore)
    }
    # Add rows for outcome: anx
    long_data <- append_long_row(long_data, "anx", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring, 
                                 -1, broad_row$anx_pre_m, broad_row$anx_pre_sd, broad_row$anx_pre_n, maxscore)
    if (!is.na(broad_row$post_n)) {
      long_data <- append_long_row(long_data, "anx", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   0, broad_row$anx_post_m, broad_row$anx_post_sd, broad_row$anx_post_n, maxscore)
    }
    if (!is.na(broad_row$fu1_n)) {
      long_data <- append_long_row(long_data, "anx", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   broad_row$anx_fu1_t, broad_row$anx_fu1_m, broad_row$anx_fu1_sd, broad_row$anx_fu1_n, maxscore)
    }
    if (!is.na(broad_row$fu2_n)) {
      long_data <- append_long_row(long_data, "anx", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   broad_row$anx_fu2_t, broad_row$anx_fu2_m, broad_row$anx_fu2_sd, broad_row$anx_fu2_n, maxscore)
    }
    if (!is.na(broad_row$fu3_n)) {
      long_data <- append_long_row(long_data, "anx", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   broad_row$anx_fu3_t, broad_row$anx_fu3_m, broad_row$anx_fu3_sd, broad_row$anx_fu3_n, maxscore)
    }
    if (!is.na(broad_row$fu4_n)) {
      long_data <- append_long_row(long_data, "anx", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   broad_row$anx_fu4_t, broad_row$anx_fu4_m, broad_row$anx_fu4_sd, broad_row$anx_fu4_n, maxscore)
    }
    # Add rows for outcome: ef
    long_data <- append_long_row(long_data, "ef", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring, 
                                 -1, broad_row$ef_pre_m, broad_row$ef_pre_sd, broad_row$ef_pre_n, maxscore)
    if (!is.na(broad_row$post_n)) {
      long_data <- append_long_row(long_data, "ef", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   0, broad_row$ef_post_m, broad_row$ef_post_sd, broad_row$ef_post_n, maxscore)
    }
    if (!is.na(broad_row$fu1_n)) {
      long_data <- append_long_row(long_data, "ef", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   broad_row$ef_fu1_t, broad_row$ef_fu1_m, broad_row$ef_fu1_sd, broad_row$ef_fu1_n, maxscore)
    }
    if (!is.na(broad_row$fu2_n)) {
      long_data <- append_long_row(long_data, "ef", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   broad_row$ef_fu2_t, broad_row$ef_fu2_m, broad_row$ef_fu2_sd, broad_row$ef_fu2_n, maxscore)
    }
    if (!is.na(broad_row$fu3_n)) {
      long_data <- append_long_row(long_data, "ef", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   broad_row$ef_fu3_t, broad_row$ef_fu3_m, broad_row$ef_fu3_sd, broad_row$ef_fu3_n, maxscore)
    }
    if (!is.na(broad_row$fu4_n)) {
      long_data <- append_long_row(long_data, "ef", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   broad_row$ef_fu4_t, broad_row$ef_fu4_m, broad_row$ef_fu4_sd, broad_row$ef_fu4_n, maxscore)
    }
    # Add rows for outcome: ang
    long_data <- append_long_row(long_data, "ang", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring, 
                                 -1, broad_row$ang_pre_m, broad_row$ang_pre_sd, broad_row$ang_pre_n, maxscore)
    if (!is.na(broad_row$post_n)) {
      long_data <- append_long_row(long_data, "ang", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   0, broad_row$ang_post_m, broad_row$ang_post_sd, broad_row$ang_post_n, maxscore)
    }
    if (!is.na(broad_row$fu1_n)) {
      long_data <- append_long_row(long_data, "ang", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   broad_row$ang_fu1_t, broad_row$ang_fu1_m, broad_row$ang_fu1_sd, broad_row$ang_fu1_n, maxscore)
    }
    if (!is.na(broad_row$fu2_n)) {
      long_data <- append_long_row(long_data, "ang", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   broad_row$ang_fu2_t, broad_row$ang_fu2_m, broad_row$ang_fu2_sd, broad_row$ang_fu2_n, maxscore)
    }
    if (!is.na(broad_row$fu3_n)) {
      long_data <- append_long_row(long_data, "ang", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   broad_row$ang_fu3_t, broad_row$ang_fu3_m, broad_row$ang_fu3_sd, broad_row$ang_fu3_n, maxscore)
    }
    if (!is.na(broad_row$fu4_n)) {
      long_data <- append_long_row(long_data, "ang", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   broad_row$ang_fu4_t, broad_row$ang_fu4_m, broad_row$ang_fu4_sd, broad_row$ang_fu4_n, maxscore)
    }
    # Add rows for outcome: se
    long_data <- append_long_row(long_data, "se", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring, 
                                 -1, broad_row$se_pre_m, broad_row$se_pre_sd, broad_row$se_pre_n, maxscore)
    if (!is.na(broad_row$post_n)) {
      long_data <- append_long_row(long_data, "se", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   0, broad_row$se_post_m, broad_row$se_post_sd, broad_row$se_post_n, maxscore)
    }
    if (!is.na(broad_row$fu1_n)) {
      long_data <- append_long_row(long_data, "se", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   broad_row$se_fu1_t, broad_row$se_fu1_m, broad_row$se_fu1_sd, broad_row$se_fu1_n, maxscore)
    }
    if (!is.na(broad_row$fu2_n)) {
      long_data <- append_long_row(long_data, "se", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   broad_row$se_fu2_t, broad_row$se_fu2_m, broad_row$se_fu2_sd, broad_row$se_fu2_n, maxscore)
    }
    if (!is.na(broad_row$fu3_n)) {
      long_data <- append_long_row(long_data, "se", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   broad_row$se_fu3_t, broad_row$se_fu3_m, broad_row$se_fu3_sd, broad_row$se_fu3_n, maxscore)
    }
    if (!is.na(broad_row$fu4_n)) {
      long_data <- append_long_row(long_data, "se", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   broad_row$se_fu4_t, broad_row$se_fu4_m, broad_row$se_fu4_sd, broad_row$se_fu4_n, maxscore)
    }
    # Add rows for outcome: srf
    long_data <- append_long_row(long_data, "srf", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring, 
                                 -1, broad_row$srf_pre_m, broad_row$srf_pre_sd, broad_row$srf_pre_n, maxscore)
    if (!is.na(broad_row$post_n)) {
      long_data <- append_long_row(long_data, "srf", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   0, broad_row$srf_post_m, broad_row$srf_post_sd, broad_row$srf_post_n, maxscore)
    }
    if (!is.na(broad_row$fu1_n)) {
      long_data <- append_long_row(long_data, "srf", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   broad_row$srf_fu1_t, broad_row$srf_fu1_m, broad_row$srf_fu1_sd, broad_row$srf_fu1_n, maxscore)
    }
    if (!is.na(broad_row$fu2_n)) {
      long_data <- append_long_row(long_data, "srf", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   broad_row$srf_fu2_t, broad_row$srf_fu2_m, broad_row$srf_fu2_sd, broad_row$srf_fu2_n, maxscore)
    }
    if (!is.na(broad_row$fu3_n)) {
      long_data <- append_long_row(long_data, "srf", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   broad_row$srf_fu3_t, broad_row$srf_fu3_m, broad_row$srf_fu3_sd, broad_row$srf_fu3_n, maxscore)
    }
    if (!is.na(broad_row$fu4_n)) {
      long_data <- append_long_row(long_data, "srf", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   broad_row$srf_fu4_t, broad_row$srf_fu4_m, broad_row$srf_fu4_sd, broad_row$srf_fu4_n, maxscore)
    }
    # Add rows for outcome: pintens
    long_data <- append_long_row(long_data, "pintens", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring, 
                                 -1, broad_row$pintens_pre_m, broad_row$pintens_pre_sd, broad_row$pintens_pre_n, maxscore)
    if (!is.na(broad_row$post_n)) {
      long_data <- append_long_row(long_data, "pintens", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   0, broad_row$pintens_post_m, broad_row$pintens_post_sd, broad_row$pintens_post_n, maxscore)
    }
    if (!is.na(broad_row$fu1_n)) {
      long_data <- append_long_row(long_data, "pintens", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   broad_row$pintens_fu1_t, broad_row$pintens_fu1_m, broad_row$pintens_fu1_sd, broad_row$pintens_fu1_n, maxscore)
    }
    if (!is.na(broad_row$fu2_n)) {
      long_data <- append_long_row(long_data, "pintens", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   broad_row$pintens_fu2_t, broad_row$pintens_fu2_m, broad_row$pintens_fu2_sd, broad_row$pintens_fu2_n, maxscore)
    }
    if (!is.na(broad_row$fu3_n)) {
      long_data <- append_long_row(long_data, "pintens", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   broad_row$pintens_fu3_t, broad_row$pintens_fu3_m, broad_row$pintens_fu3_sd, broad_row$pintens_fu3_n, maxscore)
    }
    if (!is.na(broad_row$fu4_n)) {
      long_data <- append_long_row(long_data, "pintens", broad_row$author, broad_row$year, broad_row$measurement_instrument, broad_row$rev_scoring,
                                   broad_row$pintens_fu4_t, broad_row$pintens_fu4_m, broad_row$pintens_fu4_sd, broad_row$pintens_fu4_n, maxscore)
    }
  }
  return(long_data)
}
long_data <- get_long_data(broad_data, maxscore_mapping)


