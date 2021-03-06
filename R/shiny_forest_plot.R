#install packages
# install.packages('forestplot')
# install.packages('dplyr')
# install.packages('formattable')
# install.packages('magrittr')
# install.packages('tidyverse')
# install.packages('tidyr')
# install.packages('ggplot2')
# install.packages('tidyverse')
# install.packages('lessR')
# install.packages('plotly')
# install.packages('wesanderson')
# install.packages('PRISMAstatement')
# install.packages('psych')
# install.packages('shiny')
# install.packages('metafor')
# install.packages('forestplot')
# install.packages('meta')
# install.packages('readxl')
# install.packages('DiagrammeR')
# install.packages('data.table')
# install.packages('htmltools')
# install.packages('gsheet')


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
library(gsheet)

##Read Excel table from Google Sheet
#Input
url <- 'https://docs.google.com/spreadsheets/d/1DE08OpdFC2eH8VwAV-xLmsFByGh2VvL0DRDjPLhyo9U/edit?usp=sharing'

#read data
dat_gsheet <- gsheet2tbl(url) # keep dat_gsheet as in Google Sheets
dat <- dat_gsheet # We'll continue with dataset in variable 'dat'

##clean dataset
logical <- is.na(dat$author) | dat$author == "test" | dat$author == 0 # correct for incorrect authors
dat <- dat[!logical,]
dat[dat=="na"] <- NA
dat[dat=="Yes"] <- "yes"
dat[dat=="No"] <- "no"

# rename variables (could be done in Google Sheets?)
dat <- dat %>%
  rename(pf_name_measurement_instrument = pf_measurement_name,
         meas_hrqol = measurement_of_hrqol,
         meas_pf = measurements_physical_functioning,
         meas_pinter = measurements_pain_interference,
         meas_dep = measurements_depression,
         meas_anx = measurements_anxiety,
         meas_ef = measurements_general_emotional_functioning,
         meas_ang = measurements_anger,
         meas_se = measurements_self_efficacy,
         meas_srf = measurements_social_role_functioning,
         meas_pintens = measurements_pain_intensity)

# create variables 
dat$char_age_sd <- as.character(dat$age_sd)
dat$char_age_m <- as.character(dat$age_m)
dat$char_age_sd <- paste("(", dat$char_age_sd, ")", sep = "")
dat$age_m_sd <- paste(dat$char_age_m, dat$char_age_sd, sep = " ")
dat$age_m_sd[dat$age_m_sd=="NA (NA)"] <- NA
dat$char_year <- paste("(", dat$year, ")", sep="")
dat$author_year <- paste(dat$author, dat$char_year, sep = " ")

#calculate attrition
dat$attrition_post <- round(100-((dat$sample_size_post/dat$sample_size_pre)*100), 2)
dat$attrition_fu <- round(100-((dat$sample_size_fu/dat$sample_size_post)*100), 2)

#next lines should be corrected in Google Sheet
dat$sample_size_post[dat$author == "Olason" & dat$year == 2004] = NA # incorrect sample_size_post, Olason (2004)

#create variable cohort
dat$cohort <- paste(dat$author, dat$year, dat$cohort_id, sep = "_")

#get info from study with cohort_id > 1 from same study with cohort_id = 1
copyVarsFromCohort_id1 <- c("nationality","patient_group") # TO DO: variables need to be added
for (ind1 in which(dat$cohort_id > 1)) {
  #find same study (Author,year) but with cohort_id = 1
  ind2 <- which(dat$author == dat$author[ind1] & dat$year == dat$year[ind1]  & dat$cohort_id == 1)
  for (v in copyVarsFromCohort_id1) {
    eval(parse(text=paste('dat$', v, '[', ind1, ']=dat$', v, '[', ind2, ']', sep = "")))
  }
}

# meetinstrumenten
meetinstrumenten <- c("hrqol","pf","pinter","dep","anx","ef","ang","se","srf","pintens") # TO DO: outcomes need to be added

#determine last follow-up (last_fu)
for (mi in meetinstrumenten) {
  v1 <- paste(mi, '_last_fu', sep = "") 
  dat[v1] = 1 # initialise, <meetinstrument>_last_fu = 1
  
  # Was there even a second follow-up?
  v2 <- paste('meas', mi, sep = "_")
  v3 <- paste(mi, '_measurement_after_fu1', sep = "")
  ind <- which( dat[v2] == "yes" & dat[v3] == "yes" )
  dat[ind,v1] = 2
  
  # Was there even a third follow-up?
  v2 <- paste('meas', mi, sep = "_")
  v3 <- paste(mi, '_measurement_after_fu2', sep = "")
  ind <- which( dat[v2] == "yes" & dat[v3] == "yes" )
  dat[ind,v1] = 3
}

##check
#mi <- "pf"
#dat <- dat[dat$author == "Thieme",] 

#replace missing 'n' from outcomes (pre/post) with generic 'n'
for (mi in meetinstrumenten) {
  v1 <- paste('meas', mi, sep = "_")
  
  # pre/post
  for (p in c("pre","post")) {
    #if <meetinstrument>_<pre/post>_n is missing: <meetinstrument>_<pre/post>_n = sample_size_<pre/post>
    v2 <- paste(mi, p, 'n', sep = "_")
    v3 <- paste('sample_size', p, sep = "_")
    ind <- which(dat[v1] == "yes" & is.na(dat[v2]))
    if (length(ind) > 0) {
      dat[ind,v2] <- dat[ind,v3]
    }
    
    #if <meetinstrument>_<pre/post>_n is (still) missing: <meetinstrument>_<pre/post>_n = sample_size
    ind <- which(dat[v1] == "yes" & is.na(dat[v2]))
    if (length(ind) > 0) {
      dat[ind,v2] <- dat[ind,"sample_size"]
    }
    
    #if <meetinstrument>_<pre/post>_n is (still) missing: print warning 
    ind <- which(dat[v1] == "yes" & is.na(dat[v2]))
    if (length(ind) > 0) {
      print(paste('Sample sizes (sample_size_', p, ' and sample_size) are missing for study: ', dat$author[ind], ' (', dat$year[ind], ') - ', mi ,sep = ''))
    }
  }
  
  # follow-up (try to fill missings 'n' of the last follow-up with sample_size_fu)
  for (f in 1:3) {
    v1 <- paste(mi, '_fu', f ,'_n', sep = "")
    v2 <- paste(mi, '_last_fu', sep = "")
    ind <- which( is.na(dat[v1]) & dat[v2] == f )
    dat[ind,v1] <- dat$sample_size_fu[ind]
  }
  
  # follow-up (if still missing, get the general sample_size)
  for (f in 1:3) {
    v1 <- paste(mi, '_fu', f ,'_n', sep = "")
    v2 <- paste(mi, '_last_fu', sep = "")
    ind <- which( is.na(dat[v1]) & dat[v2] == f )
    dat[ind,v1] <- dat$sample_size[ind]
  }
}

## Forest plots
contrasts <- data.frame(left = c("pre","post","pre"), right = c("post","fu","fu"))

# remove data_long if it already exists from previous run
if (exists("data_long")) {
  rm(data_long)
}

for (mi in meetinstrumenten) {
  for (i in 1:dim(contrasts)[1]) {
    contrast <- paste(contrasts$left[i], contrasts$right[i], sep="-")
    
    # add <mi>_last_fu<m/n/sd> based on <mi>_fu<last_fu>
    v1 <- paste(mi, '_last_fu', sep = "") 
    for (ind in 1:dim(dat)[1]){
      for (w in c("m","n","sd","t")) {
        v2 <- paste(mi, '_fu', dat[ind,v1], '_', w, sep="")
        v3 <- paste(mi, '_last_fu_', w, sep="")
        dat[ind,v3] <- dat[ind,v2]     
      }
    }
    
    left <- contrasts$left[i]
    right <- contrasts$right[i]
    
    # rename 'fu' to 'last_fu'
    left <- str_replace(left,"fu","last_fu")
    right <- str_replace(right,"fu","last_fu")
    
    v1 <- paste('meas', mi, sep = "_")
    v2 <- paste(mi, left, 'm', sep = "_")
    v3 <- paste(mi, right, 'm', sep = "_")
    
    logical <- dat[v1] == "yes" & !is.na(dat[v2]) & !is.na(dat[v3])
    data_fp <- dat[logical,]
    
    #data_fp <- as_tibble(data_fp)   # dit is niet nodig?
    data_fp$fu_month <- 0
    data_fp$ri <- .54
    
    v1 <- paste(mi, right, 'm', sep = "_")  # m1i ??? Klopt originele volgorde wel?? m1i=m_post, m2i=m_pre, sd1i=sd_pre, ni=n_post 
    v2 <- paste(mi, left, 'm', sep = "_") # m2i
    v3 <- paste(mi, left, 'sd', sep = "_") # sd1i
    v4 <- paste(mi, right, 'n', sep = "_") # ni
    
    data_fp <- data_fp %>%
      mutate(m1i := eval(parse(text=v1)))
    data_fp <- data_fp %>%
      mutate(m2i := eval(parse(text=v2)))
    data_fp <- data_fp %>%
      mutate(sd1i := eval(parse(text=v3)))
    data_fp <- data_fp %>%
      mutate(ni := eval(parse(text=v4)))
    
    data_fp_meta <- escalc(measure="SMCR", m1i=m1i, m2i=m2i, sd1i=sd1i, ni=ni, ri=ri, data=data_fp)
    fp_meta <- metafor::summary.escalc(data_fp_meta)
    
    fp_meta$tabletext <- cbind(fp_meta$author, fp_meta$year, fp_meta$n_pre, fp_meta$measure, 
                               fp_meta$fu_month)
    
   
    
    ## make dataset to compare with Stefans Excel file
    check_var <- paste('check', mi, contrast, sep="_")
    
    v1 <- paste(mi, 'name_measurement_instrument',sep="_")
    v2 <- paste(mi, 'last_fu_t',sep="_")
    data_fp <- data_fp %>%
      mutate(name_measurement_instrument := eval(parse(text=v1)),
             fu_month := eval(parse(text=v2)))
    
    # left
    v <- paste(mi, left, 'n', sep = "_")
    data_fp <- data_fp %>% mutate(left_n := eval(parse(text=v)))
    v <- paste(mi, left, 'sd', sep = "_")
    data_fp <- data_fp %>% mutate(left_sd := eval(parse(text=v)))
    v <- paste(mi, left, 'm', sep = "_")
    data_fp <- data_fp %>% mutate(left_m := eval(parse(text=v)))
    
    # right
    v <- paste(mi, right, 'n', sep = "_")
    data_fp <- data_fp %>% mutate(right_n := eval(parse(text=v)))
    v <- paste(mi, right, 'sd', sep = "_")
    data_fp <- data_fp %>% mutate(right_sd := eval(parse(text=v)))
    v <- paste(mi, right, 'm', sep = "_")
    data_fp <- data_fp %>% mutate(right_m := eval(parse(text=v)))
    
    data_fp_sub <- data_fp %>%
      select(author, year, cohort_id, cohort_name, name_measurement_instrument, fu_month, left_m, left_sd, left_n, right_m, right_sd, right_n)
    
    # change 1 or 2 into "a" or "b"
    data_fp_sub$cohort_id <-letters[data_fp_sub$cohort_id]
    
    # add column with contrast in first column
    data_fp_sub <- add_column(data_fp_sub, contrast = contrast, .before = 1)
    
    # add column with outcome in first column
    data_fp_sub <- add_column(data_fp_sub, outcome = mi, .before = 1)
    
    # now we can compare the data in e.g. 'check_hrqol_pre-post' with Stefans Excel-sheet
    assign(check_var,data_fp_sub)
    
    ## Eerst had ik de code gemaakt om de verschillende forester plots te maken, later bleek dat je graag de tabel in 'long format' wilde,
    ## daarom zit dat er nu op deze manier in. Achteraf gezien had dit gedaan kunnen worden voordat je de forestplots gaat maken.
    
    # combine all 'check_'-variable into one 'long format' 
    ifelse(exists("data_long"), 
           data_long <- rbind(data_long,data_fp_sub),
           data_long <- data_fp_sub)
    
  }
}


# reversed scoring. Reverse the contrast in case higher scores indicate decreased functioning.  
## to be added? probably "other" >> "NHP: GLOBAL", "DPQ: dailty activities", "PCL (negative SE)", "No sick. leave days", "NRS (0-10), average pain"
## to be checked carefully  
measReverse <- c("FIQ", "NHP", "NHP: PA", "Norfunk (0-3)", "RDQ", "QBPDS",
                 "DRI (0-100)", "PDI", "ODI (0-1)", "MPI: pain interference",
                 "RMDQ", "ODI", "QBPRS", "DPQ: Daily activities", "LBPRS", "FRI",
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

data_long$rev_scoring = 0
logical <- data_long$name_measurement_instrument %in% measReverse
data_long$rev_scoring[logical] = 1



## reverse scoring procedure ##
data <- data_long
# return: new dataframe with corrected stds and means.
correct_rev_score <- function(data) {
  # Create new copy of data
  rev_corrected_data <- data.frame(data)
  
  # Iterate over each row in data
  for (row in 1:nrow(data)) {
    if (data[row, "rev_scoring"] == 1) {
      # Replace pre by post
      rev_corrected_data[row, "left_m"] <- data[row, "right_m"]
      rev_corrected_data[row, "left_sd"] <- data[row, "right_sd"]
      rev_corrected_data[row, "left_n"] <- data[row, "right_n"]
      
      # Replace post by pre
      rev_corrected_data[row, "right_m"] <- data[row, "left_m"]
      rev_corrected_data[row, "right_sd"] <- data[row, "left_sd"]
      rev_corrected_data[row, "right_n"] <- data[row, "left_n"]
    }
  }
  return(rev_corrected_data)
}
corrected_data <- correct_rev_score(data)

#rename outcomes
corrected_data$outcome <- corrected_data$outcome %>% recode(
  "hrqol" = "health related quality of life",
  "pf" = "physical function",
  "pinter" = "pain interference",
  "dep" =  "depression",
  "anx" = "anxiety",
  "ef" = "general emotional functioning",
  "ang" = "anger",
  "se" = "self-efficacy",
  "srf" = "social functioning",
  "pintens" = "pain intensity"
)

corrected_data <- corrected_data %>%
  mutate(fu_month = if_else(contrast == "pre-post", 0, fu_month))

## create shinyapp ##

#step1: create tibble for corrected data
data_fp <- as_tibble(corrected_data)

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
                  choices = c("health related quality of life", "physical function", "pain interference", "depression", "anxiety",   
                              "self-efficacy", "social functioning", "pain intensity", "anger", "general emotional functioning"),
                  selected = "pain interference"),
      
      selectInput(inputId = "contrast",
                  label = "select contrast:",
                  choices = c("pre-post", "post-fu", "pre-fu"),
                  selected = "pre-post")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput("forestplot", height = "1300px"), width = 12
      
    )
  )
)
# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  
  output$forestplot <- renderPlot({
    
    data_fp$ri <- input$r_value
    data_fp_meta <- escalc(measure="SMCR", m1i=right_m, m2i=left_m, sd1i=left_sd, ni=right_n, ri=ri, data=data_fp)
    fp_meta <- metafor::summary.escalc(data_fp_meta)
    fp_meta2 <- filter(fp_meta, contrast == input$contrast & outcome == input$outcome)
    fp_meta2$tabletext <- cbind(fp_meta2$author, fp_meta2$year, fp_meta2$right_n, fp_meta2$name_measurement_instrument, 
                                fp_meta2$fu_month)
    
    forestplot(fp_meta2$tabletext, fp_meta2$yi, fp_meta2$ci.lb, fp_meta2$ci.ub,
               xlab = "<---favors---     ---favors post--->",
               txt_gp=fpTxtGp(label=gpar(cex=1),
                              ticks=gpar(cex=.6),
                              xlab=gpar(cex = 1),
                              title=gpar(cex = 1.1)),
               col=fpColors(box="black", lines="black", zero = "gray50"),
               zero=0, cex=0.5, lineheight = unit(1, "cm"), boxsize=0.3,
               lwd.ci=2, ci.vertices=TRUE, ci.vertices.height = .1, grid=TRUE,
               align = "l",
               graph.pos = "right",
               clip = c(-4, 4),
               alim = c(-4,4)
    )
  })
  
}

shinyApp(ui=ui, server=server)


