# paste code below in markdown file stead of the code chunk, but load_data.R file should be located in the online environment under "~/R/..."
#source("R/load_data.R")


#Input
url <- 'https://docs.google.com/spreadsheets/d/1DE08OpdFC2eH8VwAV-xLmsFByGh2VvL0DRDjPLhyo9U/edit?usp=sharing'
url_rob <- 'https://docs.google.com/spreadsheets/d/1HLicE5rhMrt2IE3SnHxNr8o5ih_v3WKlKnjCBulgF5k/edit?usp=sharing'

##read main dataset
dat_gsheet <- gsheet2tbl(url) # keep dat_gsheet as in Google Sheets
dat <- dat_gsheet # We'll continue with dataset in variable 'dat'

dat_clean <- dat %>%
  dplyr::filter(!is.na(author) & !author == "test" & !author == 0)

## create cohort variable
dat_clean$cohort <- paste(dat_clean$author, dat_clean$year, dat_clean$cohort_id, sep = "_")

##clean dataset
logical <- is.na(dat$author) | dat$author == "test" | dat$author == 0 # correct for incorrect authors
dat <- dat[!logical,]
dat[dat=="na"] <- NA
dat[dat=="Yes"] <- "yes"
dat[dat=="No"] <- "no"

##check assessor names
unique(dat_clean$assessor)

##filter on double checked entries
dat_clean <- dat_clean %>% dplyr::filter(assessor == "SKSE" | assessor == "SEMK" | assessor == "UKSE" | assessor == "UKMK")

#ROB dataset

##read ROB data
dat_rob <- gsheet2tbl(url_rob) %>%
  dplyr::filter(assessor == "SKSE")

##check assessor names
unique(dat_rob$assessor)

## filter double checked entries
dat_rob <- dat_rob %>% dplyr::filter(assessor == "SKSE" | assessor == "SEMK")

##check double entries
table(dat_rob$`study id`)
table(dat_rob$author)
table(dat_rob$year)