
#load packages
library(dplyr)
library(formattable)
library(magrittr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(plotly)
library(readxl)
library(gsheet)
library(DT)
library(reactable)
library(readr)

#obtain dataset Risk of Bias

#Input
url_rob <- 'https://docs.google.com/spreadsheets/d/1HLicE5rhMrt2IE3SnHxNr8o5ih_v3WKlKnjCBulgF5k/edit?usp=sharing'

#read data
dat <- gsheet2tbl(url_rob) %>%
  filter(assessor == "SKSE")

#create table
t_rob <- select(dat, `study id`, author, year, 6:17) %>%
  

reactable(t_rob,
          defaultSorted = "author",
          defaultSortOrder = "asc",
          defaultColGroup = colGroup(headerClass = "header"),
          style = list(fontFamily = "Arial Narrow", fontSize = "14px"),
          defaultColDef = colDef(
            header = function(value) gsub("_", " ", value, fixed = TRUE),
            cell = function(value) format(value, nsmall = 0),
            align = "left",
            minWidth = 70,
            headerStyle = list(background = "#f7f7f8")
          ),
          columns = list(
            `study id` = colDef(minWidth = 50),
            author = colDef(minWidth = 80),
            year = colDef(minWidth = 50)
          ),
          bordered = FALSE,
          highlight = FALSE,
          striped = FALSE,
          searchable = TRUE,
          showPageSizeOptions = TRUE,
          onClick = "expand"
)


