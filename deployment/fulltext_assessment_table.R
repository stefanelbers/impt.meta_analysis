install.packages("revtools")
library(revtools)
library(tidyverse)

data_fulltext <- read_bibliography("deployment/testset2.txt")

t_fulltext <- select(data_fulltext, author, year, title, LB, n1)


#create decision column
t_fulltext$decision = 0

#rename columns
names(t_fulltext)[names(t_fulltext) == "LB"] <- "study id"
names(t_fulltext)[names(t_fulltext) == "n1"] <- "comment"

#create vector with study ids of included studies
included_id <- c(1, 9, 12, 14, 16, 30, 36, 46, 48, 51, 55, 58, 69, 90, 92, 96, 101, 102, 110, 113, 119, 123,
              132, 133, 134, 138, 139, 144, 147, 148, 149, 152, 154, 164, 166, 167, 180, 181, 183, 184, 185, 
              204, 211, 212, 223, 228, 246, 266, 269, 271, 277, 284, 290, 303, 306, 317, 325, 335, 340, 350,
              354, 356, 360, 364, 375
              )

#replace decision values with 1 if study id matches vector included studies
t_fulltext$decision[t_fulltext$`study id`%in% included_id] <- 1

#validate if number of 1 matches number of included studies
freq(t_fulltext$decision)


#render table
reactable(t_fulltext,
          style = list(fontFamily = "Arial Narrow", fontSize = "14px"),
          defaultSorted = "author",
          defaultSortOrder = "asc",
          columns = list(
            author = colDef(minWidth = 105),
            year = colDef(maxWidth = 70),
            'study id' = colDef(maxWidth = 70),
            decision = colDef(maxWidth = 70, cell = function(value) {
              # Render as ✘ or ✓
              if (value == 0) "\u2718" else "\u2713"
            }, style = function(value){
              if (value == 0) {color <- '#B62A3D'} else {color <- '#456355'}
              list(background = color)})
          ),
          bordered = FALSE,
          highlight = FALSE,
          striped = FALSE,
          searchable = TRUE,
          showPageSizeOptions = TRUE,
          )

