##Obtain data from google##

#Input
url <- 'https://docs.google.com/spreadsheets/d/1DE08OpdFC2eH8VwAV-xLmsFByGh2VvL0DRDjPLhyo9U/edit?usp=sharing'

#Load packages
#install.packages('gsheet')
#install.packages('dplyr')
library(gsheet)
library(dplyr)

#read data
dat <- gsheet2tbl(url)


#Create author list

library(tidyverse)
library(magrittr)
library(tidyr)
library(dplyr)

author_list <- (dat$author) %>%
  as.data.frame() %>%
  unique() 
  
  


#Shiny

ui <- fluidPage(
  
  # App title ----
  titlePanel("data extraction forms"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      selectInput(inputId = "author",
                  label = "select author:",
                  choices = author_list,
                  selected = NULL)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      formattableOutput(outputId = "data_extraction_table")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
    output$data_extraction_table <- renderFormattable({
    
    extraction_data <- as.data.frame(t(dplyr::filter(dat, author == input$author)))
    library(formattable)
    formattable(extraction_data)   
  
  })
  
}

shinyApp(ui, server)
