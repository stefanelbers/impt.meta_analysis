#install.packages
#install.packages("corrplot")

#load packages
library(corrplot)
library(flexdashboard)
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
library(DT)
library(reactable)
library(stringr)

url <- 'https://docs.google.com/spreadsheets/d/1y-L0VzN_49IZATCGkymexlkLr7yytFukZhC07X6REgY/edit?usp=sharing'

#read data
dat_poms <- gsheet2tbl(url) # keep dat_gsheet as in Google Sheets

#recode answers
dat_poms[dat_poms=="0 - Helemaal niet"] <- 0
dat_poms[dat_poms=="1 - Een beetje"] <- 1
dat_poms[dat_poms=="2 - Redelijk"] <- 2
dat_poms[dat_poms=="3 - Best wel"] <- 3
dat_poms[dat_poms=="4 - Erg"] <- 4
dat_poms[dat_poms=="Man"] <- 0
dat_poms[dat_poms=="Vrouw"] <- 1

#rename variables
dat_poms <- dat_poms %>%
  rename(how_active = "Hoe actief ben je de afgelopen 24 uur geweest?",
         sedentary = "Hoeveel uur heb je naar schatting gelegen of gezeten in de afgelopen 24 uur?",
         gender = "Geslacht",
         age = "Leeftijd")

#reverse scoring for Ashamed (c16) and Emberrased (c44)

colnames(dat_poms)

#change variable type
poms_cols <- c(5:44)
dat_poms[,poms_cols] <- lapply(dat_poms[,poms_cols], function(x) as.numeric(as.character(x)))
as.numeric(dat_poms$how_active)
dat_poms$gender <- as.numeric(dat_poms$gender)

#create subscales
ten_cols <- c(1, 10, 15, 16, 23, 28)
ang_cols <- c(2, 11, 20, 22, 26, 33)
fat_cols <- c(3, 18, 27, 30, 39)
dep_cols <- c(4, 8, 14, 21, 24, 29, 35)
era_cols <- c(5, 12, 19, 25, 31, 40)
vig_cols <- c(6, 9, 13, 34, 37)
con_cols <- c(7, 17, 32, 36, 38)

#align subscales with columns

ten_cols <- ten_cols +4
ang_cols <- ang_cols +4
fat_cols <- fat_cols +4
dep_cols <- dep_cols +4
era_cols <- era_cols +4
vig_cols <- vig_cols +4
con_cols <- con_cols +4

dat_poms$tension    <- rowSums(dat_poms[,ten_cols])
dat_poms$anger      <- rowSums(dat_poms[,ang_cols])
dat_poms$fatigue    <- rowSums(dat_poms[,fat_cols])
dat_poms$depression <- rowSums(dat_poms[,dep_cols])
dat_poms$esteem     <- rowSums(dat_poms[,era_cols])
dat_poms$vigour     <- rowSums(dat_poms[,vig_cols])
dat_poms$confusion  <- rowSums(dat_poms[,con_cols])

poms_subscales <- c("tension", "anger", "fatigue", "depression", "esteem", "vigour", "confusion")

#descriptive data
summary(dat_poms[,poms_subscales])

#scatterplots
ggplot(dat_poms, aes(x=tension, y=age)) + geom_point()+
  geom_smooth(method=lm)

#correlation
cor.test(dat_poms$tension, dat_poms$age, method = "pearson", conf.level = 0.95)

as.numeric(dat_poms$how_active)

sapply(dat_poms, class)


corr_subset <- c("tension", "anger", "fatigue", "depression", "esteem", "vigour", "confusion", "age", "gender", "sedentary", "how_active")
dat_poms_cor <- dat_poms[,corr_subset]

#correlation plot

M <- cor(dat_poms_cor)
corrplot(M, type = "upper", order = "original", 
         tl.col = "black", tl.srt = 45)


#boxplots
boxplot(dat_poms_cor,
        main = "boxplots POMS",
        at = c(1:11),
        names = c("tension", "anger", "fatigue", "depression", "esteem", "vigour", "confusion", "age", "gender", "sedentary", "how_active"),
        las = 2,
        col = c("orange","red"),
        border = "brown",
        horizontal = TRUE,
        notch = FALSE
)


#shiny app

#create Shinyapp
ui <- fluidPage(
  
  # App title ----
  headerPanel("POMS ADB scores"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      selectInput(inputId = "xaxis",
                  label = "select POMS subscale:",
                  choices = c("tension", "anger", "fatigue", "depression", "esteem", "vigour", "confusion"),
                  selected = "tension"),
      
      selectInput(inputId = "yaxis",
                  label = "select variable:",
                  choices = c("gender", "how_active", "sedentary"),
                  selected = "how_active")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput("scatterplot", height = "1300px"),
      plotOutput("correlation"),
      plotOutput("boxplots")
    )
  )
)
# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  selectedData <- reactive(dat_poms)
  
  output$scatterplot <- renderPlot({
    
    ggplot(selectedData(), aes_string(x=input$xaxis, y=input$yaxis)) + geom_point()+
      geom_smooth(method=lm)
    
  })
  
  M <- cor(dat_poms_cor)
  
  output$correlation <- renderPlot(
    corrplot(M, type = "upper", order = "original", 
             tl.col = "black", tl.srt = 45))
  
  
  output$boxplots <- renderPlot(
    boxplot(dat_poms_cor,
            main = "boxplots POMS",
            at = c(1:11),
            names = c("tension", "anger", "fatigue", "depression", "esteem", "vigour", "confusion", "age", "gender", "sedentary", "how_active"),
            las = 2,
            col = c("orange","red"),
            border = "brown",
            horizontal = TRUE,
            notch = FALSE
    )
  )
  
}

shinyApp(ui=ui, server=server)










           