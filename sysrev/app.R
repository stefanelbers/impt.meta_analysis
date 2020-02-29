#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

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
    data_hrqol <- read_excel("data.raw/20200114.r_data_forest_plot.xlsx")
    data_hrqol <- as_tibble(data_hrqol)
    data_hrqol$ri <- 0 
    data_hrqol$ri <- input$r_value
    data_hrqol_meta <- escalc(measure="SMCR", m1i=m_post, m2i=m_pre, sd1i=sd_pre, ni=n_post, ri=ri, data=data_hrqol)
    hrqol_meta <- metafor::summary.escalc(data_hrqol_meta)
    hrqol_meta2 <- filter(hrqol_meta, contrast == input$contrast)
    
    
    forestplot(hrqol_meta2$author, hrqol_meta2$yi, hrqol_meta2$ci.lb, hrqol_meta2$ci.ub,
               zero = 0,
               cex = 2,
               lineheight = "auto",
               xlab = "Lab axis txt",
               lwd.ci= 3)
  })
  
}

shinyApp(ui=ui, server=server)

library(rsconnect)
rsconnect::deployApp('impt.meta_analysis/sysrev/app.R')
