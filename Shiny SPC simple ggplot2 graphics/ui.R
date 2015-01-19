##### ui.R #####

library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("SPC for Metrics"),  # end headerPanel
  
  uiOutput("ui"),
  
  mainPanel(
    h3("SPC chart"), 
    plotOutput("plotDisplay")
    )   # end mainPanel
  
  ))    # end page
