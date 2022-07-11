# overall LIFE Shiny Application ui for the Run App

shinyUI(
  navbarPage("LIFE",
             tabPanel("Continuous Data",source('ui_continuous.r',local = TRUE) ),
             tabPanel("Categorical Data",source('ui_categorical.r',local = TRUE) )
  )
)