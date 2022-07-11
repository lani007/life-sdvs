# overall server for LIFE Shiny Application, for Run App

shinyServer(function(input, output) {
  source('server_continuous.r',local = TRUE)
  source('server_categorical.r',local = TRUE)
})
