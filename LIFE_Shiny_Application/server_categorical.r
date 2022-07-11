# LSA server for categorical data

user_query_cat <- eventReactive(input$getDataCat | input$dataSourceCat == "User-defined" ,{
  q <- create_query_cat(tableName = input$userDataTableCat, sic = input$userDataSicCat, inDate = input$userDataDateCat, plotValue = input$userDataValueCat)
  })

# read in user csv file 
user_data_cat <- eventReactive(input$plotUserMapCat,{
  inFile <- input$userDataUploadCat
  # column 'id' has to be type character
  userData = read.csv(inFile$datapath,colClasses=c("id"="character"))
  data <- generate_data(q=NULL,userData=userData)
  return(data)
})

get_data_source_cat <- reactive({
  if (input$dataSourceCat == "Example: BMI"){
    data <- generate_data(q=q_bmi)
    legendText <- get_legend_text("D00074_F0005")
    data <- generate_data(userData = bmi_cat_data)
    legendText <-c("Untergewicht", "Normalgewicht", "Pradipositas", "Adipositas")
  }
  else if (input$dataSourceCat == "User-defined"){
    q <- user_query_cat()
    data <- generate_data(q=q)
    legendText <-get_legend_text("D00073_F0009")
    data <- generate_data(userData = ses_data)
    legendText <- c("1.Quintil (Niedrig)", "2.Quintil (Mittel)","3.Quintil (Mittel)","4.Quintil (Hoch)","5.Quintil (Hoch)")
  }
  else if (input$dataSourceCat == "Upload data"){
    data <- generate_data(userData = ses_data)
    data <- user_data_cat() 
    legendText <- get_legend_text("D00074_F0005") # DUMMY, TODO legend text user define
  }
  return(list(data,legendText))
})

get_plot_data <- reactive({   
  data <- get_data_source_cat()[[1]]
  if (input$genderCat == "male") {
    sh_gender <- "1"
  }
  else if (input$genderCat == "female"){
    sh_gender <- "2"
  }
  else if (input$genderCat == "all"){
    sh_gender <- "all"
  }
  if (input$whichmapCat == "Stadtbezirk"){
    map <- "sbz"
  }
  else if (input$whichmapCat == "Ortsteil"){
    map <- "ot"
  }
  x <- value_to_show_cat(data = data, in_gender = sh_gender, in_age=input$ageCat, in_stats = "num", map = map, filter_value = input$filterCat)
  return(x)
}) 

label_font <- reactive({
  switch(input$fontCat,
         "Bookman" = "Bookman",
         "Helvetica" = "Helvetica",
         "User-defined" = input$userFontCat)
})

get_plotType <- reactive({
  switch(input$plotType,
         "pie chart" = "pie",
         "bar chart" = "bar")
})

## for title ##
get_table_name1 <- reactive({
  switch(input$dataSourceCat,
         "Example: BMI" = "Body Mass Index",
          "User-defined"= "Socioeconomic status", # TODO: get info from VIEW
          "Upload data" = "Table_Name"
           )
})

title_text1 <- reactive({
  paste0("Absolute frequency in each category of ",get_table_name1())
})
title_text2 <- reactive({
  paste0("(gender: ",input$genderCat, ", age group: ", input$ageCat,")")
})

# PLOT ....
plotInputCat <- reactive({
  inputDf <- get_plot_data()[[2]]
  legend_text <- get_data_source_cat()[[2]]
  if(input$returnpdf){
    pdf("plot.pdf", width=13,height = 11)
    plot_categorical(plotType=get_plotType(), inputDf = inputDf, map=input$whichmapCat, 
                  filterValue = input$filterCat, legendText = legend_text, pieRadius = input$pieRadius,
                  textSize = input$legendSize,textFont = input$fontCat,
                  sizeX = input$barSizeX, sizeY = input$barSizeY) 
    dev.off()
  }
  plot_categorical(plotType=get_plotType(), inputDf = inputDf, map=input$whichmapCat, 
                filterValue = input$filterCat, legendText = legend_text, pieRadius = input$pieRadius,
                textSize = input$legendSize,textFont = input$fontCat,
                sizeX = input$barSizeX, sizeY = input$barSizeY) 
} ) # plotInput

output$mapCat <- renderPlot({
  inputDf <- get_plot_data()[[2]]
  if (length(which(!is.na(inputDf[,3])))==0){
    plot.new()
    text(0.4,0.5,"No data to plot the graphics.\nCheck if the 'minimum absolute frequency' value is set properly.",col="red",size=7)
  }else{
    plotInputCat()
  }
}) # output$map

######## show title ########
output$title1 <- renderText({
  title_text1()
})

output$title2 <- renderText({
  title_text2()
})

######## show tables ########
output$tableCat <- renderDataTable({
  get_plot_data()[[1]]
})

######## downloads ########
output$downloadMapCat <- downloadHandler(
  filename = function() {paste(input$dataSourceCat,'pdf', sep='.')},
  content = function(file) {
    device <- function(..., width, height) {
      grDevices::pdf(..., width = width, height = height)
    }
    ggsave(file, plot = plotInputCat(), device = device(width = 13))
  }) # downloadHandler

#################### save pdf ##########
output$downloadMapCat <- downloadHandler(
  filename <- "myplot.pdf",
  content <- function(file) {
    file.copy("plot.pdf", file)
  }
)