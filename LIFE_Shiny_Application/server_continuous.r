user_query <- eventReactive(input$getData | input$dataSource == "User-defined" ,{
  q <- create_query(tableName = input$userDataTable, sic = input$userDataSic, inDate = input$userDataDate, plotValue = input$userDataValue)
})

# read in user csv file 
user_data <- eventReactive(input$plotUserMap,{
  inFile <- input$userDataUpload
  # column 'id' has to be type character
  userData = read.csv(inFile$datapath,colClasses=c("id"="character"))
  data <- generate_data(q=NULL,userData=userData)
  return(data)
})

get_data_source <- reactive({
  if (input$dataSource == "Example: hand grip"){
    data <- generate_data(q=q_hg)
  }
  else if (input$dataSource == "User-defined"){
    q <- user_query()
    data <- generate_data(q=q)
  }
  else if (input$dataSource == "Upload data"){
    data <- user_data()
  }
  return(data)
})

get_fill_data <- reactive({   
  data <- get_data_source()
  if (input$gender == "male") {
    sh_gender <- "1"
  }
  else if (input$gender == "female"){
    sh_gender <- "2"
  }
  else if (input$gender == "all"){
    sh_gender <- "all"
  }
  if (input$statistics == "Absolute frequency"){
    stats <- "num"
  }
  else if (input$statistics == "Mean"){
    stats <- "mean"
  }
  else if (input$statistics == "Median"){
    stats <- "median"
  }
  if (input$whichmap == "Stadtbezirk"){
    map <- "sbz"
  }
  else if (input$whichmap == "Ortsteil"){
    map <- "ot"
  }
  if (input$standardize == FALSE){
    x <- value_to_show(data = data, in_gender = sh_gender, in_age=input$age, in_stats = stats, map = map, filter_value = input$filter)
  }
  else if (input$standardize == TRUE){
    x <- get_std_df(data=data,in_gender = sh_gender,map = map, filter_value = input$filter)
  }
  return(x)
}) 

# fill or no fill
set_alpha <- reactive({
  if (input$fill == "yes"){
    alpha <- 1
  }
  else if (input$fill == "no")(
    alpha <- 0
  )
})

set_right <- reactive({
  if (input$whichmap ==  "Stadtbezirk" || input$labelPlacement == "within each area"){
    right <- NULL
  } else if ( input$whichmap == "Ortsteil"){
    input$right
  }
})

set_left <- reactive({
  if (input$whichmap ==  "Stadtbezirk" || input$labelPlacement == "within each area"){
    left <- NULL
  } else if ( input$whichmap == "Ortsteil"){
    input$left
  }
})

set_bottom <- reactive({
  if (input$whichmap ==  "Stadtbezirk" || input$labelPlacement == "within each area"){
    bottom <- NULL
  } else if ( input$whichmap == "Ortsteil"){
    input$bottom
  }
})

label_line_color <-reactive({
  switch(input$labelLine,
         "darkgoldenrod4" = "darkgoldenrod4",
         "darkmagenta" = "darkmagenta",
         "User-defined" = input$userLabelLine)
})

color_data <- reactive({
  switch(input$color,
         "Greens" = "Greens",
         "Oranges" = "Oranges",
         "User-defined" = input$userColor )
}) 

label_font <- reactive({
  switch(input$font,
         "Bookman" = "Bookman",
         "Helvetica" = "Helvetica",
         "User-defined" = input$userFont)
})


label_text <- reactive({
  label_w_stats <- get_fill_data()[[3]]
  switch(input$label,
         "id" = label_w_stats$id,
         "name" = label_w_stats$name,
         "name: frequency" = label_w_stats$name.num,
         "name: mean" = label_w_stats$name.mean,
         "name: median" = label_w_stats$name.median)
})

get_map_data <- reactive({
   fill_data <- get_fill_data()[[2]]
  if (input$whichmap == "Stadtbezirk"){
    merge(sbz_polygon,fill_data,by.x="id",by.y = "id")
  }else if (input$whichmap == "Ortsteil"){
    merge(ot_polygon,fill_data,by.x="id",by.y = "id")
  }
})

# for x and y axis, default as best_x, best_y
label_data <- reactive({
  if (input$whichmap == "Stadtbezirk"){
    sbz_label_points
  }else if (input$whichmap == "Ortsteil"){
    ot_label_points
  }
})

## for title ##
get_table_name <- reactive({
  switch(input$dataSource,
         "Example: hand grip" = "Hand Grip Strength",
         "User-defined"= "Body Mass Index", # TODO: get info from VIEW
         "Upload data" = "Table_Name"
  )
})

title_text <- reactive({
  paste0(input$statistics," of ",get_table_name())
})

title_text0 <- reactive({
  if (input$standardize == TRUE & input$age =="all"){
      paste0(" (gender: ",input$gender, ", age group: ", input$age,", with age standardized)")
  }
  else {
    paste0(" (gender: ",input$gender, ", age group: ", input$age,")")
  }
})

# PLOT ....
plotInput <- reactive({
  map_data <- get_map_data()
  fill_column <<- map_data[,8]
  plot_continuous(map_data = get_map_data(),
               label_text = label_text() , pal = color_data(),
               right= set_right(), left=set_left(),bottom=set_bottom(),
               label_data = label_data(), inLabel_off = input$inLabel,
               margin_left = input$marginLeft, margin_right = input$marginRight,
               margin_bottom = input$marginBottom,
               right_top = input$rightTop, right_bottom = input$rightBottom, left_alignment = input$rightLeft,
               left_top = input$leftTop, left_bottom = input$leftBottom, right_alignment = input$LeftRight,
               bottom_right = input$BottomRight, bottom_left = input$BottomLeft, top_alignment = input$bottomTop, 
               label_size = input$size,
               family = label_font(), alpha = set_alpha(),
               label_line_colour = label_line_color(),
               with_legend = input$fill,
               fill_data = "fill_column") 
} ) # plotInput

# http://shiny.rstudio.com/gallery/dynamic-ui.html
output$title <- renderText({
  title_text()
})
output$title0 <- renderText({
  title_text0()
})

output$map <- renderPlot({
  fill_data <- get_fill_data()[[2]]
  if (length(which(!is.na(fill_data[,2])))==0){
    plot.new()
    text(0.4,0.5,"No data to fill the areas.\nCheck if the 'minimum absolute frequency' value is set properly.",col="red",size=7)
  }else{
    print(plotInput())
  }
}) # output$map

######## show tables ########
output$table <- renderDataTable({
    fill_data <- get_fill_data()[[1]]
})

######## downloads ########
output$downloadMap <- downloadHandler(
  filename = function() {paste(input$dataSource,'pdf', sep='.')},
  content = function(file) {
    device <- function(..., width, height) {
#       grDevices::png(..., width = width, height = height,
#                      res = 300, units = "in")
      grDevices::pdf(..., width = width, height = height)
    }
    ggsave(file, plot = plotInput(), device = device(width = 13))
  }) # downloadHandler



