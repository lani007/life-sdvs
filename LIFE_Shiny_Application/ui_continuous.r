# LSA user interface for continuous data

fluidPage(
# Application title
h3("Visualization of statistics on Leipzig map"),
  
# get statistics variables from input data
sidebarLayout(
  sidebarPanel(width=4,
     tabsetPanel(type = "tabs",
       tabPanel("Data",
          radioButtons("dataSource","Data source:", c("Example: hand grip","User-defined", "Upload data"),selected="Example: hand grip"),
          conditionalPanel(
            condition = "input.dataSource == 'User-defined'",
            textInput("userDataTable","Enter Derivattabelle Name (Example: BMI)",value = "D00074"),
            textInput("userDataValue","Enter Feldname of the data to be plotted", value = "D00074_F0004"),
            textInput("userDataSic", "Enter Feldname of SIC", value = "D00074_F0001"),
            textInput("userDataDate", "Enter Feldname of Erhebungsdatum", value = "D00074_F0002"),
            actionButton("getData","Get Data")
          ),
          conditionalPanel(
            condition = "input.dataSource == 'Upload data'",
            fileInput(inputId = "userDataUpload", label="Choose csv file to upload:", accept = '.csv'),
            actionButton("plotUserMap","Plot Map")
          ), # conditionalPanel
          radioButtons("statistics","Statistics:",c("Mean","Median","Absolute frequency"),selected = "Mean"),
          radioButtons("whichmap","Which Leipzig map?", c("Ortsteil","Stadtbezirk"),selected = "Ortsteil",inline = TRUE)
  ), #tabPanel
  tabPanel("Filter", 
      radioButtons("gender","Gender", c("all","male", "female"),selected="all"),
      radioButtons("age","Age groups", choices = c("all", age_class_name),selected = "all"),
      conditionalPanel(
        condition = "input.age == 'all' & input.statistics == 'Mean'",
        checkboxInput("standardize","Apply age standardization",value = FALSE)
      ),
      numericInput("filter", "Minimum absolute frequency of an area",value = 0)
  ), #tabPanel
  tabPanel("Map Features",
      radioButtons("fill","Fill areas with color?",c("yes","no"),selected = "yes",inline = TRUE),
      radioButtons("color","Fill color", c("Greens","Oranges","User-defined"), selected="Greens",inline=TRUE),
      conditionalPanel(
        condition = "input.color == 'User-defined'",
        textInput("userColor","Enter ColorBrewer Platte Name:",value = "Blues")
      ),
      radioButtons("font","Label font", c("Bookman","Helvetica","User-defined"), selected="Bookman",inline=TRUE),
      conditionalPanel(
        condition = "input.font == 'User-defined'",
        textInput("userFont","Enter Font:",value = "Times")
      ),
      radioButtons("label","Labeling with", c("id", "name","name: mean", "name: frequency", "name: median"),selected="id", inline=TRUE),
      sliderInput("size", "Label size:", min = 1, max = 10, value = 5, step = 0.5)
  ), # tabPanel
  tabPanel("Label Placement",
    radioButtons("labelPlacement","Label placement", c("within each area", "boundary labeling (only for Ortsteil)"),selected = "within each area"),
    conditionalPanel(
      condition = "input.labelPlacement == 'boundary labeling (only for Ortsteil)' & input.map != 'Stadtbezirk'",
      tabsetPanel(type = "tabs",
        tabPanel("Boundary Labeling",
           radioButtons("inLabel","Internal labels off ?", c("yes","no"), selected="no",inline=TRUE),
           selectInput("right","Assign labels to be plotted at the RIGHT of the map:", choices = c(Choose='', ot_label_points$id), 
                       multiple=TRUE, selectize=TRUE,width = "100%",selected = right),
           selectInput("left","Assign labels to be plotted at the LEFT of the map:", choices = c(Choose='', ot_label_points$id), 
                       multiple=TRUE, selectize=TRUE,width = "100%",selected = left),
           selectInput("bottom","Assign labels to be plotted at the BOTTOM of the map:", choices = c(Choose='', ot_label_points$id), 
                       multiple=TRUE, selectize=TRUE,width = "100%",selected = bottom),
           radioButtons("labelLine","Leader color", c("darkgoldenrod4","darkmagenta","User-defined"), selected="darkgoldenrod4",inline=TRUE),
           conditionalPanel(
             condition = "input.labelLine == 'User-defined'",
             textInput("userLabelLine","Enter R Colors:",value = "navy")
           ),
           sliderInput("marginLeft", "Margin left:", min = 0, max = 0.5, value = 0.055, step = 0.01),
           sliderInput("marginRight", "Margin right:", min = 0, max = 0.5, value = 0.02, step = 0.01),
           sliderInput("marginBottom", "Margin bottom:", min = 0, max = 0.5, value = 0, step = 0.01)
        ), # tabPanel
        tabPanel("Label Group Adjustments",
           sliderInput("rightTop", "Right label group top label position:", min = 51.24, max = 51.45, value = 51.42, step = 0.001),
           sliderInput("rightBottom", "Right label group lowest label position:", min = 51.24, max = 51.45, value = 51.35, step = 0.001),
           sliderInput("rightLeft", "Right label group left alignment:", min = 12.44, max = 12.56, value = 12.52, step = 0.001),
           sliderInput("leftTop", "Left label group top label position:", min = 51.24, max = 51.45, value = 51.35, step = 0.001),
           sliderInput("leftBottom", "Left label group lowest label position:", min = 51.24, max = 51.45, value = 51.28, step = 0.001),
           sliderInput("LeftRight", "Left label group right alignment:", min = 12.23, max = 12.26, value = 12.24, step = 0.001),
           sliderInput("BottomRight", "Bottom label group rightest label position:", min = 12.23, max = 12.54, value = 12.43, step = 0.001),
           sliderInput("BottomLeft", "Bottom label group leftest label position:", min = 12.23, max = 12.54, value = 12.34, step = 0.001),
           sliderInput("bottomTop", "Bottom label group top alignment:", min = 51.24, max = 51.28, value = 51.26, step = 0.001)
              )  # tabPanel
            )# tabsetPanel
          ) # conditionalPanel
       ) # tabPanel
     ) # tabsetPanel
  ), # sidebarPanel
  mainPanel(
    h4(strong(textOutput("title")),align = "center",style = "color:darkblue"),
    h4(textOutput("title0"),align = "center",style = "color:darkblue"),
    plotOutput("map", width = "700px", height = "400px"),
    downloadButton("downloadMap","Download map")
  )
),
fluidRow(
  dataTableOutput("table")
)
) #fluidPage
