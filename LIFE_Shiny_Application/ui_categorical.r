# LSA user interface for categorical data

fluidPage(
# Application title
h3("Visualization of statistics on Leipzig map"),
  
# get statistics variables from input data
sidebarLayout(
  sidebarPanel(width=4,
     tabsetPanel(type = "tabs",
       tabPanel("Data",
          radioButtons("dataSourceCat","Data source:", c("Example: BMI","User-defined", "Upload data"),selected="Example: BMI"),
          conditionalPanel(
            condition = "input.dataSourceCat == 'User-defined'",
            textInput("userDataTableCat","Enter Derivattabelle Name (Example: SES)",value = "D00140"),
            textInput("userDataValueCat","Enter Feldname of the data to be plotted", value = "D00140_F0009"),
            textInput("userDataSicCat", "Enter Feldname of SIC", value = "D00140_F0001"),
            textInput("userDataDateCat", "Enter Feldname of Erhebungsdatum", value = "D00140_F0003"),
            actionButton("getDataCat","Get Data")
          ),
          conditionalPanel(
            condition = "input.dataSourceCat == 'Upload data'",
            fileInput(inputId = "userDataUploadCat", label="Choose csv file to upload:", accept = '.csv'),
            actionButton("plotUserMapCat","Plot Map")
          ), # conditionalPanel
          radioButtons("whichmapCat","Which Leipzig map?", c("Ortsteil","Stadtbezirk"),selected = "Ortsteil",inline = TRUE),
          # pdf download
          checkboxInput('returnpdf', 'output pdf?', FALSE)
  ), #tabPanel
  tabPanel("Filter", 
      radioButtons("genderCat","Gender", c("all","male", "female"),selected="all"),
      radioButtons("ageCat","Age groups", choices = c("all", age_class_name),selected = "all"),
      numericInput("filterCat", "Minimum absolute frequency of an area",value = 0)
  ), #tabPanel
  tabPanel("Map Features",
      radioButtons("plotType","Plot type",c("pie chart","bar chart"),selected = "bar chart",inline = TRUE),
      conditionalPanel(
        condition = "input.plotType == 'pie chart'",
        sliderInput("pieRadius", "Pie radius:", min = 0.003, max = 0.025, value = 0.0045, step = 0.0002)
      ),
      conditionalPanel(
        condition = "input.plotType == 'bar chart'",
        sliderInput("barSizeX", "Length of x axis:", min = 0.1, max = 2, value = 0.35, step = 0.05),
        sliderInput("barSizeY", "Length of y axis:", min = 0.1, max = 2, value = 0.35, step = 0.05)
      ),
      sliderInput("legendSize", "Legend text size:", min = 0.1, max = 2, value = 1.35, step = 0.05),
      radioButtons("fontCat","Legend font", c("Bookman","Helvetica","User-defined"), selected="Helvetica",inline=TRUE),
      conditionalPanel(
        condition = "input.fontCat == 'User-defined'",
        textInput("userFontCat","Enter Font:",value = "Times")
      )
     ) # tabPanel "Map Features"
     ) # tabsetPanel
  ), # sidebarPanel
  mainPanel(
    h4(strong(textOutput("title1")),align = "center",style = "color:darkblue"),
    h4(textOutput("title2"),align = "center",style = "color:darkblue"),
    plotOutput("mapCat", width = "800px", height = "600px"),
    downloadButton("downloadMapCat","Download map")
  )
),
fluidRow(
  dataTableOutput("tableCat")
)
) #fluidPage
