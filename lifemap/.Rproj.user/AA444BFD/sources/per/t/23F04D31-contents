#' Plots categorical data as pie charts for each region of Leipzig.
#'
#' User can define which labels are placed in which of the three label regions.
#'
#' @title Plots pie charts for each region in a map
#' @param map polygon data of the map
#' @param plotType "pie" for pie chart and "bar" for bar chart
#' @param inputDf categorical data to be plotted, three columns with colnames=("id", "category", "num"). "id": region id
#' @param filterValue sets filter for in each region. If sum(num) of a region is smaller than filterValue, no chart is displayed
#' @param colList color list for pie chart. Default list is inside the function
#' @param legendText a vector of legend text for each category. Default is values in column "category"
#' @param textSize textSize for legendText
#' @param textFont font of the legendText
#' @param pieRadius set radius for pie chart
#' @param sizeX length of x axis for bar chart
#' @param sizeY length of y axis for bar chart
#' @param legendPos_lx, _ly, _rx, _ry: lower left and upper right coordinates of the color legend
#'
#' @return map of Leipzig with pie charts in each region
#'
#' @examples see "use-cases/usecase-category_chart_bmi.r"
#'
#' @export
plot_categorical <- function(map="Ortsteil", plotType="bar", inputDf=bmiDf, filterValue = 0,
                          colList=NULL, legendText=NULL, textSize=0.8, textFont="Helvetica",
                          legendPos_lx=12.35, legendPos_ly=51.24, legendPos_rx=12.357, legendPos_ry=51.27,
                          pieRadius=0.0045, sizeX=0.3, sizeY=0.3){
  categories <- as.vector(sort(unique(inputDf$category)))
  if (is.null(legendText)){
    legendText <- categories
  }
  # empty plot as backgroup
  if (map== "Ortsteil"){
    mapDf <- ot_polygon
    x=ot_polygon$long
    y=ot_polygon$lat
    labelDf <- ot_label_points
  } else if (map == "Stadtbezirk"){
    mapDf <- sbz_polygon
    x=sbz_polygon$long
    y=sbz_polygon$lat
    labelDf <- sbz_label_points
  }
  # map data
  map_split <- split(mapDf,mapDf$id) # id is used as names of each split
  plot(x, y,
       xlab="",
       ylab="",
       type="n",
       axes=F)

  for(j in 1:length(map_split)){
    lines(map_split[[j]]$long,map_split[[j]]$lat,type="l",col="grey10",lwd = 1)
    map_split_id <- names(map_split)[j]
    # if a region has no data --> next
    if(nrow(inputDf[inputDf$id==map_split_id,])==0) {next}
    # if a region's absolute frequency < filterValue --> next
    if(sum(inputDf$num[inputDf$id==map_split_id]) < filterValue){next}
    label_x <- labelDf[which(labelDf$id==map_split_id),]$best_x
    label_y <- labelDf[which(labelDf$id==map_split_id),]$best_y
    chart_data <- c()
    if (is.null(colList)){
      colListDefault=c("purple","orange","springgreen4","yellow","dodgerblue4","red3")
    }
    else{
      colListDefault=colList
    }
    colList <- c()
    for (i in 1:length(categories)){
      d <- inputDf$num[inputDf$id==map_split_id & inputDf$category== i ] # if no data, gives "integer(0)" back
      if (length(d) == 0){
        chart_data <- c(chart_data, 0.0001) # floating.pie[plotrix] ignores 0 and use next color, so add an value to replace 0
      }
      chart_data <- c(chart_data, inputDf$num[inputDf$id==map_split_id & inputDf$category== i ])
      colList <- c(colList,colListDefault[i])
    } # for
    if (plotType == "bar"){
      plot_bar(label_x=label_x, label_y=label_y,chart_data=chart_data,colList = colList,sizeX = sizeX , sizeY = sizeY)
    }
    else if (plotType == "pie"){
      floating.pie(label_x,label_y,chart_data,radius = pieRadius,col=colList)
    }
  }
  color.legend(legendPos_lx,legendPos_ry,legendPos_rx,legendPos_ly,legend=legendText,rect.col=colList,
               align="rb", gradient = 'y',cex=textSize,family=textFont)
}

plot_bar <- function(label_x=label_x, label_y=label_y,chart_data=chart_data,colList=colList,sizeX=sizeX,sizeY=sizeY){
  subplot(barplot(chart_data,xaxt='n',yaxt='n',col = colList), x=label_x, y=label_y,size = c(sizeX,sizeY))
}
