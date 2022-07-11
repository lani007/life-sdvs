## enable external labeling on color-filled maps

############## pull labels out of the map RIGHT ##############
assign_right_labels <- function(right_group=NULL, label_data, right_top, right_bottom, left_alignment){
  # right_group has pulled out labels on the right
  right <- label_data[label_data$id %in% right_group,]
  # assign each id in right group a y_axis value}
  right_max_y <- right_top
  right_min_y <- right_bottom
  interval <- (right_max_y - right_min_y)/(length(right_group)-1)
  right_y_points <- seq(right_min_y, right_max_y, length.out = length(right_group))
  right_y_ordered <- right[with(right,order(best_y)),]
  right_y_ordered$right_label_y <- right_y_points
  #fixed x : label_data$best_x[label_data$id=="29"]
  right_y_ordered$right_label_x <- left_alignment
  right <- right_y_ordered[,c("id", "right_label_x", "right_label_y")]
  return(right)
}

############## pull labels out of the map BOTTOM ##############
assign_bottom_labels <- function(bottom_group, label_data, top_alignment, bottom_right,bottom_left){
  bottom <- label_data[label_data$id %in% bottom_group,]
  # assign each id in bottom group a x_axis value
  # label_data$best_x[ which(label_data$id == "33")]
  bottom_max_x <- bottom_right
  # label_data$best_x[ which(label_data$id == "70")]
  bottom_min_x <- bottom_left
  interval <- (bottom_max_x - bottom_min_x)/(length(bottom_group)-1)
  bottom_x_points <- seq(bottom_min_x, bottom_max_x, length.out = length(bottom_group))
  bottom_x_ordered <- bottom[with(bottom,order(best_x)),]
  bottom_x_ordered$bottom_label_x <- bottom_x_points
  # label_data$best_y[label_data$id=="55"]
  bottom_x_ordered$bottom_label_y <- top_alignment
  bottom <- bottom_x_ordered[,c("id", "bottom_label_x", "bottom_label_y")]
  return(bottom)
}

  ############## pull labels out of the map LEFT ##############
assign_left_labels <- function(left_group, label_data, left_top, left_bottom, right_alignment){
  left <- label_data[label_data$id %in% left_group,]
  # assign each id in left group a y_axis value
  # label_data$best_y[ which(label_data$id == "75")]
  left_max_y <- left_top
  # label_data$best_y[ which(label_data$id == "54")]
  left_min_y <- left_bottom
  interval <- (left_max_y - left_min_y)/(length(left_group)-1)
  left_y_points <- seq(left_min_y, left_max_y, length.out = length(left_group))
  left_y_ordered <- left[with(left,order(best_y)),]
  left_y_ordered$left_label_y <- left_y_points
  #fixed x
  min_x <- right_alignment # min(ot_polygon$long)
  left_y_ordered$left_label_x <- min_x - interval
  left <- left_y_ordered[,c("id", "left_label_x", "left_label_y")]
  return(left)
}

# remove the internal labels if external label exists for the polygon
remove_inside_label <- function(right_group=NULL, left_group=NULL, bottom_group=NULL, label_data){
  polygons_to_remove <- c(right_group,left_group,bottom_group)
  labels_to_remove <- data.frame(id=label_data[, c("id")])
  labels_to_remove$inside_x <- label_data$best_x
  labels_to_remove$inside_y <- label_data$best_y
  indx <- which(labels_to_remove$id %in% polygons_to_remove == TRUE)
  labels_to_remove$inside_x[indx] <- NA
  labels_to_remove$inside_y[indx] <- NA
  return(labels_to_remove)
}

# calculate the position of the external labels in three groups
calculate_label_outside <- function(right_group=NULL, left_group=NULL, bottom_group=NULL, label_data,
                                    right_top, right_bottom, left_alignment,
                                    left_top,left_bottom,right_alignment,
                                    top_alignment, bottom_right, bottom_left){
  if(is.null(right_group)){
    right_labels <-data.frame(id=label_data$id,right_label_y =NA,right_label_x=NA)
  } else {
    right_labels <- assign_right_labels(right_group, label_data,right_top, right_bottom , left_alignment)
  }
  if(is.null(bottom_group)){
    bottom_labels <-data.frame(id=label_data$id,bottom_label_y =NA,bottom_label_x=NA)
  } else {
    bottom_labels <- assign_bottom_labels(bottom_group, label_data,top_alignment, bottom_right, bottom_left)
  }
  if(is.null(left_group)){
    left_labels <-data.frame(id=label_data$id,left_label_y =NA,left_label_x=NA)
  } else {
    left_labels <- assign_left_labels(left_group, label_data,left_top,left_bottom,right_alignment)
  }
  group_labels <- Reduce(function(x,y)merge(x,y,all=TRUE),list(right_labels,left_labels,bottom_labels))
  # add label_inside
  label_inside <- remove_inside_label(right_group=right_group,left_group=left_group, bottom_group=bottom_group,label_data=label_data)
  group_labels <-merge(group_labels,label_inside,by="id",all=TRUE)
  group_labels$best_x <- label_data$best_x
  group_labels$best_y <- label_data$best_y
  return(group_labels)
}


#' Plots map of Leipzig and places labels at the label regions outside of the map. Three label regions: right, left and bottom.
#'
#' User can define which labels are placed in which of the three label regions.
#'
#' @title Plots map and places part of the labels outside of the map (Only for map of Leipzig)
#' @param map_data polygon data of the map
#' @param label_text text or graphics of the labels
#' @param pal colouring palette, as in ggplot2
#' @param fill_data OPTIONAL: a column in map_data for filling polygons with colours (alwasys merge with map_data)
#' @param title title of the plot
#' @param label_data dataframe contains label best_x and best_y
#' @param right an array of polygon ids whose labels are placed in label region right
#' @param left an array of polygon ids whose labels are placed in label region left
#' @param bottom an array of polygon ids whose labels are placed in label region bottom
#' @param label_line_colour line colour for labels outside
#' @param label_size text size of the labels
#' @param family font of the labels
#'
#' @return ggplot2 map of Leipzig with labels in label regions
#'
#' @examples see "use-cases/usecase-plot_continous.r" and "use-cases/usecase-plot_continous_sbz.r"
#'
#' @export
plot_continuous <- function(map_data,label_text, pal="Greens", fill_data=NULL, title=NULL,
                               label_data, right=NULL, left=NULL, bottom=NULL, label_line_colour="blue",
                               label_size=4, family="Helvetica", alpha=1, inLabel_off="no",
                               margin_left=0.055, margin_right=0.02, margin_bottom=0,
                               right_top=51.43357, right_bottom=51.3519, left_alignment=12.52072,
                               left_top=51.36014, left_bottom=51.27666, right_alignment=12.23666,
                               bottom_right=12.44595, bottom_left=12.3366, top_alignment=51.2598,
                               with_legend="yes"){
  # calculate_label
  group_labels <- calculate_label_outside(right_group=right, left_group=left,bottom_group=bottom, label_data= label_data,
                                          right_top=right_top, right_bottom=right_bottom , left_alignment=left_alignment,
                                          left_top=left_top,left_bottom=left_bottom,right_alignment=right_alignment,
                                          bottom_right=bottom_right, bottom_left=bottom_left, top_alignment=top_alignment)
  group_labels <- cbind(group_labels,label_text)
  if (inLabel_off == "no"){
    inside_x <- group_labels$inside_x
    inside_y <- group_labels$inside_y
  } else {
    inside_x <- NULL
    inside_y <- NULL
  }

  # to expand margin (shrink map area for larger labels)
  min_x <- min(map_data$long)-margin_left
  max_x <- max(map_data$long)+margin_right
  min_y <- min(map_data$lat)-margin_bottom
  max_y <- max(map_data$lat)

  options(warn=-1)
  g <- plot_basic_fill(map_data=map_data,label_x=inside_x, label_y=inside_y, label_text=label_text, fill_data=fill_data, pal=pal, title=title, label_size = label_size, family=family,alpha=alpha)+
    scale_x_continuous(limits=c(min_x,max_x))+
    scale_y_continuous(limits=c(min_y,max_y))+
    coord_fixed() +
    if (with_legend=="yes"){
      theme(legend.position=c(0.9,0.2))
    }else{
      theme(legend.position="none")
    }

  if(!is.null(right)){
    right <- resolve_intersection(group_labels)[[1]]
    g <- g + geom_text(data=right, aes(x=label_x, y=label_y, label=label_text,hjust=0, vjust=0), size=label_size,family=family, na.rm = TRUE) +
         geom_segment(data=right, aes(x = best_x, y = best_y, xend = label_x, yend = label_y) , na.rm = TRUE, size=.3, color=label_line_colour)
    }

  if(!is.null(left)){
    left <- resolve_intersection(group_labels)[[2]]
    g <- g + geom_text(data=left, aes(x=label_x, y=label_y, label=label_text,hjust=1, vjust=1), size=label_size,family=family, na.rm = TRUE) +
         geom_segment(data=left, aes(x = best_x, y = best_y, xend = label_x, yend = label_y) , na.rm = TRUE, size=.3, color=label_line_colour)
    }

  if(!is.null(bottom)){
    bottom <- resolve_intersection(group_labels)[[3]]
    g <- g + geom_text(data=bottom, aes(x=label_x, y=label_y, label=label_text,angle=45,hjust=1, vjust=1),size=label_size,family=family, na.rm = TRUE) +
         geom_segment(data=bottom, aes(x = best_x, y = best_y, xend = label_x, yend = label_y) , na.rm = TRUE, size=.3, color=label_line_colour)
  }
  options(warn=0)
  return(g)  # rememeber to return only the graph, otherwise, all data
}
