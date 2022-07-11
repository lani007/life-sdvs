# select label points and their polygons at each label group
select_df <- function(group_labels){
  right_col <- c("id","right_label_x","right_label_y")
  right <- group_labels[complete.cases(group_labels[right_col]),]
  colnames(right)[c(2,3)]<- c("label_x", "label_y")
  left_col <- c("id","left_label_x","left_label_y")
  left <- group_labels[complete.cases(group_labels[left_col]),]
  colnames(left)[which(names(left) == "left_label_x")]<- "label_x"
  colnames(left)[which(names(left) == "left_label_y")]<- "label_y"
  bottom_col <- c("id","bottom_label_x","bottom_label_y")
  bottom <- group_labels[complete.cases(group_labels[bottom_col]),]
  colnames(bottom)[which(names(bottom) == "bottom_label_x")]<- "label_x"
  colnames(bottom)[which(names(bottom) == "bottom_label_y")]<- "label_y"
  return(list(right,left,bottom))
}

calculate_slope_right <- function(label_x, label_y, best_x, best_y){
  slope = (label_y-best_y)/(label_x-best_x)
  return(slope)
}

calculate_slope_left <- function(label_x, label_y, best_x, best_y){
  slope = (best_y-label_y)/(best_x-label_x)
  return(slope)
}

calculate_slope_bottom <- function(label_x, label_y, best_x, best_y){
  slope = (label_x-best_x)/(label_y-best_y)
  return(slope)
}

# returns an array with slope of a fixed label_y to all other polygon points
find_min_slope <- function(label_ordered,group){
  new_labels <- label_ordered[FALSE,]
  len <- nrow(label_ordered)-1
  for (i in 1:len)
  {
    # print(i)
    if(group == "right")
      label_ordered$slope <- calculate_slope_right(label_ordered$label_x, label_ordered$label_y[1], label_ordered$best_x, label_ordered$best_y)
    if (group == "left")
      label_ordered$slope <- calculate_slope_left(label_ordered$label_x, label_ordered$label_y[1], label_ordered$best_x, label_ordered$best_y)
    if (group == "bottom")
      label_ordered$slope <- calculate_slope_bottom(label_ordered$label_x[1], label_ordered$label_y, label_ordered$best_x, label_ordered$best_y)

    colp <- c("id","best_x","best_y","label_x","label_y","slope")
    # print(label_ordered[,colp])
    # return index of the polygon with smallest slope
    selected_polygon_index <- which(label_ordered$slope == min(label_ordered$slope))
    # cat("index =",selected_polygon_index)
    # swap polygon if slope min is not at the first row
    if (selected_polygon_index != 1){
      col <- c("id","label_text","best_x","best_y")
      tmp <- label_ordered[1,col]
      label_ordered[1,col] <-label_ordered[selected_polygon_index,col]
      label_ordered[selected_polygon_index,col] <- tmp
    }
    # print(label_ordered[,colp])
    new_labels <- rbind(new_labels,label_ordered[1,])
    label_ordered = label_ordered[-1,]
    if (i == len){
      new_labels <- rbind(new_labels,label_ordered[1,])
    }
  }
  return(new_labels)
}

resolve_intersection <- function(group_labels){
  selected_df <- select_df(group_labels)
  if (nrow(selected_df[[1]])!=0){
    right <- selected_df[[1]]
    label_ordered_right <- right[with(right,order(label_y,decreasing = TRUE)),]
    new_right <- find_min_slope(label_ordered_right,group="right")
  }else{
    new_right <- NULL
  }
  if (nrow(selected_df[[2]])!=0){
    left <- selected_df[[2]]
    # left from bottom (works!)
    label_ordered_left <- left[with(left,order(label_y)),]
    new_left <- find_min_slope(label_ordered_left,group="left")
  }else{
    new_left <- NULL
  }
  if (nrow(selected_df[[3]])!=0){
    bottom <- selected_df[[3]]
    label_ordered_bottom<- bottom[with(bottom,order(label_x)),]
    new_bottom <- find_min_slope(label_ordered_bottom,group="bottom")
  }else{
    new_bottom <- NULL
  }
  return(list(new_right,new_left,new_bottom))
}
