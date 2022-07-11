# calculate center (not centroid) of each OT (ggplot2-book.pdf p.80)
# centers are used for putting labels in plots (nrow = 63)
mid_range <- function(x) mean(range(x, na.rm = TRUE))  #na.rm: remove missing data

find_centers <- function(polygon_data) {
    # calculate the mean of range (max and min) of lat/long for each OT
    centers <- ddply(polygon_data, .(id), colwise(mid_range, .(lat, long)))
    return(centers)
}

# find point in polygon which has min y-distance with y of center
# find_mid_points()

find_mid_points <- function(current_id, label_centers, polygon_data) {
    center <- label_centers[label_centers$id == current_id, ]
    center_x <- center$long
    center_y <- center$lat

    polygon_points <- polygon_data[polygon_data$id == current_id, ]
    polygon_points <- polygon_points[, c("long", "lat", "order")]  # select columns

    polygon_points$x_diff <- abs(polygon_points$long - center_x)
    polygon_points$y_diff <- abs(polygon_points$lat - center_y)

    polygon_points_ordered_x <- polygon_points[with(polygon_points, order(x_diff)), ]
    polygon_points_ordered_y <- polygon_points[with(polygon_points, order(y_diff)), ]
    min_x_for_x <- polygon_points_ordered_y$long[[1]]  # for adjusting x
    min_y_for_y <- polygon_points_ordered_x$lat[[1]]  # for adjusting y

    ## find mid_x ## min_x_point at right of center
    if (min_x_for_x - center_x > 0)
        {
            i <- 1
            while (polygon_points_ordered_y$long[[i]] - center_x > 0) {
                i <- i + 1
                another_x <- polygon_points_ordered_y$long[[i]]
            }
        }  # if

    if (min_x_for_x - center_x < 0)
        {
            # min_x_point at left of center
            i <- 1
            while (polygon_points_ordered_y$long[[i]] - center_x < 0) {
                # to find first point on the left which has smallest y-diff
                i <- i + 1
                another_x <- polygon_points_ordered_y$long[[i]]
            }  # while
        }  # if

    ## find mid_y ## min_x_point at right of center
    if (min_y_for_y - center_y > 0)
        {
            i <- 1
            while (polygon_points_ordered_x$lat[[i]] - center_y > 0) {
                i <- i + 1
                another_y <- polygon_points_ordered_x$lat[[i]]
            }
        }  # if

    if (min_y_for_y - center_y < 0)
        {
            # min_x_point at left of center
            i <- 1
            while (polygon_points_ordered_x$lat[[i]] - center_y < 0) {
                # to find first point on the left which has smallest y-diff
                i <- i + 1
                another_y <- polygon_points_ordered_x$lat[[i]]
            }  # while
        }  # if
    mid_x <- (min_x_for_x + another_x)/2
    mid_y <- (min_y_for_y + another_y)/2
    # return(c(mid_x, center_x, center_y))
    return(c(mid_x, mid_y))
}

find_mid_points_step2 <- function(current_id,label_centers,polygon_data) {
    mid_x <- label_centers$mid_x[label_centers$id == current_id]
    mid_y <- label_centers$mid_y[label_centers$id == current_id]

    polygon_points <- polygon_data[polygon_data$id == current_id, ]
    polygon_points <- polygon_points[, c("long", "lat", "order")]  # select columns

    polygon_points$x_diff <- abs(polygon_points$long - mid_x)
    polygon_points$y_diff <- abs(polygon_points$lat - mid_y)

    polygon_points_ordered_x <- polygon_points[with(polygon_points, order(x_diff)), ]
    polygon_points_ordered_y <- polygon_points[with(polygon_points, order(y_diff)), ]
    min_x_for_x <- polygon_points_ordered_y$long[[1]]  # for adjusting x
    min_y_for_y <- polygon_points_ordered_x$lat[[1]]  # for adjusting y

    ## find mid_yx_x ## min_x_point at right of center
    if (min_x_for_x - mid_x > 0)
        {
            i <- 1
            while (polygon_points_ordered_y$long[[i]] - mid_x > 0) {
                i <- i + 1
                another_x <- polygon_points_ordered_y$long[[i]]
            }
        }  # if

    if (min_x_for_x - mid_x < 0)
        {
            # min_x_point at left of center
            i <- 1
            while (polygon_points_ordered_y$long[[i]] - mid_x < 0) {
                # to find first point on the left which has smallest y-diff
                i <- i + 1
                another_x <- polygon_points_ordered_y$long[[i]]
            }  # while
        }  # if

    ## find mid_xy_y ## min_x_point at right of center
    if (min_y_for_y - mid_y > 0)
        {
            i <- 1
            while (polygon_points_ordered_x$lat[[i]] - mid_y > 0) {
                i <- i + 1
                another_y <- polygon_points_ordered_x$lat[[i]]
            }
        }  # if

    if (min_y_for_y - mid_y < 0)
        {
            # min_x_point at left of center
            i <- 1
            while (polygon_points_ordered_x$lat[[i]] - mid_y < 0) {
                # to find first point on the left which has smallest y-diff
                i <- i + 1
                another_y <- polygon_points_ordered_x$lat[[i]]
            }  # while
        }  # if
    mid_yx_x <- (min_x_for_x + another_x)/2
    mid_xy_y <- (min_y_for_y + another_y)/2
    return(c(mid_yx_x, mid_xy_y))
}

# calculate the euclidean distance between 2 points in 2D
euc_dist <- function(x_point, y_point, polygon_x, polygon_y) {
    diff_x = (polygon_x - x_point)^2
    diff_y = (polygon_y - y_point)^2
    dist <- sqrt(diff_x + diff_y)
    return(dist)
}

# get the mininum distance (min_dist) from a point to its polygon OUTPUT: list of min_list of eacht id
min_dist <- function(current_id, x_point, y_point, list_polygon_data) {
    # transfer in_polygon_data into data.frame
    polygon_data <- data.frame(list_polygon_data)
    polygon_points <- polygon_data[polygon_data$id == current_id, ]
    polygon_points <- polygon_points[, c("long", "lat", "order")]  # select columns
    polygon_x_points <- polygon_points$long
    polygon_y_points <- polygon_points$lat
    # cat ('start min_dist') cat ('current_id: ', current_id, 'x_point: ',x_point, 'y_point: ', y_point, '\n' )
    min_dist_list <- mapply(euc_dist, x_point, y_point, polygon_x_points, polygon_y_points)
    out_data <- min(min_dist_list)
    return(out_data)
}

# select the best point from candidate points for all polygon
# best candidate point: has the largest min_dist to polygon
# candidate points: center, mid_x, mid_y, mid_xy, mid_yx

find_best_candidate_points <- function(current_id, label_centers,max_values, mid_dist_df) {
    if (max_values[rownames(mid_dist_df) == current_id] == 1) {
        best_x <- label_centers$long[label_centers$id == current_id]
        best_y <- label_centers$lat[label_centers$id == current_id]
    }

    if (max_values[rownames(mid_dist_df) == current_id] == 2) {
        best_x <- label_centers$mid_x[label_centers$id == current_id]
        best_y <- label_centers$lat[label_centers$id == current_id]
    }

    if (max_values[rownames(mid_dist_df) == current_id] == 3) {
        best_x <- label_centers$long[label_centers$id == current_id]
        best_y <- label_centers$mid_y[label_centers$id == current_id]
    }

    if (max_values[rownames(mid_dist_df) == current_id] == 4) {
        best_x <- label_centers$mid_x[label_centers$id == current_id]
        best_y <- label_centers$mid_xy_y[label_centers$id == current_id]
    }

    if (max_values[rownames(mid_dist_df) == current_id] == 5) {
        best_x <- label_centers$mid_yx_x[label_centers$id == current_id]
        best_y <- label_centers$mid_y[label_centers$id == current_id]
    }
    return(c(best_x, best_y))
}

#' Find good positions to place label/graphics within each polygon
#'
#' @title Find good labeling positions
#' @param label_data dataframe containing label information. Muss contain a column called "id". Number of rows = number of polygons.
#' @param polygon_data dataframe containing longtitude and latitude values of polygons. Muss contain a column called "id" for each polygon.
#' @return A dataframe containing labeling information.
#' The best found positions for all polygons are saved in two vectors: best_x (longitude) and best_y (latitude).
#' @examples see "use-cases/usecase-find_label_point.r"
#'
find_label_points <- function(label_data, polygon_data) {
    centers <- find_centers(polygon_data)
    label_centers <<- merge(centers, label_data)
    # first step of find_mid_points
    current_id <- label_centers$id
    mid_points <- sapply(current_id, find_mid_points,label_centers, polygon_data)  #sapply wrapper of lapply by default returning a vector

    # assign results to label_centers
    label_centers$mid_x <- mid_points[1, ]
    label_centers$mid_y <- mid_points[2, ]

    # second step of find_mid_points
    mid_points_step2 <- sapply(current_id, find_mid_points_step2,label_centers, polygon_data)  #sapply wrapper of lapply by default returning a vector
    # assign results to label_centers
    label_centers$mid_yx_x <- mid_points_step2[1, ]
    label_centers$mid_xy_y <- mid_points_step2[2, ]

    label_id <- label_centers$id
    # x, y data for labels
    label_center_x <- label_centers$long
    label_center_y <- label_centers$lat
    label_mid_x <- label_centers$mid_x
    label_mid_y <- label_centers$mid_y
    label_mid_xy_x <- label_centers$mid_x
    label_mid_xy_y <- label_centers$mid_xy_y
    label_mid_yx_x <- label_centers$mid_yx_x
    label_mid_yx_y <- label_centers$mid_y
# calculate minimum distance to polygon of all candidate points ()
    min_dist_center <- mapply(min_dist, current_id, label_center_x, label_center_y, MoreArgs=list(polygon_data), SIMPLIFY = FALSE)
    min_dist_mid_x <- mapply(min_dist, current_id, label_mid_x, label_center_y, MoreArgs=list(polygon_data), SIMPLIFY = FALSE)
    min_dist_mid_y <- mapply(min_dist, current_id, label_center_x, label_mid_y, MoreArgs=list(polygon_data), SIMPLIFY = FALSE)
    min_dist_mid_xy <- mapply(min_dist, current_id, label_mid_x, label_mid_xy_y, MoreArgs=list(polygon_data), SIMPLIFY = FALSE)
    min_dist_mid_yx <- mapply(min_dist, current_id, label_mid_yx_x, label_mid_y, MoreArgs=list(polygon_data), SIMPLIFY = FALSE)

    # select the best candidate point create data.frame select the max in each row (id) and return col number (which candidate point)
    mid_dist_df <- cbind(min_dist_center, min_dist_mid_x, min_dist_mid_y, min_dist_mid_xy, min_dist_mid_yx)
    # determines the location of max of each row
    max_values <- apply(mid_dist_df, 1, which.max)  # 1: rows
    # 1:min_dist_center, 2:min_dist_mid_x, 3:min_dist_mid_y, 4:min_dist_mid_xy, 5:min_dist_mid_yx

    best_points <- sapply(current_id, find_best_candidate_points, label_centers, max_values,mid_dist_df)
    label_centers$best_x <- best_points[1, ]
    label_centers$best_y <- best_points[2, ]
    best_points_data <- label_centers
    return(best_points_data)
}


