# map with internal labeling and fill with colors of statistics

save.png <- function(plot, file.name) {
  png(filename=file.name, width=800, height=800);
  print(plot);
  dev.off();
}

# create a blank ggplot theme
create_theme <- function(){
  theme_opts <- list(theme(
    panel.background = element_blank(),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    # remove axes title
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text =  element_text(size=9),
    legend.key.size = unit(0.4,"cm"),
    plot.title = element_text(lineheight=.6, face="bold"),
    panel.margin = unit(-0.2,"cm")
  ))
}

#' Plots map and places labels/graphics inside each polygon.
#' Create choropleth map style for continous data.
#'
#' @title Plot map with labels/graphics inside each polygon
#' @param map_data polygon data for plotting map
#' @param label_x longtitude positions for labels/graphics
#' @param label_y latitude positions for labels/graphics
#' @param label_text text or graphics of the labels
#' @param fill_data OPTIONAL: a column in map_data for filling polygons with colours (alwasys merge with map_data)
#' @param pal colouring palette, as in ggplot2
#' @param title title of the plot
#' @param label_size text size of the labels
#' @param family font of the labels
#'
#' @return a ggplot2 plot
#'
#' @examples see "use-cases/usecase-plot_basic_fill"
#' @export
plot_basic_fill <- function(map_data,label_x=ot_label_points$best_x, label_y=ot_label_points$best_y,label_text=ot_label_points$id,
                               fill_data=NULL,pal="Greens",title=NULL,label_size=4, family="Helvetica",alpha = 1,...){
  g <- ggplot2::ggplot(map_data, aes(long, lat, group = id)) +
    geom_polygon(aes_string(fill= fill_data), alpha=alpha) + # OT file
    geom_polygon(colour = "grey60", alpha=0.9, fill = NA) +  # OT outlines
    annotate("text", label = label_text, x = label_x, y = label_y, size = label_size, family=family)+
    scale_fill_distiller(type = "seq", palette = pal, direction = 1, na.value = "bisque4", breaks = scales::pretty_breaks(n = 5), space = "Lab",guide="legend") +
    # guides(fill = guide_legend(reverse = TRUE))+
    ggtitle(title) +
    xlab("Latitude") +
    ylab("Longitude") +
    coord_fixed() +
    create_theme()
    return(g)  # rememeber to return only the graph, otherwise, all data
}
