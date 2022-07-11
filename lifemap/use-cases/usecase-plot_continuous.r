library(lifemap)
load("data/ot_label_points.rdata")
load("data/ot_polygon.rdata")
load("data/ot_population.rdata")

###################################
# use case for labeling outside
###################################

# assign id of Ortsteil for each label region
bottom <- c("70", "04","03", "42", "43", "33","50")
left<- c("66","65","62","63","61","60")
right <- c("91", "12", "06", "01", "20", "21","14")

title_ot <- "Anzahl der Einwohner in Ortsteile"
map_data <- merge(ot_polygon,ot_population,by.x = "id", by.y = "id")
pop <- map_data$pop.total
label_text <- ot_label_points$ot.name
p1 <- plot_continuous(map_data = map_data,
                         label_text = label_text , pal = "Greens",
                         title = title_ot,alpha=1,inLabel_off = TRUE,
                         right=right, left=left,bottom=bottom,
                         margin_left=0.12,margin_right=0.1,margin_bottom=0.05,
                         label_data = ot_label_points,
                         family = "Helvetica", label_size = 3,
                         label_line_colour = "#663300",
                         with_legend = "yes",
                         fill_data = "pop")
p1
