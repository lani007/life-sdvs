library(lifemap)
library(dplyr)
load("data/sbz_label_points.rdata")
load("data/sbz_polygon.rdata")
load("data/ot_population.rdata")

###################################
# use case for labeling outside
###################################

# though no external labels, right, bottom, left still have to be assigned as NULL
right <- NULL
bottom <- NULL
left <- NULL

# assign id of Ortsteil for each label region
sbz_labels <- ot_label[,c(1,3)]
head(ot_population)
head(sbz_labels)
sbz_population <- merge(ot_population,sbz_labels)
head(sbz_population)
sbz_population <- select(sbz_population,sbz.id,pop.total)
colnames(sbz_population)[1] <- "id"
head(sbz_population)
sbz_pop<- sbz_population %>% group_by(id) %>% dplyr::summarise(pop=sum(pop.total))
title_sbz <- "Population size in Stadtbezirke"
map_data <- merge(sbz_polygon,sbz_pop,by.x = "id", by.y = "id")
pop <- map_data$pop.total
label_text <- sbz_label_points$sbz.name
p1 <- plot_continuous(map_data = map_data,
                         label_text = label_text , pal = "Greens",
                         title = title_sbz,
                         right=right, left=left,bottom=bottom,
                         label_data = sbz_label_points,
                         alpha=1,
                         family = "Helvetica", label_size = 3.5,
                         label_line_colour = "#663300",
                         fill_data = "pop")
p1
