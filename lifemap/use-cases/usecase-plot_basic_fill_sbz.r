library(lifemap)
library(dplyr)
load("data/sbz_label_points.rdata")
load("data/sbz_label.rdata")
load("data/sbz_polygon.rdata")
load("data/ot_handkraft.rdata")

###################################
# use case for label inside
###################################
sbz_labels <- ot_label[,c(1,3)]
sbz_handkraft <- merge(ot_handkraft,sbz_labels)

male_data <- sbz_handkraft[ which(sbz_handkraft$SEX==1),]
female_data <- sbz_handkraft[ which(sbz_handkraft$SEX==2),]
col <- c("sbz.id","MEDIAN_HG_MAX")
male_data <- male_data[,col]
colnames(male_data)[2] <- "median_hg_max_male"
female_data <- female_data[,col]
colnames(female_data)[2] <- "median_hg_max_female"

group <- male_data %>% group_by(sbz.id)
dplyr::summarise(group,male_data=mean(median_hg_max_male))

group <- female_data %>% group_by(sbz.id)
dplyr::summarise(group,female_data=mean(median_hg_max_female))
colnames(male_data)[1] <- "id"
colnames(female_data)[1] <- "id"

fill_data <- merge(male_data,female_data,by.x = "id",by.y = "id")
map_data <- merge(sbz_polygon,fill_data,by.x="id",by.y = "id")
male_fill <- map_data$median_hg_max_male
female_fill <- map_data$median_hg_max_female

title_male = "Handkraft der Maenner in Stadtbezirk"
outfile_male = "plots/handkraft-ml-leipzig_sbz.png"
title_female= "Handkraft der Frauen in Stadtbezirk"
outfile_female = "plots/handkraft-wbl-leipzig_sbz.png"

# pal and fill take strings
# plot male
p_inside_male <- plot_basic_fill(map_data = map_data,
                                   label_x = sbz_label_points$best_x,
                                   label_y = sbz_label_points$best_y,
                                   label_text = sbz_label_points$id, pal = "Greens",
                                   title = title_male,
                                   fill = "male_fill"
)

p_inside_female <- plot_basic_fill(map_data = map_data,
                                     label_x = sbz_label_points$best_x,
                                     label_y = sbz_label_points$best_y,
                                     label_text = sbz_label_points$sbz.name, pal = "Oranges",
                                     title = title_female,alpha = 1,
                                     fill = "female_fill"
)

p_inside_male
p_inside_female

