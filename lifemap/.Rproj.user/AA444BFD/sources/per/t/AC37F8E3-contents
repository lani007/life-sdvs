library(lifemap)

load("data/ot_label_points.rdata")
load("data/ot_polygon.rdata")
load("data/ot_handkraft.rdata")

###################################
# use case for label inside
###################################
male_data <- ot_handkraft[ which(ot_handkraft$SEX==1),]
female_data <- ot_handkraft[ which(ot_handkraft$SEX==2),]
col <- c("id","MEDIAN_HG_MAX")
male_data <- male_data[,col]
colnames(male_data)[2] <- "median_hg_max_male"
female_data <- female_data[,col]
colnames(female_data)[2] <- "median_hg_max_female"

fill_data <- merge(male_data,female_data,by.x = "id",by.y = "id")
map_data <- merge(ot_polygon,fill_data,by.x="id",by.y = "id")
male_fill <- map_data$median_hg_max_male
female_fill <- map_data$median_hg_max_female

title_male = "Handkraft der Maenner in Ortsteile"
outfile_male = "plots/handkraft-ml-leipzig_ot.png"
title_female= "Handkraft der Frauen in Ortsteile"
outfile_female = "plots/handkraft-wbl-leipzig_ot.png"

# pal and fill take strings
# plot male
p_inside_male <- plot_basic_fill(map_data = map_data,
                        label_text = ot_label_points$id, pal = "Greens",
                        title = title_male,
                        fill = "male_fill"
                        )

p_inside_female <- plot_basic_fill(map_data = map_data,
                                   label_text = ot_label_points$id, pal = "Oranges",
                                   title = title_female,
                                   fill = "female_fill"
)

p_inside_male
p_inside_female

# save.png(p_inside_male,outfile_male)
# save.png(p_inside_female,outfile_female)


