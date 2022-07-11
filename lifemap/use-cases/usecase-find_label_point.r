library(plyr)
# label information of all Ortsteil in Leipzig
load("data/ot_label.rdata")
# polygon information of all Ortsteil in Leipzig
load("data/ot_polygon.rdata")

found_label_points <- find_label_points(ot_label,ot_polygon)
head(found_label_points)
