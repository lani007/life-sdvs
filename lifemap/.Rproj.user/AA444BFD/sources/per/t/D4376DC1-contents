library(plotrix)
library(TeachingDemos)
library(dplyr)
load("data/ot_polygon.rdata")
load("data/ot_label_points.rdata")
load("data/sbz_polygon.rdata")
load("data/sbz_label_points.rdata")

# Ortsteile data
bmiDf <- read.csv("data-raw/bmi.csv")
bmiDf$id <- sprintf("%02d",bmiDf$id)
filter_value <- 30
plot_categorical(plotType = "pie",filterValue = filter_value)
plot_categorical(plotType = "bar",filterValue = filter_value)


# Stadtbezirk data
bmiDf_merged <- merge(bmiDf, ot_label_points,by="id")
bmiDf_sbz <- bmiDf_merged %>% group_by(sbz.id,category) %>% summarize(num=sum(num))
colnames(bmiDf_sbz)[1] <- "id"

categories <- sort(unique(bmiDf$category))
legends <- paste0("legend ",categories,sep="")
plot_categorical(plotType = "pie",map="Stadtbezirk",inputDf =bmiDf_sbz,textFont ="Times",
              filterValue = filter_value,legendText =legends)

