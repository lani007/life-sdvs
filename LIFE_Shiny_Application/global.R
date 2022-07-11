load("data/ot_label_points.rdata")
load("data/ot_polygon.rdata")
load("data/sbz_label_points.rdata")
load("data/sbz_polygon.rdata")
# contains mapping between OT and SBZ
load("data/ot_label.rdata")

library(shiny)
library(lifemap)
library(dplyr)
library(TeachingDemos)
library(plotrix)

source('get_data_continuous.r', local = TRUE)
source('get_data_categorical.r', local = TRUE)
source('get_data_standardization.r', local = TRUE)

age_class_name <- c("(18,40]","(40,60]","(60,80+]")
