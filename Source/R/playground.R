# install.packages("httr")
# install.packages("rvest")
# install.packages("xml2")
# install.packages("tidyverse")
# install.packages("xlsx")
# install.packages("randomForest")
# install.packages("rJava")
# install.packages("reshape")
# install.packages("tictoc")

# clean all objects from workspace
rm(list=ls())

# setwd("~/git/PFPC/Source")
setwd("~/Work/git/Power_Failure_Prediction/Source")
# setwd("~/Repo/Frank/Power_Failure_Prediction/Source")

library(tidyverse)
library(rJava)
library(randomForest)
library(reshape)
library(tictoc)
source("./R/util.R")
source("./R/data.R")

# =================================================================================================

# gen_gust_info()
# gen_pole_info()
# gen_family_info()
# gen_meters_info()
# gen_village_info()
# gen_station_observation()
# merge_all_info()

# =================================================================================================
# Data pre-processing

# =================================================================================================

# md = data_pre_processing()
# 
# data = md$data
# info = md$info

# =================================================================================================

data = list()
data$train  = read.csv("./data/train.csv",  fileEncoding="UTF-8")
data$submit = read.csv("./data/submit.csv", fileEncoding="UTF-8")
data$all    = read.csv("./data/merged.csv", fileEncoding="UTF-8")

# =================================================================================================

info = list()
info$time_stamp = format(Sys.time(), "%m%d_%H%M%S")
info$tn_tp = c("Hagibis", "Chan.hom", "Dujuan", "Soudelor", "Fung.wong", "Matmo", "Nepartak", "MerantiAndMalakas")
info$ts_tp = c("NesatAndHaitang", "Megi")
info$tp = c(info$tn_tp, info$ts_tp)

features = c("pole1", "pole2", "pole3", "pole4", "pole5", "pole6", "pole7", "pole8", "pole9", "pole10")
features = c(features, "precp_total", "precp_day", "precp_24h", "precp_12h", "precp_6h", "precp_3h", "precp_1h", "wind", "gust")
features = c(features, "max_hh")
info$features = features

info$cities = levels(data$train$CityName)

info$col_tn = c(info$features, "outage")
info$row_tn = which(data$all$tp %in% info$tn_tp)
info$row_ts = which(data$all$tp %in% info$ts_tp)

data$tn = data$all[info$row_tn,]

# =================================================================================================

randomForest_city()

# =================================================================================================