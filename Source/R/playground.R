# install.packages("httr")
# install.packages("rvest")
# install.packages("xml2")
# install.packages("tidyverse")
# install.packages("xlsx")
# install.packages("randomForest")
# install.packages("rJava")
# install.packages("reshape")

# clean all objects from workspace
rm(list=ls())

# setwd("~/git/PFPC/Source")
setwd("~/Work/git/Power_Failure_Prediction/Source")
# setwd("~/Repo/Frank/Power_Failure_Prediction/Source")

library(tidyverse)
library(rJava)
library(randomForest)
library(reshape)
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
info = list()
info$tn_tp = c("Hagibis", "Chan.hom", "Dujuan", "Soudelor", "Fung.wong", "Matmo", "Nepartak", "MerantiAndMalakas")
info$ts_tp = c("NesatAndHaitang", "Megi")
info$tp = c(info$tn_tp, info$ts_tp)

features = c("pole1", "pole2", "pole3", "pole4", "pole5", "pole6", "pole7", "pole8", "pole9", "pole10")
features = c(features, "precp_total", "precp_day", "precp_24h", "precp_12h", "precp_6h", "precp_3h", "precp_1h", "wind", "gust")
features = c(features, "max_hh")
info$features = features

info$city_ig = c("澎湖縣", "連江縣", "金門縣", "嘉義市")

# =================================================================================================

data = list()
data$train  = read.csv("./data/train.csv",  fileEncoding="UTF-8")
data$submit = read.csv("./data/submit.csv", fileEncoding="UTF-8")
data$all    = read.csv("./data/merged.csv", fileEncoding="UTF-8")

# =============================================================================================

info$row_max = apply(data$train[, info$tn_tp], 1, max)

row_sum  = rowSums(data$train[, info$tn_tp])
row_zero = which(row_sum == 0)
row_none_zero = which(row_sum > 0)

row_zero_village = which(data$train$CityName %in% info$city_ig)

# info$row_zero = row_zero
info$row_zero = unique(c(row_zero, row_zero_village))
info$row_none_zero = setdiff(row_none_zero, info$row_zero)
info$vil_sel  = data$train$VilCode[row_none_zero]

# =============================================================================================

colnames(data$all)

sel_cols = c(info$features, "outage")

raw_train = filter(data$all, tp == "Soudelor")
raw_train = raw_train[, sel_cols]
raw_all   = data$all[, sel_cols]

rf_tp = randomForest(outage~., data=raw_train, ntree=500)

real = raw_train$outage
pred = predict(rf_tp, newdata=raw_train[, info$features])
cm = CM(real, pred)
cm

rf_all = randomForest(outage~., data=raw_all, ntree=500)

real = raw_train$outage
pred = predict(rf_all, newdata=raw_train[, info$features])
cm = CM(real, pred)
cm

pred = list()
real = list()

for (tp_name in info$tp) {
    raw  = filter(data$all, tp == tp_name)
    raw  = raw[, sel_cols]
    real[[tp_name]] = raw$outage
    pred[[tp_name]] = predict(rf_all, newdata=raw[, info$features])
    cm   = CM(real[[tp_name]], pred[[tp_name]])
    message(sprintf("%20s: %2.6f", tp_name, cm))
}

save(rf_all, file="./rf_all.model")

pd = round(cbind(pred$NesatAndHaitang, pred$Megi))
head(pd)
colnames(pd) = info$ts_tp
colnames(data$submit)

gen_submit(submit=data$submit, pd=pd)
