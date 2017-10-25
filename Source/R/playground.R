# install.packages("httr")
# install.packages("rvest")
# install.packages("xml2")
# install.packages("tidyverse")
# install.packages("xlsx")
# install.packages("randomForest")
# install.packages("rJava")

# clean all objects from workspace
rm(list=ls())

# setwd("~/git/PFPC/Source")
setwd("~/Work/git/Power_Failure_Prediction/Source")
# setwd("~/Repo/Frank/Power_Failure_Prediction/Source")

library(tidyverse)
library(rJava)
library(randomForest)
source("./R/util.R")

# =================================================================================================

# gen_gust_info()
# gen_pole_info()
# gen_family_info()

# =================================================================================================
# Data pre-processing

md <- data_pre_processing()

data <- md$data
info <- md$info

# =================================================================================================
# Building up random forest model
rf <- build_rf_model(
    raw=data$tp,
    scope=info$tn_tp,
    feature=info$feature,
    row_zero=info$row_zero,
    row_max=info$row_max)

# =================================================================================================

# for (i in 1:length(info$tn_tp)) {
#     tp_name <- info$tn_tp[i]
#     
#     real <- info$real[[tp_name]]
#     pred <- real
#     pred[t] <- 0
#     
#     score <- CM(real, pred)
#     message(sprintf("CM: %2.6f - %s", score, tp_name))
# }

# =================================================================================================
# Prediction and evaluation

bs_tn <- c("Soudelor", "Soudelor")
tg_tn <- c("MerantiAndMalakas", "MerantiAndMalakas")

bs_ts <- c("Soudelor", "Megi")
tg_ts <- c("MerantiAndMalakas", "NesatAndHaitang")

pd <- damage_forecasting(
    model=rf,
    raw=data$tp,
    real=info$real,
    pair=rbind(bs_tn, tg_tn),
    feature=info$feature,
    row_zero=info$row_zero,
    row_max=info$row_max,
    magic=info$magic)

pd <- damage_forecasting(
    model=rf,
    raw=data$tp,
    real=info$real,
    pair=rbind(bs_ts, tg_ts),
    feature=info$feature,
    row_zero=info$row_zero,
    row_max=info$row_max,
    magic=info$magic)

gen_submit(train=data$train, submit=data$submit, pd=pd, en_train=T)

# =================================================================================================
