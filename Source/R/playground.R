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
# Prediction and evaluation

bs <- c("Soudelor", "Megi")
tg <- c("MerantiAndMalakas", "NesatAndHaitang")
pair <- rbind(bs, tg)

pd <- damage_forecasting(
    model=rf,
    raw=data$tp,
    real=info$real,
    pair=pair,
    feature=info$feature,
    row_zero=info$row_zero,
    row_max=info$row_max,
    magic=info$magic)

gen_submit(train=data$train, submit=data$submit, pd=pd, en_train=T)

# =================================================================================================
