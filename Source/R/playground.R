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

md = data_pre_processing()

data = md$data
info = md$info

# =================================================================================================
# Building up random forest model
rf = build_rf_model(raw=data$tp, targets=info$tn_tp)

# =================================================================================================

# for (i in 1:length(info$tn_tp)) {
#     tp_name = info$tn_tp[i]
#     
#     real = info$real[[tp_name]]
#     pred = real
#     pred[t] = 0
#     
#     score = CM(real, pred)
#     message(sprintf("CM: %2.6f - %s", score, tp_name))
# }

# =================================================================================================
# Prediction and evaluation

tn_1 = c("Soudelor", "Soudelor")
tn_2 = c("MerantiAndMalakas", "MerantiAndMalakas")
pd = power_outage_forecasting(model=rf, raw=data$tp, real=info$real, pair=rbind(tn_1, tn_2))

ts_1 = c("Soudelor", "Megi")
ts_2 = c("MerantiAndMalakas", "NesatAndHaitang")
pd = power_outage_forecasting(model=rf, raw=data$tp, real=info$real, pair=rbind(ts_1, ts_2))
gen_submit(train=data$train, submit=data$submit, pd=pd, en_train=T)

# =================================================================================================

info$tn_tp
info$ts_tp

pair = NULL

bs = info$tn_tp
bs = c(info$tn_tp[5])
tg = info$tn_tp
tg = info$ts_tp
for (i in 1:length(bs)) {
    bs_name = bs[i]
    for (j in 1:length(tg)) {
        tg_name = tg[j]
        pair = rbind(pair, c(bs_name, tg_name))
        message(sprintf("%-20s, %-20s", bs_name, tg_name))
    }
}

pd = power_outage_forecasting(
    model=rf,
    raw=data$tp,
    real=info$real,
    pair=pair,
    feature=info$feature,
    row_zero=info$row_zero,
    row_max=info$row_max,
    magic=info$magic)
