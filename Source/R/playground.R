# install.packages("httr")
# install.packages("rvest")
# install.packages("xml2")
# install.packages("tidyverse")
# install.packages("xlsx")
# install.packages("randomForest")
# install.packages("rJava")

setwd("~/git/PFPC/Source")
# setwd("~/Work/git/Power_Failure_Prediction/Source")
# setwd("~/Repo/Frank/Power_Failure_Prediction/Source")

library(tidyverse)
library(rJava)
library(randomForest)
source("./R/util.R")

# =================================================================================================

pd_path <- paste(c(getwd(), "/prediction/"), collapse='')
if ( !dir.exists(pd_path) ) { 
    dir.create(pd_path) 
} 

# =================================================================================================

# gen_pole_info()
# gen_family_info()
# gen_gust_info()

# =================================================================================================

# 載入颱風停電戶資料
train  <- read.csv("./data/train.csv",  fileEncoding="UTF-8")
submit <- read.csv("./data/submit.csv", fileEncoding="UTF-8")
gust   <- read.csv("./data/gust.csv",   fileEncoding="UTF-8")
lastpd <- read.csv("./submit/57.33500_submit_dc_1020_061302.csv", fileEncoding="UTF-8")

# =================================================================================================
# Define column and row index

col_tn_tp   <- c("Hagibis", "Chan.hom", "Dujuan", "Soudelor", "Fung.wong", "Matmo", "Nepartak", "MerantiAndMalakas")
col_ts_tp   <- c("NesatAndHaitang", "Megi")
col_feature <- c("pole1", "pole2", "pole3", "pole4", "pole5", "pole6", "pole7", "pole8", "pole9", "pole10", "household", "maxWind", "gust")

fp_max <- apply(train[, col_tn_tp], 1, max)

tmp_sum <- rowSums(train[, col_tn_tp])
row_zero <- which(tmp_sum == 0)
row_none_zero <- which(tmp_sum > 0)

message(sprintf("total rows: %d, zero: %d, non-zero: %d", NROW(tmp_sum), NROW(row_zero), NROW(row_none_zero)))

# =================================================================================================

vil_raw <- gen_village_info(raw=train)

# =================================================================================================
# Data pre-processing

raw_tn <- gen_tp_raw(vil=vil_raw, gust=gust, tp=train, col_tp=col_tn_tp)
raw_ts <- gen_tp_raw(vil=vil_raw, gust=gust, tp=NULL,  col_tp=col_ts_tp, is_training=F)

# Building up random forest model
rf <- build_rf_model(raw_tn, col_feature=col_feature)

# =================================================================================================

soudelor_pred        <- gen_predict(model=rf$Soudelor, raw=raw_tn$Soudelor[col_feature],        row_zero=row_zero, row_max=fp_max)
meranti_pred         <- gen_predict(model=rf$Meranti,  raw=raw_tn$Meranti[col_feature],         row_zero=row_zero, row_max=fp_max)
megi_pred            <- gen_predict(model=rf$Soudelor, raw=raw_ts$Megi[col_feature],            row_zero=row_zero, row_max=fp_max, magic_value=1.45)
nesatAndHaitang_pred <- gen_predict(model=rf$Meranti,  raw=raw_ts$NesatAndHaitang[col_feature], row_zero=row_zero, row_max=fp_max, magic_value=1.53)

# =================================================================================================

soudelor_pred <- gen_predict(model=rf$Soudelor, raw=raw_tn$Soudelor[col_feature], row_zero=row_zero, row_max=fp_max, magic_value=1.53)
meranti_pred  <- gen_predict(model=rf$Meranti,  raw=raw_tn$Meranti[col_feature],  row_zero=row_zero, row_max=fp_max, magic_value=1.53)
tn_real       <- cbind(train$Soudelor, train$MerantiAndMalakas)
tn_pred       <- cbind(soudelor_pred, meranti_pred)
Scoring(tn_pred, tn_real)

# =================================================================================================

ts_real <- cbind(lastpd$NesatAndHaitang, lastpd$Megi)
ts_pred <- cbind(nesatAndHaitang_pred, megi_pred)
Scoring(ts_pred, ts_real)

# =================================================================================================

f_submit <- paste(c(pd_path, "submit_dc_", format(Sys.time(), "%m%d_%H%M%S"), ".csv"), collapse='')
submit_dc <- cbind(submit[1:4], nesatAndHaitang_pred) %>% cbind(megi_pred)
names(submit_dc)[5:6] <- c("NesatAndHaitang", "Megi")
write.csv(submit_dc, file=f_submit, row.names=FALSE, fileEncoding="UTF-8")
