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

# Building up random forest model
rf <- build_rf_model(raw=data$tp, scope=info$tn_tp, feature=info$feature, row_zero=info$row_zero, row_max=info$row_max)

# =================================================================================================

# Define column and row index

# =================================================================================================


# =================================================================================================
# Data pre-processing



# =================================================================================================

md <- list()
md[[1]] <- rf

names(md) <- c("rf")

md$rf$Soudelor
a <- "Soudelor"
b <- "Megi"
md$rf[[a]]

ts_tp   <- c("NesatAndHaitang", "Megi")

(b %in% ts_tp)

a <- list()

md[[1]] <- train
md[[2]] <- rf
md[[3]] <- magic
md[[4]] <- row_zero
md[[5]] <- fp_max

# =================================================================================================

a <- NULL
a <- c(a, 1)

damage_forecasting <- function(model, raw, real, pair, row_zero, row_max, magic) {
    
    tn_real <- NULL
    ts_real <- NULL
    tn_pred <- NULL
    ts_pred <- NULL
    
    for (i in 1:length(pair)) {
        bs <- pair[i][1]
        tg <- pair[i][2]
        
        tn_pd <- gen_predict(model=model[[bs]], raw=raw[[bs]][col_feature], row_zero=row_zero, row_max=fp_max, magic_value=1.53)
        ts_pd <- gen_predict(model=model[[bs]], raw=raw[[tg]][col_feature], row_zero=row_zero, row_max=fp_max, magic_value=magic[i])
        
        tn_real <- c(tn_real, real[bs])
        tn_pred <- c(tn_pred, tn_pd)
        ts_real <- c(tn_real, real[tg])
        ts_pred <- c(ts_pred, ts_pd)
    }
    
    Scoring(tn_pred, tn_real)
    Scoring(ts_pred, ts_real)
}

gen_predict <- function(model, raw, row_zero, row_max, magic_value=1) {
    pd <- predict(model, newdata=raw) * magic_value
    pd[row_zero] <- 0
    pd <- apply(cbind(row_max, pd), 1, min)
    
    return(pd)
}

magic <- 1.53




soudelor_pred        <- gen_predict(model=rf$Soudelor, raw=raw_tn$Soudelor[col_feature],        row_zero=row_zero, row_max=fp_max)
meranti_pred         <- gen_predict(model=rf$Meranti,  raw=raw_tn$Meranti[col_feature],         row_zero=row_zero, row_max=fp_max)
megi_pred            <- gen_predict(model=rf$Soudelor, raw=raw_ts$Megi[col_feature],            row_zero=row_zero, row_max=fp_max, magic_value=1.45)
nesatAndHaitang_pred <- gen_predict(model=rf$Meranti,  raw=raw_ts$NesatAndHaitang[col_feature], row_zero=row_zero, row_max=fp_max, magic_value=1.53)

# =================================================================================================

gen_targets_prediction <- function(model, raw, real, row_zero, row_max, magic_value) {

    # Evaluate model performance
    
    pd <- list()
    for (i in 1:len(target)) {
        model <- rf[model[i]]
        raw   <- 
        
        pd[[i]] <- gen_predict(model=rf[["Soudelor"]], raw=raw_ts$Megi[col_feature],            row_zero=row_zero, row_max=fp_max, magic_value=1.45)
    }
    rf[["Soudelor"]]
    
    soudelor_pred        <- gen_predict(model=rf$Soudelor, raw=raw_tn$Soudelor[col_feature],        row_zero=row_zero, row_max=fp_max)
    meranti_pred         <- gen_predict(model=rf$Meranti,  raw=raw_tn$Meranti[col_feature],         row_zero=row_zero, row_max=fp_max)
    megi_pred            <- gen_predict(model=rf$Soudelor, raw=raw_ts$Megi[col_feature],            row_zero=row_zero, row_max=fp_max, magic_value=1.45)
    nesatAndHaitang_pred <- gen_predict(model=rf$Meranti,  raw=raw_ts$NesatAndHaitang[col_feature], row_zero=row_zero, row_max=fp_max, magic_value=1.53)
    
    tn_real <- cbind(real$Soudelor, real$MerantiAndMalakas)
    tn_pred <- cbind(soudelor_pred, meranti_pred)
    Scoring(tn_pred, tn_real)
    
    # Generate predictions
    ts_real <- cbind(lastpd$NesatAndHaitang, lastpd$Megi)
    ts_pred <- cbind(nesatAndHaitang_pred, megi_pred)
    Scoring(ts_pred, ts_real)
    
    
}

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

pd_path <- paste(c(getwd(), "/prediction/"), collapse='')
if ( !dir.exists(pd_path) ) { 
    dir.create(pd_path) 
} 

f_submit <- paste(c(pd_path, "submit_dc_", format(Sys.time(), "%m%d_%H%M%S"), ".csv"), collapse='')
submit_dc <- cbind(submit[1:4], nesatAndHaitang_pred) %>% cbind(megi_pred)
names(submit_dc)[5:6] <- c("NesatAndHaitang", "Megi")
write.csv(submit_dc, file=f_submit, row.names=FALSE, fileEncoding="UTF-8")
