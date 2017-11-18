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
# merge_all_info(f_last_submit="59.01400_submit_dc_1112_233124.csv")

# =================================================================================================

data = gen_data()
info = gen_info()

# =================================================================================================
# Build randomforest via all data
# randomForest_all()

# =================================================================================================
# randomForest_city()
# result = rbind(result, evaluate_per_type(model=model, type_name="typhoon", type_idx="tp",       type_set=info$tn_tp))    

md = randomForest_type(type_name="city", type_idx="grp_city", type_set=info$cities, outage_lv=0)
md = randomForest_type(type_name="town", type_idx="Towns",    type_set=info$towns,  outage_lv=0)
md = randomForest_type(type_name="tp",   type_idx="tp",       type_set=info$tn_tp,  outage_lv=0)
    

# =================================================================================================
md = randomForest_tp()
