# 颱風停電預測挑戰賽
  https://dc.dsp.im/main/content/Typhoon-caused-Power-Outages-Prediction-Challenge

### Data pre-processing
* gen_pole_info()
  - Source  
    電桿資料 - 政府資料開放平台 - 台灣電力公司_電桿坐標及桿號  
    https://data.gov.tw/dataset/33305  
  - Inputs  
    ./data/poledata/*pole.csv    
  - Output  
    ./data/pole.csv

* gen_family_info()
  - Source  
    人口戶數資料 - 政府資料開放平臺 - 村里戶數、單一年齡人口
    https://data.gov.tw/dataset/32973#r0
  - Inputs  
    ./data/opendata10603M030.csv
  - Output  
    ./data/family.csv

* gen_meters_info()
  - Source
    用電戶數 - 台電 - 縣市住商用電資訊 - 各縣市村里售電資訊
    http://www.taipower.com.tw/content/announcement/ann01.aspx?BType=37
  - Inputs
    ./data/open_sell_amt_vil.csv
  - Output  
    ./data/meters.csv

* gen_village_info()
  - Inputs  
    ./data/train.csv  
    ./data/pole.csv  
    ./data/family.csv  
    ./data/meters.csv  
  - Output  
    ./data/village.csv

# Reference data
* 颱風資料 - 颱風資料庫
  - http://rdc28.cwb.gov.tw/

* 電桿資料 - 政府資料開放平台 - 台灣電力公司_電桿坐標及桿號
  - https://data.gov.tw/dataset/33305
  - ./data/poledata/*pole.csv


# Log
* 1103
  - Integate meters info
  - Only use villages which had power outage before to trian rf model

* 1028
  - Code restructure
  - Add function to build random forest model per city

* 1025
  - Code restructure
    data_preprocessing()
    collect_info()
    build_rf_model()
    damage_forecasting()
    gen_submit()

* 1023
  - Support to batch processing all typhoons info

* 1021
  - Set zero: Set the predicted value of a village to zero if the records of this village is always 0
  - Set upper bound: the predicted value should not exceed the maximum of the historical records

* 1019
  - Clean data - Pole (DONE: 1019)
  - Clean data - quts (DONE: 1019)
  - Add scoring function
  - Handle missing value after merge table (pole, household, set to zero)

* 1017
  - Create a github project and upload the source, and then share this project to Louis (DONE: 1017)

# Submit history
* 59.01400_submit_dc_1112_233124.csv
  - Add CWB observation data (town-level)
  - mix the prediction from 58.14500_submit_dc_1103_213554.csv as training data

* 58.14500_submit_dc_1103_213554.csv
  - Replace meters with household
  - Only use villages which had power outage before to trian rf model

* 57.60200_submit_dc_1026_032611.csv
  - set the prediction of a village to zero if this village is in 澎湖縣, 連江縣, 金門縣 (remove unrepresentative data)

* 57.44300_submit_dc_1024_165546.csv
  - random foreset, set ntree=5000

* 57.40700_submit_dc_1020_233555.csv
  - Set the upper bound of a prediction according to the power failure history

* 57.33500_submit_dc_1020_061302.csv
  - Cleaning partial family data (correct district / village name)
  - Set the prediction of a village to zero if this village has no record of power failure

* 56.67500_submit_dc_1018_1449.csv
  - Cleaning pole data (correct village name)
  - random foreset, set ntree=3000

* 56.55600_submit_dc_1015_vill.csv
  - Integrating village-level data of pole, family (instead of district-level)

