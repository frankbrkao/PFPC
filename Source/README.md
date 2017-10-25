# Working items
* More Features
  - Download the detail of wind / rain info from 颱風資料庫(http://rdc28.cwb.gov.tw/)  

* Clean data
  - Family

* Data normalization
  - Predict the percenatge of failed household instead of # of failed household

* Train
  - Try: Only use village records which 
  - Try: xgboost
  - Implement cross validation

* Scoring
  - Try: Set the predicted value of a village to zero if the failed household is always 0 at this village

* Survey
  - Paper survey - features, learning method

# Log
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

