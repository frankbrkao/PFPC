# =================================================================================================

data_pre_processing = function() {
    # 載入颱風停電戶資料

    data = list()
    data$train  = read.csv("./data/train.csv",  fileEncoding="UTF-8")
    data$submit = read.csv("./data/submit.csv", fileEncoding="UTF-8")
    data$lastpd = read.csv("./submit/57.60200_submit_dc_1026_032611.csv", fileEncoding="UTF-8")

    info = collect_info(train=data$train, lastpd=data$lastpd)

    # raw_village = gen_village_info(raw=data$train)
    raw_village = read.csv("./data/village.csv", fileEncoding="UTF-8")
    data$tp     = gen_tp_raw(village=raw_village, train=data$train, lastpd=data$lastpd, tn_tp=info$tn_tp, ts_tp=info$ts_tp)

    md      = list()
    md$data = data
    md$info = info

    return (md)
}

# =================================================================================================
# Define column and row index

collect_info = function(train, lastpd) {

    info = list()

    # info$tn_tp = c("Hagibis", "Chan.hom", "Dujuan", "Soudelor", "Fung.wong", "Matmo", "Nepartak", "MerantiAndMalakas")
    # info$tn_tp = c("Chan.hom", "Dujuan", "Soudelor", "Fung.wong", "Matmo", "Nepartak", "MerantiAndMalakas")
    info$tn_tp = c("Dujuan", "Soudelor", "Matmo", "Nepartak", "MerantiAndMalakas")
    info$ts_tp = c("NesatAndHaitang", "Megi")
    info$features = c("pole1", "pole2", "pole3", "pole4", "pole5", "pole6", "pole7", "pole8", "pole9", "pole10", "household", "maxWind", "gust")

    # info$city_ig = c("澎湖縣", "連江縣", "金門縣", "嘉義市", "新竹市")
    info$city_ig = c("澎湖縣", "連江縣", "金門縣", "嘉義市")

    # =============================================================================================

    info$row_max = apply(train[, info$tn_tp], 1, max)

    row_sum  = rowSums(train[, info$tn_tp])
    row_zero = which(row_sum == 0)
    row_none_zero = which(row_sum > 0)

    row_zero_village = which(train$CityName %in% info$city_ig)

    # info$row_zero = row_zero
    info$row_zero = unique(c(row_zero, row_zero_village))
    info$vil_sel  = data$train$VilCode[row_none_zero]
    
    message(sprintf("row_zero: %d, village_zero: %d", NROW(row_zero), NROW(row_zero_village)))
    message(sprintf("total rows: %d, zero: %d, non-zero: %d", NROW(row_sum), NROW(info$row_zero), NROW(row_none_zero)))

    # =============================================================================================

    col_selL = c("VilCode", info$tn_tp)
    col_selR = c("VilCode", info$ts_tp)
    tp_list = c(info$tn_tp, info$ts_tp)

    tp = left_join(train[col_selL], lastpd[col_selR], by="VilCode")
    info$real  = round(tp[tp_list], 0)
    info$magic = rep(1.57, length(tp_list))
    names(info$magic) = tp_list

    # =============================================================================================

    row_city = list()
    info$cities = setdiff(levels(train$City), info$city_ig)

    for (city in info$cities)
        row_city[[city]] = which(train$CityName == city)

    info$row_city = row_city

    return (info)
}

# =================================================================================================

gen_gust_info = function() {

    # 各颱風風力資料
    # 資料來源為颱風資料庫(http://rdc28.cwb.gov.tw/)
    library(xlsx)
    gust = xlsx::read.xlsx("./data/gust.xlsx", 1)
    names(gust)[1] = "CityName"
    write.csv(gust, file="./data/gust.csv", row.names=F, fileEncoding="UTF-8")
}

# =================================================================================================

gen_village_info = function(raw=train) {

    pole   = read.csv("./data/pole.csv",   fileEncoding="UTF-8", stringsAsFactors=F)
    family = read.csv("./data/family.csv", fileEncoding="UTF-8", stringsAsFactors=F)

    col_sel = c("CityName", "TownName", "VilName", "VilCode", "key")

    raw$key  = paste0(raw$CityName, raw$TownName, raw$VilName)
    raw = left_join(raw[col_sel], pole, by="key")
    raw = left_join(raw, family,  by="key")

    # TODO: Correct family info
    # Set the missing value to 0
    raw[is.na(raw)] = 0

    write.csv(raw, file="./data/village.csv", row.names=F, fileEncoding="UTF-8")

    return (raw)
}

# =================================================================================================

remove_tp_prefix = function (x) {
    if (regexpr("_", x)[1] == -1)
        return (x)
    else
        return( strsplit(x, "_")[[1]][2] )
}

gen_tp_raw = function(village, train, lastpd, tn_tp, ts_tp) {

    gust = read.csv("./data/gust.csv", fileEncoding="UTF-8")

    tp_list  = c(tn_tp, ts_tp)
    col_city = c("CityName")
    col_gust = colnames(gust)

    # =============================================================================================
    
    col_selL  = c("VilCode", tn_tp)
    col_selR  = c("VilCode", ts_tp)
    tp_outage = left_join(train[col_selL], lastpd[col_selR], by="VilCode")

    # =============================================================================================    
    
    raw = list()

    for (tp in tp_list) {

        col_tp_maxWind = paste0(tolower(tp), "_maxWind")
        col_tp_gust    = paste0(tolower(tp), "_gust")
        col_sel        = c(col_city, col_tp_maxWind, col_tp_gust)
        col_outage     = c("VilCode", tp)

        if (col_sel[2] %in% col_gust) {
            raw_tp = left_join(village, gust[, col_sel], by="CityName")
            names(raw_tp) = sapply(names(raw_tp), remove_tp_prefix, USE.NAMES=FALSE)

            raw_tp = left_join(raw_tp, tp_outage[, col_outage], by="VilCode")
            colnames(raw_tp)[colnames(raw_tp) == tp] = "n_outage"
            
            raw[[tp]] = raw_tp
        } else {
            message(paste0("[W] No info in table gust, skip: ", tp))
        }
    }

    return (raw)
}

# =================================================================================================

build_rf_model = function(raw, targets) {

    features = info$features
    
    rf = list()
    col_real = c("n_outage")
    col_sel  = c(features, col_real)
    
    for (tp in targets) {
        tp_data = raw[[tp]]
        
        if ( is.null(tp_data) ) {
            rf[[tp]] = NULL
        } else {
            rf[[tp]] = randomForest(n_outage~., data=tp_data[, col_sel], ntree=500)
            real = tp_data[, col_real]
            pred = gen_predict(model=rf[[tp]], raw=tp_data[, features], magic_value=1)
            cm = CM(real, pred)
            message(sprintf("Training score: %2.6f - %s", cm, tp))
        }
    }
    
    return (rf)
}

# =================================================================================================

build_rf_model_alldata = function(raw, targets) {

    features = info$features
    vil_sel  = info$vil_sel

    rf = list()
    col_real = c("n_outage")
    col_sel  = c(features, col_real)

    tn <- NULL

    for (tp in targets) {
        tp_data = raw[[tp]]
        tn = rbind(tn, tp_data[, col_sel])
    }

    data_vil = as.data.frame(vil_sel)
    names(data_vil) = c("VilCode")

    data_sel = inner_join(tp_data, data_vil, by="VilCode")
    rf = randomForest(n_outage~., data=data_sel[, col_sel], ntree=500)

    for (tp in targets) {
        tp_data = raw[[tp]]
        real = tp_data[, col_real]
        pred = gen_predict(model=rf, raw=tp_data[, features], magic_value=1)
        cm = CM(real, pred)
        message(sprintf("Training score: %2.6f - %s", cm, tp))
    }

    return (rf)
}

# =================================================================================================

build_rf_city_model = function(raw, targets) {

    features = info$features
    col_sel  = c(features, "n_outage")

    for (tp in targets) {
        rf[[tp]] = randomForest_city(raw=raw[[tp]][, col_sel], target=tp)
    }
}

randomForest_city = function(raw, target) {

    cities   = info$cities
    row_city = info$row_city
    features = info$features
    row_zero = info$row_zero

    col_real = c("n_outage")
    col_sel  = c(features, col_real)

    # target="Dujuan"
    # raw=data$tp[[target]][, col_sel]

    rf_city  = list()
    score    = NULL
    real     = raw[, col_real]
    pred     = rep(0, length(real))

    for (city in cities) {
        # city = cities[1]
        row_sel   = row_city[[city]]
        row_none_zero = setdiff(row_sel, row_zero)

        if (length(row_none_zero) > 15) {
            # rf_city[[city]] = randomForest(n_outage~., raw[row_sel, col_sel])
            rf_city[[city]] = randomForest(n_outage~., data=raw[row_none_zero, col_sel])
            pred[row_sel] = predict(rf_city[[city]], newdata=raw[row_sel, features])
        }

        if (sum(real[row_sel]) > 0) {
            cm = CM(real[row_sel], pred[row_sel])
        } else {
            cm = 1
        }

        score = c(score, cm)
        # message(sprintf("%s - %s: none zero: %d, sum: %2.8f", target, city, length(row_none_zero), sum(real[row_sel])))
        message(sprintf("%s - %s: %2.6f", target, city, cm))
    }

    message(sprintf("Avg city score: %2.6f", mean(score)))

    pred = round(pred, 0)
    pred = apply(cbind(info$row_max, pred), 1, min)
    pred[info$row_zero] = 0
    cm    = CM(real, pred)
    score = c(score, cm)

    message(sprintf("Training score: %2.6f", cm))

    return (rf_city)
}

# =================================================================================================

#  Morisita Similarity (CM)
CM = function(x, y) {
    # Check variable type
    x = as.numeric(x)
    y = as.numeric(y)

    x = ifelse(x < 0, 0, x)
    y = ifelse(y < 0, 0, y)

    # The formula
    sim = 2*sum(x*y)/(sum(x^2+y^2))

    return(sim)
}

scoring = function(real, pred, bs, tg) {

    score = NULL

    for ( i in 1:ncol(real) ) {
        cm = CM(real[, i], pred[, i])
        bs_name = bs[i]
        tg_name = tg[i]
        message(sprintf("score: %2.12f - bs: %-20s, tg: %-20s", cm, bs_name, tg_name))
        score = c(score, cm)
    }

    score = mean(score)
    score = ifelse(score < 0, 0, score) * 100
    message(sprintf("final: %2.12f", score))
}

gen_predict = function(model, raw, magic_value=1) {
    
    row_zero = info$row_zero
    row_max  = info$row_max
    
    pd = predict(model, newdata=raw) * magic_value
    pd = round(pd, 0)
    pd[row_zero] = 0
    pd = apply(cbind(row_max, pd), 1, min)

    return(pd)
}

# =================================================================================================

power_outage_forecasting = function(model, raw, real, pair) {

    features = info$features
    magic    = info$magic
    
    # tn_real = NULL
    ts_real = NULL
    # tn_pred = NULL
    ts_pred = NULL
    
    for (i in 1:nrow(pair)) {

        bs = pair[i, 1]
        tg = pair[i, 2]

        # tn_pd = gen_predict(model=model[[bs]], raw=raw[[bs]][features], magic_value=magic[bs])
        ts_pd = gen_predict(model=model[[bs]], raw=raw[[tg]][features], magic_value=magic[tg])

        # tn_real = cbind(tn_real, real[[bs]])
        # tn_pred = cbind(tn_pred, tn_pd)
        ts_real = cbind(ts_real, real[[tg]])
        ts_pred = cbind(ts_pred, ts_pd)
    }

    # colnames(tn_pred) = pair[,1]
    colnames(ts_pred) = pair[,2]

    # scoring(tn_real, tn_pred)
    scoring(ts_real, ts_pred, pair[,1], pair[,2])

    return(as.data.frame(ts_pred))
}

# =================================================================================================

gen_submit = function(train, submit, pd, en_train=F) {

    submit = data$submit
    col_submit = c("CityName", "TownName", "VilCode", "VilName")

    pd_path = paste(c(getwd(), "/prediction/"), collapse='')
    if ( !dir.exists(pd_path) ) {
        dir.create(pd_path)
    }

    f_submit = paste(c(pd_path, "submit_dc_", format(Sys.time(), "%m%d_%H%M%S"), ".csv"), collapse='')
    submit_dc = cbind(submit[col_submit], pd["NesatAndHaitang"], pd["Megi"])
    write.csv(submit_dc, file=f_submit, row.names=FALSE, fileEncoding="UTF-8")
    message(sprintf("save to %s", f_submit))

    if (!en_train)
        return ("")

    # f_train: for the purpose of analysis
    f_train  = paste(c(pd_path, "submit_dc_", format(Sys.time(), "%m%d_%H%M%S"), "_train.csv"), collapse='')
    train_dc = cbind(train, pd["NesatAndHaitang"], pd["Megi"])
    write.csv(train_dc, file=f_train, row.names=FALSE, fileEncoding="UTF-8")
}

# =================================================================================================

gen_pole_info = function() {

    # =============================================================================================

    # 加入電桿資料
    # 資料來源為政府資料開放平台(https://data.gov.tw/dataset/33305)
    pole_files = c("北北區處pole.csv",    "嘉義區處pole.csv",     "澎湖區處pole.csv",
                   "北南區處pole.csv",    "基隆區處pole.csv",     "花蓮區處pole.csv",
                   "北市區處pole.csv",    "宜蘭區處pole.csv",     "苗栗區處pole.csv",
                   "北西區處pole.csv",    "屏東區處pole.csv",     "金門區處pole.csv",
                   "南投區處pole.csv",    "彰化區處pole.csv",     "雲林區處pole.csv",
                   "台中區處pole.csv",    "新營區處pole.csv",     "馬祖區處pole.csv",
                   "台南區處pole.csv",    "新竹區處pole.csv",     "高雄區處pole.csv",
                   "台東區處pole.csv",    "桃園區處pole.csv",     "鳳山區處pole.csv")

    pole_wd = c()
    for (i in 1:length(pole_files)) {
        f_pole = pole_files[i]
        pole_wd[i] = paste0("./data/poledata/", f_pole)
    }

    pole = list()

    for (i in 1:length(pole_wd)) {
        pole[[i]] = read.csv(pole_wd[i], header=T, stringsAsFactors=F, sep="\t")
        pole[[i]] = pole[[i]][c("縣市", "行政區", "村里", "型式")]
    }

    pole = Reduce(x=pole, f=rbind)

    #清理電桿資料
    pole$縣市   = as.factor(pole$縣市)
    pole$行政區 = as.factor(pole$行政區)
    pole$村里   = as.factor(pole$村里)
    pole$型式   = as.factor(pole$型式)

    levels(pole$縣市)   = gsub(x=levels(pole$縣市), pattern="臺", replacement="台")
    levels(pole$縣市)   = gsub(x=levels(pole$縣市), pattern="台東", replacement="臺東")
    levels(pole$行政區) = gsub(x=levels(pole$行政區), pattern="頭份鎮", replacement="頭份市")

    pole$key = paste0(pole$縣市, pole$行政區, pole$村里) %>% as.factor()

    # ==== Correct village info ===================================================================

    levels(pole$key) = gsub(x=levels(pole$key), pattern="南投縣名間鄉部下村", replacement="南投縣名間鄉廍下村")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="南投縣竹山鎮回瑤里", replacement="南投縣竹山鎮硘磘里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="台中市北屯區部子里", replacement="台中市北屯區廍子里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="台中市外埔區部子里", replacement="台中市外埔區廍子里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="台中市大安區龜殼里", replacement="台中市大安區龜壳村")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="台中市大肚區蔗部里", replacement="台中市大肚區蔗廍里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="台中市清水區慷榔里", replacement="台中市清水區槺榔里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="台中市西區公館里",   replacement="台中市西區公舘里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="台北市萬華區糖部里", replacement="台北市萬華區糖廍里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="台南市七股區慷榔里", replacement="台南市七股區槺榔里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="台南市七股區鹽埕里", replacement="台南市七股區塩埕里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="台南市佳里區頂部里", replacement="台南市佳里區頂廍里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="台南市安南區鹽田里", replacement="台南市安南區塩田里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="台南市官田區南部里", replacement="台南市官田區南廍里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="台南市山上區玉峰里", replacement="台南市山上區玉峯里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="台南市後壁區後部里", replacement="台南市後壁區後廍里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="台南市新化區山腳里", replacement="台南市新化區山脚里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="台南市新化區那拔里", replacement="台南市新化區𦰡拔里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="台南市新營區舊部里", replacement="台南市新營區舊廍里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="台南市永康區鹽洲里", replacement="台南市永康區塩洲里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="台南市永康區鹽行里", replacement="台南市永康區塩行里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="台南市西港區羨林里", replacement="台南市西港區檨林里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="台南市麻豆區寮部里", replacement="台南市麻豆區寮廍里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="台南市龍崎區石曹里", replacement="台南市龍崎區石𥕢里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="嘉義市西區磚瑤里",   replacement="嘉義市西區磚磘里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="嘉義縣中埔鄉石弄村", replacement="嘉義縣中埔鄉石硦村")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="嘉義縣中埔鄉鹽館村", replacement="嘉義縣中埔鄉塩館村")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="嘉義縣朴子市雙溪里", replacement="嘉義縣朴子市双溪里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="嘉義縣梅山鄉瑞峰村", replacement="嘉義縣梅山鄉瑞峯村")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="嘉義縣梅山鄉雙溪村", replacement="嘉義縣梅山鄉双溪村")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="嘉義縣民雄鄉雙福村", replacement="嘉義縣民雄鄉双福村")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="嘉義縣竹崎鄉文峰村", replacement="嘉義縣竹崎鄉文峯村")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="宜蘭縣宜蘭市中正里", replacement="宜蘭縣宜蘭市東門里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="宜蘭縣宜蘭市和睦里", replacement="宜蘭縣宜蘭市神農里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="宜蘭縣宜蘭市大東里", replacement="宜蘭縣宜蘭市大新里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="宜蘭縣宜蘭市大道里", replacement="宜蘭縣宜蘭市南門里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="宜蘭縣宜蘭市慶和里", replacement="宜蘭縣宜蘭市北門里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="宜蘭縣宜蘭市新興里", replacement="宜蘭縣宜蘭市大新里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="宜蘭縣宜蘭市昇平里", replacement="宜蘭縣宜蘭市新民里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="宜蘭縣宜蘭市民生里", replacement="宜蘭縣宜蘭市新民里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="宜蘭縣宜蘭市鄂王里", replacement="宜蘭縣宜蘭市西門里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="屏東縣新園鄉瓦瑤村", replacement="屏東縣新園鄉瓦磘村")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="屏東縣東港鎮下部里", replacement="屏東縣東港鎮下廍里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="屏東縣林邊鄉崎峰村", replacement="屏東縣林邊鄉崎峯村")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="屏東縣滿州鄉響林村", replacement="屏東縣滿州鄉响林村")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="屏東縣瑪家鄉涼山村", replacement="屏東縣瑪家鄉凉山村")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="屏東縣萬丹鄉廈北村", replacement="屏東縣萬丹鄉厦北村")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="屏東縣萬丹鄉廈南村", replacement="屏東縣萬丹鄉厦南村")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="屏東縣里港鄉三部村", replacement="屏東縣里港鄉三廍村")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="屏東縣霧臺鄉霧台村", replacement="屏東縣霧臺鄉霧臺村")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="彰化縣二水鄉上豐村", replacement="彰化縣二水鄉上豊村")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="彰化縣員林市大峰里", replacement="彰化縣員林市大峯里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="彰化縣埔心鄉南館村", replacement="彰化縣埔心鄉南舘村")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="彰化縣埔心鄉埤腳村", replacement="彰化縣埔心鄉埤脚村")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="彰化縣埔心鄉新館村", replacement="彰化縣埔心鄉新舘村")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="彰化縣埔心鄉舊館村", replacement="彰化縣埔心鄉舊舘村")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="彰化縣埔鹽鄉瓦瑤村", replacement="彰化縣埔鹽鄉瓦磘村")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="彰化縣埔鹽鄉部子村", replacement="彰化縣埔鹽鄉廍子村")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="彰化縣彰化市下部里", replacement="彰化縣彰化市下廍里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="彰化縣彰化市寶部里", replacement="彰化縣彰化市寶廍里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="彰化縣彰化市磚瑤里", replacement="彰化縣彰化市磚磘里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="彰化縣芳苑鄉頂部村", replacement="彰化縣芳苑鄉頂廍村")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="新北市三峽區永館里", replacement="新北市三峽區永舘里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="新北市中和區灰瑤里", replacement="新北市中和區灰磘里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="新北市中和區瓦瑤里", replacement="新北市中和區瓦磘里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="新北市土城區峰廷里", replacement="新北市土城區峯廷里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="新北市坪林區石曹里", replacement="新北市坪林區石𥕢里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="新北市新店區五峰里", replacement="新北市新店區五峯里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="新北市板橋區公館里", replacement="新北市板橋區公舘里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="新北市樹林區槍寮里", replacement="新北市樹林區獇寮里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="新北市永和區新部里", replacement="新北市永和區新廍里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="新北市瑞芳區濂新里", replacement="新北市瑞芳區濓新里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="新北市瑞芳區濂洞里", replacement="新北市瑞芳區濓洞里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="新北市瑞芳區爪峰里", replacement="新北市瑞芳區爪峯里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="新北市萬里區崁腳里", replacement="新北市萬里區崁脚里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="新竹縣北埔鄉水砌村", replacement="新竹縣北埔鄉水磜村")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="新竹縣竹東鎮上館里", replacement="新竹縣竹東鎮上舘里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="新竹縣竹東鎮雞林里", replacement="新竹縣竹東鎮鷄林里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="桃園市大園區果林里", replacement="桃園市大園區菓林里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="桃園市新屋區慷榔里", replacement="桃園市新屋區槺榔里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="桃園市蘆竹區大華里", replacement="桃園市龜山區大華里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="澎湖縣湖西鄉果葉村", replacement="澎湖縣湖西鄉菓葉村")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="澎湖縣馬公市時裡里", replacement="澎湖縣馬公市嵵裡里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="臺東縣綠島鄉公館村", replacement="臺東縣綠島鄉公舘村")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="臺東縣達仁鄉台板村", replacement="臺東縣達仁鄉台坂村")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="臺東縣達仁鄉土板村", replacement="臺東縣達仁鄉土坂村")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="臺東縣關山鎮里龍里", replacement="臺東縣關山鎮里壠里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="苗栗縣三義鄉雙湖村", replacement="苗栗縣三義鄉双湖村")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="苗栗縣三義鄉雙潭村", replacement="苗栗縣三義鄉双潭村")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="苗栗縣竹南鎮公館里", replacement="苗栗縣竹南鎮公舘里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="苗栗縣苑裡鎮上館里", replacement="苗栗縣苑裡鎮上舘里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="苗栗縣苑裡鎮山腳里", replacement="苗栗縣苑裡鎮山脚里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="連江縣北竿鄉板里村", replacement="連江縣北竿鄉坂里村")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="雲林縣元長鄉瓦瑤村", replacement="雲林縣元長鄉瓦磘村")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="雲林縣北港鎮公館里", replacement="雲林縣北港鎮公舘里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="雲林縣口湖鄉台子村", replacement="雲林縣口湖鄉臺子村")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="雲林縣四湖鄉柏子村", replacement="雲林縣四湖鄉萡子村")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="雲林縣四湖鄉柏東村", replacement="雲林縣四湖鄉萡東村")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="雲林縣斗六市崙峰里", replacement="雲林縣斗六市崙峯里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="雲林縣水林鄉舊埔村", replacement="雲林縣水林鄉瓊埔村")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="雲林縣臺西鄉台西村", replacement="雲林縣臺西鄉臺西村")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="雲林縣西螺鎮公館里", replacement="雲林縣西螺鎮公舘里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="雲林縣麥寮鄉瓦瑤村", replacement="雲林縣麥寮鄉瓦磘村")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="高雄市左營區復興里", replacement="高雄市左營區永清里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="高雄市左營區部北里", replacement="高雄市左營區廍北里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="高雄市左營區部南里", replacement="高雄市左營區廍南里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="高雄市湖內區公館里", replacement="高雄市湖內區公舘里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="高雄市阿蓮區峰山里", replacement="高雄市阿蓮區峯山里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="高雄市鳥松區帝埔里", replacement="高雄市鳥松區坔埔里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="高雄市鳳山區海風里", replacement="高雄市鳳山區海光里")
    levels(pole$key) = gsub(x=levels(pole$key), pattern="高雄市鳳山區誠正里", replacement="高雄市鳳山區生明里")

    # =============================================================================================

    lv_key = levels(pole$key)
    row_t1 = grep(x=lv_key, pattern="雲林縣水林鄉")
    row_t2 = grep(x=lv_key, pattern="埔村")
    row_t3 = grep(x=lv_key, pattern="雲林縣水林鄉春埔村")
    row_t4 = grep(x=lv_key, pattern="雲林縣水林鄉海埔村")
    row_t5 = intersect(row_t1, row_t2)
    row_t5 = setdiff(row_t5, row_t3)
    row_t5 = setdiff(row_t5, row_t4)

    levels(pole$key) = gsub(x=levels(pole$key), pattern=lv_key[row_t5], replacement="雲林縣水林鄉瓊埔村")

    # =============================================================================================

    pole_type = group_by(pole, key, 型式) %>%
        summarise(n=n()) %>%
        ungroup()

    pole_type = pole_type[apply(pole_type, 1, function(x) grepl("縣", x) || grepl("市", x)),]
    pole_type = spread(pole_type, key=key, value=n, fill=0)
    pole_type = t(pole_type[,-1]) %>% as.data.frame()

    names(pole_type) = c("型式", "pole1", "pole2", "pole3", "pole4", "pole5", "pole6", "pole7", "pole8", "pole9", "pole10")
    # 後面讀取中文欄位有錯，先將電桿種類的欄位都改用英文
    # 原資料為："3T桿", "H桿", "木併桿", "木桿", "水泥併桿",
    # "水泥桿", "用戶自備桿", "鋼併桿", "鋼桿", "電塔"

    pole_type$key = rownames(pole_type)
    pole_type = pole_type[, -1]
    write.csv(pole_type, file="./data/pole.csv", row.names=F, fileEncoding="UTF-8")

    # ==== checking missing recorders =============================================================
    #
    # train  = read.csv("./data/train.csv",  fileEncoding="UTF-8")
    # train$key  = paste0(train$CityName, train$TownName, train$VilName)
    #
    # missing_r = right_join(train, pole_type, by="key")
    # missing_r = missing_r[is.na(missing_r$CityName),]
    # missing_r
}

gen_family_info = function() {

    # 加入人口戶數資料
    # 資料來源為政府資料開放平臺(https://data.gov.tw/dataset/32973#r0)

    family = read.csv("./data/opendata10603M030.csv", stringsAsFactors=F, fileEncoding="UTF-8-BOM")
    # family[263,3]

    family = family[-1, c("site_id", "village", "household_no")]
    family$site_id = gsub(x=family$site_id, pattern="　",           replacement="")
    family$site_id = gsub(x=family$site_id, pattern="臺北",         replacement="台北")
    family$site_id = gsub(x=family$site_id, pattern="臺中",         replacement="台中")
    family$site_id = gsub(x=family$site_id, pattern="臺南",         replacement="台南")
    family$site_id = gsub(x=family$site_id, pattern="高雄市三民一", replacement="高雄市三民區")
    family$site_id = gsub(x=family$site_id, pattern="高雄市三民二", replacement="高雄市三民區")
    family$site_id = gsub(x=family$site_id, pattern="高雄市鳳山一", replacement="高雄市鳳山區")
    family$site_id = gsub(x=family$site_id, pattern="高雄市鳳山二", replacement="高雄市鳳山區")
    family$key = paste0(family$site_id, family$village)

    family$key = gsub(x=family$key, pattern="台北市信義區富台里",   replacement="台北市信義區富臺里")
    family$key = gsub(x=family$key, pattern="台南市麻豆區晋江里",   replacement="台南市麻豆區晉江里")
    family$key = gsub(x=family$key, pattern="新竹縣竹北市中崙里",   replacement="新竹縣竹北市斗崙里")
    family$key = gsub(x=family$key, pattern="彰化縣彰化市南瑶里",   replacement="彰化縣彰化市南瑤里")
    family$key = gsub(x=family$key, pattern="雲林縣臺西鄉台西村",   replacement="雲林縣臺西鄉臺西村")
    family$key = gsub(x=family$key, pattern="雲林縣口湖鄉台子村",   replacement="雲林縣口湖鄉臺子村")
    family$key = gsub(x=family$key, pattern="嘉義縣中埔鄉塩舘村",   replacement="嘉義縣中埔鄉塩館村")
    family$key = gsub(x=family$key, pattern="屏東縣霧臺鄉霧台村",   replacement="屏東縣霧臺鄉霧臺村")

    family$household_no = as.character(family$household_no) %>% as.numeric()
    family_grp = group_by(family, key) %>% summarise(household=sum(household_no))
    write.csv(family_grp, file="./data/family.csv", row.names=F, fileEncoding="UTF-8")

    # TODO: 17 missing recorders
    # ==== checking missing recorders =============================================================
    #
    # train  = read.csv("./data/train.csv",  fileEncoding="UTF-8")
    # train$key  = paste0(train$CityName, train$TownName, train$VilName)
    #
    # missing_r = right_join(train, family, by="key")
    # missing_r = missing_r[is.na(missing_r$CityName),]
    # missing_r
}

gen_meters = function() {

    # 用電戶數 台電 - 縣市住商用電資訊 - 各縣市村里售電資訊
    # http://www.taipower.com.tw/content/announcement/ann01.aspx?BType=37
    # ./data/open_sell_amt_vil.csv

    meters = read.csv("./data/open_sell_amt_vil.csv", stringsAsFactors=T, fileEncoding="UTF-8-BOM")

    colnames(meters) <- c("date", "city", "town", "village", "electric energe sale", "meters")
    meters$"electric energe sale" = NULL

    levels(meters$city)    = gsub(x=levels(meters$city),    pattern=" ", replacement="")
    levels(meters$town)    = gsub(x=levels(meters$town),    pattern=" ", replacement="")
    levels(meters$village) = gsub(x=levels(meters$village), pattern=" ", replacement="")

    levels(meters$city)    = gsub(" ",  "", levels(meters$city))
    levels(meters$town)    = gsub(" ",  "", levels(meters$town))
    levels(meters$town)    = gsub("　", "", levels(meters$town))
    levels(meters$village) = gsub(" ",  "", levels(meters$village))

    lv_date = levels(meters$date)
    lv_date = gsub("年", "", lv_date)
    lv_date = gsub("月", "", lv_date)
    lv_date = as.integer(lv_date)
    levels(meters$date) = lv_date 

   row_even = which(lv_date %% 2 == 0)

    meters$date2 = meters$date
    levels(meters$date2)[row_even] = lv_date[row_even] - 1

    # =============================================================================================

    levels(meters$city) = gsub(x=levels(meters$city), pattern="臺北",   replacement="台北")
    levels(meters$city) = gsub(x=levels(meters$city), pattern="臺中",   replacement="台中")
    levels(meters$city) = gsub(x=levels(meters$city), pattern="臺南",   replacement="台南")

    levels(meters$town) = gsub(x=levels(meters$town), pattern="員林鎮", replacement="員林市")
    levels(meters$town) = gsub(x=levels(meters$town), pattern="頭份鎮", replacement="頭份市")

    levels(meters$village) = gsub(x=levels(meters$village), pattern="高明里　", replacement="高明里")

    meters = meters[-c(grep(x=meters$village, pattern="無法分類")),]
    meters$key = paste0(meters$city, meters$town, meters$village)
    meters$key = as.factor(meters$key)

    # =============================================================================================

    levels(meters$key) = gsub(x=levels(meters$key), pattern="台中市大安區龜売里", replacement="台中市大安區龜壳村")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="台中市西區公館里", replacement="台中市西區公舘里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="台北市信義區富台里",   replacement="台北市信義區富臺里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="台南市新化區山腳里", replacement="台南市新化區山脚里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="台南市新化區那拔里", replacement="台南市新化區𦰡拔里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="台南市永康區鹽洲里", replacement="台南市永康區塩洲里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="台南市麻豆區晋江里",   replacement="台南市麻豆區晉江里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="宜蘭縣宜蘭市中正里", replacement="宜蘭縣宜蘭市東門里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="宜蘭縣宜蘭市和睦里", replacement="宜蘭縣宜蘭市神農里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="宜蘭縣宜蘭市大東里", replacement="宜蘭縣宜蘭市大新里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="宜蘭縣宜蘭市大道里", replacement="宜蘭縣宜蘭市南門里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="宜蘭縣宜蘭市慶和里", replacement="宜蘭縣宜蘭市北門里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="宜蘭縣宜蘭市新興里", replacement="宜蘭縣宜蘭市大新里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="宜蘭縣宜蘭市昇平里", replacement="宜蘭縣宜蘭市新民里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="宜蘭縣宜蘭市民生里", replacement="宜蘭縣宜蘭市新民里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="宜蘭縣宜蘭市鄂王里", replacement="宜蘭縣宜蘭市西門里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="屏東縣瑪家鄉涼山村", replacement="屏東縣瑪家鄉凉山村")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="屏東縣萬丹鄉廈北村", replacement="屏東縣萬丹鄉厦北村")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="屏東縣萬丹鄉廈南村", replacement="屏東縣萬丹鄉厦南村")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="屏東縣霧臺鄉霧台村", replacement="屏東縣霧臺鄉霧臺村")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="彰化縣埔心鄉南館村", replacement="彰化縣埔心鄉南舘村")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="彰化縣埔心鄉埤腳村", replacement="彰化縣埔心鄉埤脚村")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="彰化縣埔心鄉新館村", replacement="彰化縣埔心鄉新舘村")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="彰化縣埔心鄉舊館村", replacement="彰化縣埔心鄉舊舘村")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="彰化縣埔鹽鄉瓦廍村", replacement="彰化縣埔鹽鄉瓦磘村")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="新北市三峽區永館里", replacement="新北市三峽區永舘里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="新北市坪林區石曹里", replacement="新北市坪林區石𥕢里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="新北市新店區五峰里", replacement="新北市新店區五峯里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="新北市板橋區公館里", replacement="新北市板橋區公舘里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="新北市樹林區猐寮里", replacement="新北市樹林區獇寮里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="新北市永和區新部里", replacement="新北市永和區新廍里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="新北市瑞芳區濂新里", replacement="新北市瑞芳區濓新里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="新北市瑞芳區濂洞里", replacement="新北市瑞芳區濓洞里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="新北市萬里區崁腳里", replacement="新北市萬里區崁脚里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="新竹縣竹東鎮上館里", replacement="新竹縣竹東鎮上舘里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="新竹縣竹東鎮雞林里", replacement="新竹縣竹東鎮鷄林里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="桃園市蘆竹區大華里", replacement="桃園市龜山區大華里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="臺東縣綠島鄉公館村", replacement="臺東縣綠島鄉公舘村")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="苗栗縣竹南鎮公館里", replacement="苗栗縣竹南鎮公舘里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="苗栗縣苑裡鎮上館里", replacement="苗栗縣苑裡鎮上舘里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="苗栗縣苑裡鎮山腳里", replacement="苗栗縣苑裡鎮山脚里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="雲林縣北港鎮公館里", replacement="雲林縣北港鎮公舘里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="雲林縣西螺鎮公館里", replacement="雲林縣西螺鎮公舘里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="高雄市左營區復興里", replacement="高雄市左營區永清里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="高雄市湖內區公館里", replacement="高雄市湖內區公舘里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="高雄市鳳山區海風里", replacement="高雄市鳳山區海光里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="高雄市鳳山區誠正里", replacement="高雄市鳳山區生明里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="宜蘭縣蘇澳鎮岳明里", replacement="宜蘭縣蘇澳鎮港邊里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="桃園市中壢區後興里", replacement="桃園市中壢區復興里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="桃園市中壢區興和里", replacement="桃園市中壢區興合里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="桃園市八德區竹圍里", replacement="桃園市八德區竹園里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="桃園市大溪區人文里", replacement="桃園市大溪區仁文里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="桃園市平鎮區振興里", replacement="桃園市平鎮區鎮興里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="桃園市平鎮區舊明里", replacement="桃園市中壢區舊明里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="桃園市桃園區中杉里", replacement="桃園市桃園區中山里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="桃園市桃園區大竹里", replacement="桃園市蘆竹區大竹里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="桃園市桃園區大竹里", replacement="桃園市蘆竹區大竹里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="桃園市桃園區廣龍里", replacement="桃園市八德區廣隆里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="桃園市桃園區楓樹里", replacement="桃園市龜山區楓樹里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="桃園市桃園區汴州里", replacement="桃園市桃園區汴洲里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="桃園市桃園區汴州里", replacement="桃園市桃園區汴洲里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="桃園市桃園區清溪里", replacement="桃園市桃園區青溪里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="桃園市龍潭區仁安里", replacement="桃園市龍潭區佳安里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="桃園市龍潭區仁安里", replacement="桃園市龍潭區佳安里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="桃園市龍潭區大坪里", replacement="桃園市龍潭區大平里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="桃園市龍潭區太平里", replacement="桃園市龍潭區大平里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="桃園市龍潭區東勢里", replacement="桃園市平鎮區東勢里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="桃園市龍潭區東安里", replacement="桃園市平鎮區東安里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="桃園市龜山區坑口里", replacement="桃園市蘆竹區坑口里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="金門縣金城鎮光前里", replacement="金門縣金沙鎮光前里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="金門縣金寧鄉瓊林里", replacement="金門縣金湖鎮瓊林里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="金門縣金寧鄉賢庵里", replacement="金門縣金城鎮賢庵里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="桃園市中壢區中福里", replacement="桃園市蘆竹區中福里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="桃園市中壢區南興里", replacement="桃園市大溪區南興里")
    levels(meters$key) = gsub(x=levels(meters$key), pattern="桃園市中壢區東明路", replacement="桃園市中壢區信義里")

    meters_grp = group_by(meters, date2, key) %>% summarise(meters=sum(meters))
    write.csv(meters_grp, file="./data/meters.csv", row.names=F, fileEncoding="UTF-8")

    # ==== checking missing recorders =============================================================
    #
    train  = read.csv("./data/train.csv", fileEncoding="UTF-8")
    train$key = paste0(train$CityName, train$TownName, train$VilName)
    meters$key = as.character(meters$key)

    missing_r = right_join(train, meters, by="key")
    missing_r = missing_r[is.na(missing_r$CityName),]
    levels(as.factor(missing_r$key))
}
