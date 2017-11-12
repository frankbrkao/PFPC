# =================================================================================================

data_pre_processing = function() {
    # 載入颱風停電戶資料

    # merge_all_info(f_last_submit="58.14500_submit_dc_1103_213554.csv")

    data = list()
    data$train  = read.csv("./data/train.csv",  fileEncoding="UTF-8")
    data$submit = read.csv("./data/submit.csv", fileEncoding="UTF-8")
    data$all    = read.csv("./data/merged.csv", fileEncoding="UTF-8")

    info = collect_info(train=data$train, lastpd=data$lastpd)

    data$tp = gen_tp_raw(train=data$train, lastpd=data$lastpd, tn_tp=info$tn_tp, ts_tp=info$ts_tp)

    md      = list()
    md$data = data
    md$info = info

    return (md)
}

# =================================================================================================
# Define column and row index

collect_info = function(train, lastpd) {

    info = list()

    info$tn_tp = c("Hagibis", "Chan.hom", "Dujuan", "Soudelor", "Fung.wong", "Matmo", "Nepartak", "MerantiAndMalakas")
    # info$tn_tp = c("Chan.hom", "Dujuan", "Soudelor", "Fung.wong", "Matmo", "Nepartak", "MerantiAndMalakas")
    # info$tn_tp = c("Dujuan", "Soudelor", "Matmo", "Nepartak", "MerantiAndMalakas")
    info$ts_tp = c("NesatAndHaitang", "Megi")
    # info$features = c("pole1", "pole2", "pole3", "pole4", "pole5", "pole6", "pole7", "pole8", "pole9", "pole10", "household", "maxWind", "gust")
    info$features = c("pole1", "pole2", "pole3", "pole4", "pole5", "pole6", "pole7", "pole8", "pole9", "pole10", "meters", "maxWind", "gust")

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
    info$row_none_zero = setdiff(row_none_zero, info$row_zero)
    info$vil_sel  = train$VilCode[row_none_zero]

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

remove_tp_prefix = function (x) {
    if (regexpr("_", x)[1] == -1)
        return (x)
    else
        return( strsplit(x, "_")[[1]][2] )
}

# =================================================================================================

build_rf_model = function(raw, targets) {

    features = info$features
    row_sel  = info$row_none_zero
    
    rf = list()
    col_real = c("n_outage")
    col_sel  = c(features, col_real)
    
    for (tp in targets) {
        tp_data = raw[[tp]]
        
        if ( is.null(tp_data) ) {
            rf[[tp]] = NULL
        } else {
            
            rf[[tp]] = randomForest(n_outage~., data=tp_data[row_sel, col_sel], ntree=500)
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
