# =================================================================================================

gen_data = function() {

    data = list()
    data$train  = read.csv("./data/train.csv",  fileEncoding="UTF-8")
    data$submit = read.csv("./data/submit.csv", fileEncoding="UTF-8")
    data$ref    = read.csv("./data/ref.csv",    fileEncoding="UTF-8")
    data$all    = read.csv("./data/merged.csv", fileEncoding="UTF-8")

    data$all$Towns = paste0(data$all$CityName, data$all$TownName) %>% as.factor()
    data$all$Vils  = paste0(data$all$CityName, data$all$TownName, data$all$VilName) %>% as.factor()
    
    data$all$grp_city = data$all$CityName
    
    row_sel = which(data$all$CityName == "嘉義市")
    data$all$grp_city[row_sel] = "嘉義縣"
    
    row_sel = which(data$all$CityName == "新竹市")
    data$all$grp_city[row_sel] = "新竹縣"
    
    row_sel = which(data$all$CityName == "連江縣")
    data$all$grp_city[row_sel] = "澎湖縣"
    
    row_sel = which(data$all$CityName == "金門縣")
    data$all$grp_city[row_sel] = "澎湖縣"
    
    data$all <- droplevels(data$all)
    
    return (data)
}

gen_info = function() {

    info = list()
    info$time_stamp = format(Sys.time(), "%m%d_%H%M%S")
    info$tn_tp = c("Hagibis", "Chan.hom", "Dujuan", "Soudelor", "Fung.wong", "Matmo", "Nepartak", "MerantiAndMalakas")
    info$ts_tp = c("NesatAndHaitang", "Megi")
    info$tp = c(info$tn_tp, info$ts_tp)

    features = c("pole1", "pole2", "pole3", "pole4", "pole5", "pole6", "pole7", "pole8", "pole9", "pole10")
    features = c(features, "precp_day", "precp_24h", "precp_12h", "precp_6h", "precp_3h", "precp_1h", "wind", "gust")
    # features = c(features, "precp_day", "wind", "gust")
    # features = c(features, "precp_day", "gust")
    # features = c(features, "max_hh")
    info$features = features

    info$cities   = levels(data$all$grp_city)
    info$towns    = levels(data$all$Towns)
    info$villages = levels(data$all$Vils)

    info$col_tn = c(info$features, "outage")
    info$row_tn = which(data$all$tp %in% info$tn_tp)
    info$row_ts = which(data$all$tp %in% info$ts_tp)

    info$tn_ratio = 0.8

    info$pd_path = paste0(getwd(), "/prediction/")
    if ( !dir.exists(info$pd_path) ) {
        dir.create(info$pd_path)
    }

    info$md_path = paste0(getwd(), "/model/")
    if ( !dir.exists(info$md_path) ) {
        dir.create(info$md_path)
    }

    return (info)
}

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

    train = data$train

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

randomForest_HL = function(save_model=T) {

    st = Sys.time()

    md = list()

    info$H = 500

    data$all$H = 0
    sel_row = which(data$all$outage > info$H)
    data$all$H[sel_row] = 1
    data$all$H = as.factor(data$all$H)

    md$real = data$all$H
    md$pred = rep(0, length(md$real))

    col_tn = c(info$features, "H")
    raw_tn = data$all[info$row_tn,]

    scope_H = intersect(which(data$all$outage > info$H), info$row_tn)
    scope_L = intersect(which(data$all$outage <= info$H), info$row_tn)
    len_H   = length(scope_H)
    len_L   = length(scope_L)

    ratio_tn = 0.85

    idx_H = sample(1:len_H, len_H*ratio_tn, replace=F)
    tn_H = scope_H[idx_H]

    idx_L = sample(1:len_L, len_H*ratio_tn, replace=F)
    tn_L = scope_L[idx_L]

    sel_row = cbind(tn_H, tn_L)
    row_ts  = setdiff(info$row_tn, sel_row)

    md$model = randomForest(H~., data=data$all[sel_row, col_tn], ntree=500)    
    md$pred  = predict(md$model, newdata=data$all[, info$features])

    md$score_tn = (sum(md$pred[sel_row] == md$real[sel_row]) / length(sel_row))
    md$score_ts = (sum(md$pred[row_ts] == md$real[row_ts]) / length(row_ts))

    tn_len = length(sel_row)
    ts_len = length(row_ts)
    tn_err = (1-md$score_tn) * length(sel_row)
    ts_err = (1-md$score_ts) * length(row_ts)

    tn_score = md$score_tn * 100
    ts_score = md$score_ts * 100

    message(sprintf("tn : %2.4f (%6.0f / %6.0f), ts : %2.4f (%6.0f / %6.0f)", tn_score, tn_err, tn_len, ts_score, ts_err, ts_len))

    tn_H_len = length(tn_H)
    tn_L_len = length(tn_L)

    md$score_tn_H = (sum(md$pred[tn_H] == md$real[tn_H]) / tn_H_len)
    md$score_tn_L = (sum(md$pred[tn_L] == md$real[tn_L]) / tn_L_len)

    tn_H_err = (1-md$score_tn_H) * tn_H_len
    tn_L_err = (1-md$score_tn_L) * tn_L_len

    tn_H_score = md$score_tn_H * 100
    tn_L_score = md$score_tn_L * 100

    message(sprintf("tnH: %2.4f (%6.0f / %6.0f), tnL: %2.4f (%6.0f / %6.0f)", tn_H_score, tn_H_err, tn_H_len, tn_L_score, tn_L_err, tn_L_len))

    ts_H = setdiff(scope_H, tn_H)
    ts_L = setdiff(scope_L, tn_L)

    ts_H_len = length(ts_H)
    ts_L_len = length(ts_L)

    md$score_ts_H = (sum(md$pred[ts_H] == md$real[ts_H]) / ts_H_len)
    md$score_ts_L = (sum(md$pred[ts_L] == md$real[ts_L]) / ts_L_len)

    ts_H_err = (1-md$score_ts_H) * ts_H_len
    ts_L_err = (1-md$score_ts_L) * ts_L_len

    ts_H_score = md$score_ts_H * 100
    ts_L_score = md$score_ts_L * 100

    message(sprintf("tnH: %2.4f (%6.0f / %6.0f), tnL: %2.4f (%6.0f / %6.0f)", ts_H_score, ts_H_err, ts_H_len, ts_L_score, ts_L_err, ts_L_len))    

    return (md)
}

# =================================================================================================
# Build randomforest via all data

randomForest_all = function(save_model=T) {

    st = Sys.time()

    md = list()
    md$real = data$all$outage
    md$pred = rep(0, length(md$real))

    # city = info$cities[1]

    # sel_row = which(data$all$outage < info$limit_outage)
    # sel_row = intersect(sel_row, info$row_tn)
    # md$model = randomForest(outage~., data=data$all[sel_row, info$col_tn], ntree=500)

    md$model = randomForest(outage~., data=data$all[info$row_tn, info$col_tn], ntree=500)    
    md$pred  = round(predict(md$model, newdata=data$all[, info$features]))

    cm = CM(md$real[info$row_tn], md$pred[info$row_tn])
    duration = difftime(Sys.time(), st, units="sec")
    message(sprintf(" total - rows: %5d, cm: %2.6f, duration: %6.2fs", length(info$row_tn), cm, duration))

    save_submit(md$real, md$pred)
    save_performance(model=md)
    save_model(f_prefix="rf_all_", model=md$model)
}

# =================================================================================================
# Build randomforest model per type

randomForest_type = function(
    type_name, 
    type_idx, 
    type_set, 
    outage_lv=0, 
    tn_ratio=0.8, 
    en_vd=F) {

    # type_name="city"
    # type_idx="grp_city"
    # type_set=info$cities
    # outage_lv=0
    # tn_ratio=0.8
    # en_vd=F

    # type_name="town"
    # type_idx="Towns"
    # type_set=info$towns
    # outage_lv=1
    # tn_ratio=0.8
    # en_vd=F
    
    st = Sys.time()
    
    md = list()
    md$real = data$all$outage
    md$pred = rep(0, length(md$real))
    
    for (type in type_set) {
        # print(type)
        # type = "台中市中區"
        build_md_st = Sys.time()
        
        row_outage = which(data$all$outage >= outage_lv)
        row_type   = which(data$all[, type_idx] == type)
        row_raw  = intersect(row_type, info$row_tn)

        if (length(which(data$all$outage[row_raw] >= outage_lv)) > 0) {
            row_raw  = intersect(row_raw, row_outage)
        } else {
            row_outage = which(data$all$outage >= 0)
            row_raw  = intersect(row_raw, row_outage)
        }
        
        raw      = data$all[row_raw,]
        raw_len  = nrow(raw)
        
        # =========================================================================================
        
        if (en_vd) {
            tn_len   = as.integer(tn_ratio * raw_len)
            sampling_idx = sample(1:raw_len, replace=F)
            
            row_tn = row_raw[sampling_idx[1:tn_len]]
            row_vd = row_raw[sampling_idx[(tn_len+1):raw_len]]
        } else {
            tn_len = raw_len
            row_tn = row_raw
            row_vd = NULL
        }
        
        # print(tn_len)
        
        # =========================================================================================
        
        md[[type]] = randomForest(outage~., data=data$all[row_tn, info$col_tn], ntree=500)
        md$pred[row_type] = round(predict(md[[type]], newdata=data$all[row_type, info$features]))

        # =========================================================================================

        tn_cm = CM(md$real[row_tn], md$pred[row_tn]) * 100
        vd_cm = CM(md$real[row_vd], md$pred[row_vd]) * 100
    
        duration = difftime(Sys.time(), build_md_st, units="sec")
        message(sprintf("%20s - rows: %5d / %5d, tn_cm: %2.6f, vd_cm: %2.6f, duration: %6.2fs", type, tn_len, raw_len, tn_cm, vd_cm, duration))
    }
    
    # =================================================================================================
    
    if (type_name == "tp") {
        row_tp = which(data$all$tp == "Megi")
        md$pred[row_tp] = round(predict(md[["Soudelor"]], newdata=data$all[row_tp, info$features]))
        
        row_tp = which(data$all$tp == "NesatAndHaitang")
        md$pred[row_tp] = round(predict(md[["MerantiAndMalakas"]], newdata=data$all[row_tp, info$features]))
    }
    
    # =================================================================================================
    
    cm = CM(md$real[info$row_tn], md$pred[info$row_tn])
    duration = difftime(Sys.time(), st, units="sec")
    message(sprintf(" total - rows: %5d, duration: %6.2fs", length(info$row_tn), duration))
    
    info$time_stamp = format(Sys.time(), "%m%d_%H%M%S")
    
    md$pred = apply(cbind(md$pred, data$all$max_outage), 1, FUN=min)
    
    f_prefix = paste0("rf_", type_name, "city", "_")
    
    save_submit(md$real, md$pred)
    save_performance(model=md)
    save_model(f_prefix=f_prefix, model=md)
    
    return (md)
}

# =================================================================================================

save_submit = function(real, pred) {
    
    # real     = model$real
    # pred     = model$pred
    
    submit   = data$submit
    sel_cols = colnames(submit)
    ref      = data$ref

    for (tp in info$ts_tp) {
        row_tp = which(data$all$tp == tp)
        submit[,tp] = pred[row_tp]
        submit[,paste0(tp,"_last")] = real[row_tp]

        cm = CM(real[row_tp], pred[row_tp])
        message(sprintf("%20s: %6.6f", tp, cm))
    }

    # =============================================================================================    
        
    f_submit = paste0(info$pd_path, "submit_", info$time_stamp, ".csv")
    write.csv(submit[, sel_cols], file=f_submit, row.names=FALSE, fileEncoding="UTF-8")
    message(sprintf("save submit: %s", f_submit))
    
    f_submit = paste0(info$pd_path, "submit_", info$time_stamp, "_diff.csv")
    write.csv(submit, file=f_submit, row.names=FALSE, fileEncoding="UTF-8")
    message(sprintf("save   diff: %s", f_submit))
    
    save_comparison(f_submit=f_submit)
}

# =================================================================================================

save_comparison = function(f_submit) {
    
    # f_submit = 'submit_1117_194831.csv'
    submit = read.csv(f_submit, fileEncoding="UTF-8")
    ref    = data$ref
    log    = NULL
    
    grp = group_by(submit, CityName) %>% 
        summarise(NesatAndHaitang = sum(NesatAndHaitang), Megi = sum(Megi))
    
    result = left_join(ref, grp, by=c("City" = "CityName"))
    colnames(result) = c("City", "NandH", "Megi", "NandH_pd", "Megi_pd")
    
    row_total = data_frame("Total", as.integer(672000), as.integer(4206000), sum(result[,"NandH_pd"]), sum(result[,"Megi_pd"]))
    colnames(row_total) = colnames(result)
    result = rbind(row_total, result)

    # =============================================================================================    
    
    message("Score - city outage")
    log = rbind(log, "Score - city outage")
    
    row_city = (2:nrow(result))
    
    result = transform(result, NandH_pd=as.integer(NandH_pd))
    result = transform(result, Megi_pd=as.integer(Megi_pd))
    
    for (tp in info$ts_tp) {
        if (tp == "NesatAndHaitang") {
            tpname = "NandH"
        } else {
            tpname = tp            
        }
        
        tp_pd    = paste0(tpname, "_pd")
        tp_df    = paste0(tpname, "_df")
        tp_ratio = paste0(tpname, "_ratio")
        
        result[, tp_df]    = result[, tp_pd] - result[, tpname]
        result[, tp_ratio] = (result[, tp_df] / result[, tpname]) * 100
        
        cm = CM(result[row_city, tpname], result[row_city, tp_pd])
        
        tl_real  = result[1, tpname]
        tl_pred  = result[1, tp_pd]
        tl_df    = tl_pred - tl_real
        tl_ratio = tl_df / tl_real
        
        message(sprintf("%20s - cm: %6.6f, total: %7d / %7d, diff: %d, ratio: %2.4f", tp, cm, tl_real, tl_pred, tl_df, tl_ratio))
        log = rbind(log, sprintf("%20s - cm: %6.6f, total: %7d / %7d, diff: %8d, ratio: %2.4f", tp, cm, tl_real, tl_pred, tl_df, tl_ratio))
        
        sel_col = c("City", tpname, tp_pd, tp_df, tp_ratio)
        print(result[order(-result[,tpname]), sel_col])
    }    
    
    # print(result[order(-result$Megi, -result$NandH), sel_col])

    f_submit = paste0(info$pd_path, "submit_", info$time_stamp, "_score.csv")
    write.csv(result, file=f_submit, row.names=FALSE, fileEncoding="UTF-8")
    write(log, file=f_submit, append=TRUE)
    message(sprintf("save  score: %s", f_submit))
}

# =================================================================================================

save_model = function(f_prefix, model) {

    st = Sys.time()

    f_md = paste0(info$md_path, f_prefix, info$time_stamp, ".md")
    save(model, file=f_md)

    duration = difftime(Sys.time(), st, units="sec")
    message(sprintf("save  model: %s", f_md))
    message(sprintf("duration of saving a model: %2.2fs", duration))
}

# =================================================================================================

evaluate_per_type = function(model, type_name, type_idx, type_set) {
    
    # message(sprintf("==== Evaluate performance per %s ====", type_name))
    
    result = NULL
    
    for (type in type_set) {
        
        row_type = which(data$all[, type_idx] == type)
        sel_row  = intersect(info$row_tn, row_type)
        cm = CM(model$real[sel_row], model$pred[sel_row])

        result = rbind(result, sprintf("%30s: %2.6f", type, cm))
        # message(sprintf("%8s: %2.6f", type, cm))
    }
    
    return (result)
}

# =================================================================================================

save_performance = function(model) {

    st = Sys.time()
    
    result = NULL
    result = rbind(result, "=====================================================")
    result = rbind(result, evaluate_per_type(model=model, type_name="typhoon", type_idx="tp",       type_set=info$tn_tp))
    result = rbind(result, "=====================================================")
    result = rbind(result, evaluate_per_type(model=model, type_name="city",    type_idx="CityName", type_set=info$cities))
    result = rbind(result, "=====================================================")
    result = rbind(result, evaluate_per_type(model=model, type_name="town",    type_idx="Towns",    type_set=info$towns))
    result = rbind(result, "=====================================================")
    result = rbind(result, evaluate_per_type(model=model, type_name="village", type_idx="Vils",     type_set=info$villages))
    result = rbind(result, "=====================================================")
    
    cm = CM(model$real[info$row_tn], model$pred[info$row_tn])
    result = rbind(result, sprintf("total: %2.6f", cm))
    result = rbind(result, "=====================================================")
    
    # duration = Sys.time() - st
    # message(sprintf(" total - rows: %5d, cm: %2.6f, duration: %6.2fs", length(info$row_tn), cm, duration))
    
    # =================================================================================================
    
    for (tp in info$ts_tp) {
        
        row_tp = which(data$all$tp == tp)
        cm = CM(model$real[row_tp], model$pred[row_tp])
        result = rbind(result, sprintf("%20s: %6.2f", tp, cm))
    }
    
    # =================================================================================================
    
    f_submit = paste0(info$pd_path, "submit_", info$time_stamp, "_pf.csv")
    write.csv(result, file=f_submit, row.names=FALSE, fileEncoding="UTF-8")
    message(sprintf("save submit: %s", f_submit))
    
    # =================================================================================================
    
    sel_col   = c("CityName", "TownName", "VilName", "VilCode", "key", "tp", "outage")
    pf_detail = cbind(data$all[, sel_col], model$pred)
    f_submit = paste0(info$pd_path, "submit_", info$time_stamp, "_pf_detail.csv")
    write.csv(pf_detail, file=f_submit, row.names=FALSE, fileEncoding="UTF-8")
    message(sprintf("save detail: %s", f_submit))
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
