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

    family = read.csv("./data/opendata10603M030.csv", fileEncoding="UTF-8")
    # family[263,3]

    family = family[-1, c("site_id", "village", "household_no")]
    levels(family$site_id) = gsub(x=levels(family$site_id), pattern="　",           replacement="")
    levels(family$site_id) = gsub(x=levels(family$site_id), pattern=" ",            replacement="")
    levels(family$site_id) = gsub(x=levels(family$site_id), pattern="臺北",         replacement="台北")
    levels(family$site_id) = gsub(x=levels(family$site_id), pattern="臺中",         replacement="台中")
    levels(family$site_id) = gsub(x=levels(family$site_id), pattern="臺南",         replacement="台南")
    levels(family$site_id) = gsub(x=levels(family$site_id), pattern="高雄市三民一", replacement="高雄市三民區")
    levels(family$site_id) = gsub(x=levels(family$site_id), pattern="高雄市三民二", replacement="高雄市三民區")
    levels(family$site_id) = gsub(x=levels(family$site_id), pattern="高雄市鳳山一", replacement="高雄市鳳山區")
    levels(family$site_id) = gsub(x=levels(family$site_id), pattern="高雄市鳳山二", replacement="高雄市鳳山區")

    levels(family$village) = gsub(x=levels(family$village), pattern="高雄市鳳山二", replacement="高雄市鳳山區")

    family$key = as.factor(paste0(family$site_id, family$village))

    levels(family$key) = gsub(x=levels(family$key), pattern="台北市信義區富台里",   replacement="台北市信義區富臺里")
    levels(family$key) = gsub(x=levels(family$key), pattern="台南市麻豆區晋江里",   replacement="台南市麻豆區晉江里")
    levels(family$key) = gsub(x=levels(family$key), pattern="新竹縣竹北市中崙里",   replacement="新竹縣竹北市斗崙里")
    levels(family$key) = gsub(x=levels(family$key), pattern="彰化縣彰化市南瑶里",   replacement="彰化縣彰化市南瑤里")
    levels(family$key) = gsub(x=levels(family$key), pattern="雲林縣臺西鄉台西村",   replacement="雲林縣臺西鄉臺西村")
    levels(family$key) = gsub(x=levels(family$key), pattern="雲林縣口湖鄉台子村",   replacement="雲林縣口湖鄉臺子村")
    levels(family$key) = gsub(x=levels(family$key), pattern="嘉義縣中埔鄉塩舘村",   replacement="嘉義縣中埔鄉塩館村")
    levels(family$key) = gsub(x=levels(family$key), pattern="屏東縣霧臺鄉霧台村",   replacement="屏東縣霧臺鄉霧臺村")
    levels(family$key) = gsub(x=levels(family$key), pattern="屏東縣霧臺鄉霧台村",   replacement="屏東縣霧臺鄉霧臺村")

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
    # levels(as.factor(missing_r$key))
}

gen_meters_info = function() {

    # 用電戶數 台電 - 縣市住商用電資訊 - 各縣市村里售電資訊
    # http://www.taipower.com.tw/content/announcement/ann01.aspx?BType=37
    # ./data/open_sell_amt_vil.csv

    meters = read.csv("./data/open_sell_amt_vil.csv", fileEncoding="UTF-8")

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
    levels(meters$key) = gsub(x=levels(meters$key), pattern="桃園市中壢區興合里", replacement="桃園市中壢區興和里")
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
    # train  = read.csv("./data/train.csv", fileEncoding="UTF-8")
    # train$key = paste0(train$CityName, train$TownName, train$VilName)
    # meters$key = as.character(meters$key)
    # 
    # missing_r = right_join(train, meters, by="key")
    # missing_r = missing_r[is.na(missing_r$CityName),]
    # levels(as.factor(missing_r$key))
}

# =================================================================================================

gen_village_info = function() {
    
    raw    = read.csv("./data/train.csv",  fileEncoding="UTF-8", stringsAsFactors=F)
    pole   = read.csv("./data/pole.csv",   fileEncoding="UTF-8", stringsAsFactors=F)
    family = read.csv("./data/family.csv", fileEncoding="UTF-8", stringsAsFactors=F)
    meters = read.csv("./data/meters.csv", fileEncoding="UTF-8", stringsAsFactors=F)
    meters = meters[which(meters$date2 == "201703"), c("key", "meters")]
    
    col_sel = c("CityName", "TownName", "VilName", "VilCode", "key")
    
    raw$key = paste0(raw$CityName, raw$TownName, raw$VilName)
    raw = left_join(raw[col_sel], pole, by="key")
    raw = left_join(raw, family,  by="key")
    raw = left_join(raw, meters,  by="key")
    
    # TODO: Correct family info
    # Set the missing value to 0
    raw[is.na(raw)] = 0
    
    write.csv(raw, file="./data/village.csv", row.names=F, fileEncoding="UTF-8")
}

# =================================================================================================

gen_station_observation = function() {

    st_locations = read.csv("./data/station_locaion.csv", stringsAsFactors=T, fileEncoding="UTF-8")
    station_observation = read.csv("./data/station_observation.csv", fileEncoding="UTF-8")
    village = read.csv("./data/village.csv", fileEncoding="UTF-8")

    levels(st_locations$city) = gsub(x=levels(st_locations$city), pattern="臺",   replacement="台")
    levels(st_locations$city) = gsub(x=levels(st_locations$city), pattern="台東", replacement="臺東")

    levels(st_locations$town) = gsub(x=levels(st_locations$town), pattern="台西鄉", replacement="臺西鄉")
    levels(st_locations$town) = gsub(x=levels(st_locations$town), pattern="溪洲鄉", replacement="溪州鄉")
    levels(st_locations$town) = gsub(x=levels(st_locations$town), pattern="霧台鄉", replacement="霧臺鄉")

    # ==== checking missing recorders =============================================================
    #
    # missing_r = right_join(village, st_locations, by=c("CityName" = "city"))
    # missing_r = missing_r[is.na(missing_r$TownName),]
    # levels(as.factor(missing_r$key))
    # nrow(missing_r)
    # head(missing_r)

    # missing_r = right_join(village, st_locations, by=c("TownName" = "town"))
    # missing_r = missing_r[is.na(missing_r$VilName),]
    # levels(as.factor(missing_r$TownName))

    # missing_r = right_join(st_locations, station_observation, by=c("st_no" = "st_code"))
    # missing_r = missing_r[is.na(missing_r$city),]
    # levels(as.factor(missing_r$st_no))

    # =============================================================================================

    levels(station_observation$tp_eng) = gsub(x=levels(station_observation$tp_eng), pattern="meranti", replacement="merantiandmalakas")
    levels(station_observation$tp_eng) = gsub(x=levels(station_observation$tp_eng), pattern="nesat",   replacement="nesatandhaitang")

    st_obs = right_join(st_locations, station_observation, by=c("st_no" = "st_code"))

    st_obs$town_key = paste0(st_obs$city, st_obs$town) %>% as.factor
        
    st_obs$city_grp = st_obs$city
    sel_rows = which(st_obs$city_grp == "新竹市")
    st_obs$city_grp[sel_rows] = "新竹縣"
    sel_rows = which(st_obs$city_grp == "嘉義市")
    st_obs$city_grp[sel_rows] = "嘉義縣"

    # sel_rows = which(st_obs$city %in% c("新竹縣", "台中市", "南投縣", "嘉義縣"))
    # st_obs_gust = st_obs[sel_rows,]

    # =============================================================================================

    village$town_key = paste0(village$CityName, village$TownName) %>% as.factor
    
    village$city_grp = village$CityName
    sel_rows = which(village$city_grp == "新竹市")
    village$city_grp[sel_rows] = "新竹縣"
    sel_rows = which(village$city_grp == "嘉義市")
    village$city_grp[sel_rows] = "嘉義縣"

    # =============================================================================================

    grp_obs_city = group_by(st_obs, city_grp, tp_code, tp_cht, tp_eng) %>% 
        summarise(
            c_precp_total   = mean(precp_total), 
            c_precp_day     = max(precp_day),
            c_precp_24h     = max(precp_24h),
            c_precp_12h     = max(precp_12h),
            c_precp_6h      = max(precp_6h),
            c_precp_3h      = max(precp_3h),
            c_precp_1h      = max(precp_1h),
            c_wind          = max(wind),
            c_gust          = max(gust),
            c_avg_precp_day = mean(precp_day),
            c_avg_precp_24h = mean(precp_24h),
            c_avg_precp_12h = mean(precp_12h),
            c_avg_precp_6h  = mean(precp_6h),
            c_avg_precp_3h  = mean(precp_3h),
            c_avg_precp_1h  = mean(precp_1h),
            c_avg_wind      = mean(wind)
            )

    # =============================================================================================

    obs_gust = filter(obs_gust, (city %in% c("新竹縣", "台中市")) & (gust > 0))

    grp_obs_gust = group_by(obs_gust, tp_code, tp_cht, tp_eng) %>% 
        summarise(c_gust = mean(gust))

    sel_rows = which(grp_obs_city$city_grp == "苗栗縣")
    grp_obs_city$c_gust[sel_rows] = grp_obs_gust$c_gust

    # =============================================================================================

    obs_gust = filter(obs_gust, (city %in% c("台中市", "南投縣", "嘉義縣")) & (gust > 0))

    grp_obs_gust = group_by(obs_gust, tp_code, tp_cht, tp_eng) %>% 
        summarise(c_gust = mean(gust))

    sel_rows = which(grp_obs_city$city_grp == "雲林縣")
    grp_obs_city$c_gust[sel_rows] = grp_obs_gust$c_gust

    sel_rows = which(grp_obs_city$city_grp == "彰化縣")
    grp_obs_city$c_gust[sel_rows] = grp_obs_gust$c_gust
    
    # =============================================================================================    
    
    grp_obs_town = group_by(st_obs, town_key, tp_code, tp_cht, tp_eng) %>%
        summarise(
            t_precp_total = mean(precp_total), 
            t_precp_day   = max(precp_day),
            t_precp_24h   = max(precp_24h),
            t_precp_12h   = max(precp_12h),
            t_precp_6h    = max(precp_6h),
            t_precp_3h    = max(precp_3h),
            t_precp_1h    = max(precp_1h),
            t_wind        = max(wind),
            t_gust        = max(gust))

    # =============================================================================================
        
    tp_list = data.frame(tp = levels(st_obs$tp_eng))
    vil_tp  = merge(village, tp_list, all=TRUE)
    
    vil_st  = left_join(vil_tp, grp_obs_city, by=c("city_grp" = "city_grp", "tp" = "tp_eng"))
    vil_st  = left_join(vil_st, grp_obs_town, by=c("town_key" = "town_key", "tp" = "tp_eng"))

    vil_st$precp_total = vil_st$t_precp_total
    vil_st$precp_day   = vil_st$t_precp_day
    vil_st$precp_24h   = vil_st$t_precp_24h
    vil_st$precp_12h   = vil_st$t_precp_12h
    vil_st$precp_6h    = vil_st$t_precp_6h
    vil_st$precp_3h    = vil_st$t_precp_3h
    vil_st$precp_1h    = vil_st$t_precp_1h 
    vil_st$wind        = vil_st$t_wind
    vil_st$gust        = vil_st$t_gust

    null_rows = is.na(vil_st$precp_total)
    vil_st$precp_total[null_rows] = vil_st$c_precp_total[null_rows]

    null_rows = is.na(vil_st$precp_day)
    vil_st$precp_day[null_rows] = vil_st$c_precp_day[null_rows]

    null_rows = is.na(vil_st$precp_24h)
    vil_st$precp_24h[null_rows] = vil_st$c_precp_24h[null_rows]

    null_rows = is.na(vil_st$precp_12h)
    vil_st$precp_12h[null_rows] = vil_st$c_precp_12h[null_rows]

    null_rows = is.na(vil_st$precp_6h)
    vil_st$precp_6h[null_rows] = vil_st$c_precp_6h[null_rows]

    null_rows = is.na(vil_st$precp_3h)
    vil_st$precp_3h[null_rows] = vil_st$c_precp_3h[null_rows]

    null_rows = is.na(vil_st$precp_1h)
    vil_st$precp_1h[null_rows] = vil_st$c_precp_1h[null_rows]

    null_rows = is.na(vil_st$wind)
    vil_st$wind[null_rows] = vil_st$c_wind[null_rows]

    null_rows = is.na(vil_st$gust)
    vil_st$gust[null_rows] = vil_st$c_gust[null_rows]

    zero_rows = which(vil_st$wind == 0)
    vil_st$wind[zero_rows] = vil_st$c_avg_wind[zero_rows]

    zero_rows = which(vil_st$gust == 0)
    vil_st$gust[zero_rows] = vil_st$c_gust[zero_rows]

    zero_rows = which(vil_st$precp_24h == 0)
    vil_st$precp_total[zero_rows] = vil_st$c_precp_total[zero_rows]
    vil_st$precp_day[zero_rows]   = vil_st$c_avg_precp_day[zero_rows]
    vil_st$precp_24h[zero_rows]   = vil_st$c_avg_precp_24h[zero_rows]
    vil_st$precp_12h[zero_rows]   = vil_st$c_avg_precp_12h[zero_rows]
    vil_st$precp_6h[zero_rows]    = vil_st$c_avg_precp_6h[zero_rows]
    vil_st$precp_3h[zero_rows]    = vil_st$c_avg_precp_3h[zero_rows]
    vil_st$precp_1h[zero_rows]    = vil_st$c_avg_precp_1h[zero_rows]

    # =============================================================================================
        
    write.csv(vil_st, file="./data/station_obs.csv", row.names=F, fileEncoding="UTF-8")
}    

# =================================================================================================

merge_all_info = function(f_last_submit) {

    lastpd      = read.csv(paste0("./submit/", f_last_submit), fileEncoding="UTF-8")
    train       = read.csv("./data/train.csv",  fileEncoding="UTF-8")
    station_obs = read.csv("./data/station_obs.csv", fileEncoding="UTF-8")

    tn_tp = c("Hagibis", "Chan.hom", "Dujuan", "Soudelor", "Fung.wong", "Matmo", "Nepartak", "MerantiAndMalakas")
    ts_tp = c("NesatAndHaitang", "Megi")

    col_selL = c("VilCode", tn_tp)
    col_selR = c("VilCode", ts_tp)

    tp_outage = left_join(train[col_selL], lastpd[col_selR], by="VilCode")
    tp_outage = melt(tp_outage, id=c("VilCode"))
    colnames(tp_outage) = c("VilCode", "tp", "outage")
    levels(tp_outage$tp) = levels(tp_outage$tp)

    # =============================================================================================

    vil_max_outage = filter(tp_outage, tp %in% tn_tp) %>%
        group_by(VilCode) %>% 
        summarise(outage=max(outage))
    
    colnames(vil_max_outage)[2] = c("max_outage")

    # =============================================================================================
    merged = left_join(station_obs, tp_outage, by=c("VilCode", "tp"))
    merged = left_join(merged, vil_max_outage, by=c("VilCode"))
    
    merged$max_hh = apply(merged[,c("household", "meters", "max_outage")], 1, max)
    merged$outage_pct = (merged$outage / merged$max_hh) * 100
    
    # =============================================================================================

    merged$Towns = paste0(merged$CityName, merged$TownName)
    merged$Vils  = paste0(merged$CityName, merged$TownName, merged$VilName)

    merged$grp_city = merged$CityName
    
    row_sel = which(merged$CityName == "嘉義市")
    merged$grp_city[row_sel] = "嘉義縣"
    
    row_sel = which(merged$CityName == "新竹市")
    merged$grp_city[row_sel] = "新竹縣"
    
    row_sel = which(merged$CityName == "連江縣")
    merged$grp_city[row_sel] = "澎湖縣"
    
    row_sel = which(merged$CityName == "金門縣")
    merged$grp_city[row_sel] = "澎湖縣"

    # =============================================================================================
    
    sel_cols = c("CityName", "TownName", "VilName", "VilCode", "key", "grp_city", "Towns", "Vils", "tp", "tp_code.x")
    sel_cols = c(sel_cols, "pole1", "pole2", "pole3", "pole4", "pole5", "pole6", "pole7", "pole8", "pole9", "pole10")
    sel_cols = c(sel_cols, "precp_total", "precp_day", "precp_24h", "precp_12h", "precp_6h", "precp_3h", "precp_1h", "wind", "gust")
    sel_cols = c(sel_cols, "household", "meters", "max_outage", "max_hh", "outage", "outage_pct")
    
    write.csv(merged[,sel_cols], file="./data/merged.csv", row.names=F, fileEncoding="UTF-8")
}

