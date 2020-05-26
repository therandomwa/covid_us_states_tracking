library(plyr)
library(dplyr)
library(data.table)
library(magrittr)
library(stringr)
library(tidyverse)
load_object <- function(file) {
  tmp <- new.env()
  load(file = file, envir = tmp)
  tmp[[ls(tmp)[1]]]
}


# command + option + o to see the pipeline structure

### 0. load files ----

file_date = Sys.Date()-1 # change accordingly if the editing date is not the scraping date
file_date_name = file_date %>% format("%Y%m%d")

# load Aijin's data
df_aw = read.csv("../Data/raw_states/meta_2020-05-25_aw.csv")

# load Chistian's data
df_cbp = load_object("../Data/raw_states/meta_2020-05-25-cbp.rda")

### 1. compile files ----
df_aw$last.update = df_aw$last.update %>% 
  as.character %>% as.Date("%m/%d/%y") %>% format("%m/%d/%y")
col_num = grep("age|gender|race|eth", colnames(df_cbp))
df2_cbp = as.data.frame(df_cbp)
df2_cbp[,col_num] = NA
df2_cbp[,col_num] = 
  sapply(df_cbp[,col_num], function(y){
    sapply(y, 
           function(x){
             x = as.data.frame(x)
             if (all(is.na(x[,2]))){
               return (NA)
             }
             if (str_detect(x[,1][1], "ethnicity")) {
               paste0(x[,1], ":", x[,2]) %>% 
                 sub("ethnicity", "eth", .) %>% 
                 paste0(collapse = "; ")
             } else{
               paste0(x[,1], ":", x[,2]) %>% 
                 sub(".*?_", "", .) %>% 
                 paste0(collapse = "; ")}
           })})
df2_cbp$last.update = df2_cbp$last.update %>% format("%m/%d/%y")
df2_cbp[!is.na(df2_cbp$positive_eth),]$total.case = 
  paste0(df2_cbp[!is.na(df2_cbp$positive_eth),]$total.case, "; ", 
         df2_cbp[!is.na(df2_cbp$positive_eth),]$positive_eth)
df2_cbp[!is.na(df2_cbp$death_eth),]$total.death = 
  paste0(df2_cbp[!is.na(df2_cbp$death_eth),]$total.death, "; ", 
         df2_cbp[!is.na(df2_cbp$death_eth),]$death_eth)
df2_cbp[!is.na(df2_cbp$hosp_eth),]$total_hosp = 
  paste0(df2_cbp[!is.na(df2_cbp$hosp_eth),]$total_hosp, "; ", 
         df2_cbp[!is.na(df2_cbp$hosp_eth),]$hosp_eth)
df2_cbp[, c("positive_eth", "death_eth", "hosp_eth")] = NULL

meta = rbind.fill(df_aw, df2_cbp)
meta$X = NULL
meta = meta[order(meta$state_name), ]
meta[meta == ""] = NA
meta$positivity.rate = NULL
meta$county.details = NULL
meta$Link = NULL






### 2. data cleaning ----

df = meta
df[df == ""] = NA

### race

race_standard = function(race_var){
  
  race_name = df %>% 
    filter(!is.na(get(race_var))) %>%
    select(state_name) %>% 
    unlist %>% 
    as.vector
  
  # get it into a dataframe
  race_df = df %>%
    filter(!is.na(get(race_var))) %>% 
    select(race_var) %>% 
    unlist %>% 
    as.character %>% 
    strsplit("; |:") %>%
    lapply(function(x)
      matrix(x, ncol = 2, byrow = TRUE) %>%
        as.data.frame)
  
  # convert % to decimal
  race_df = lapply(race_df, function(x) {
    x[, 2] = x[, 2] %>% as.character
    x[grep("%", x[, 2]), ] = x[grep("%", x[, 2]), ] %>%
      mutate(V2 = gsub("%|<", "", V2) %>% as.numeric) %>%
      mutate(V2 = V2 / 100)
    x[, 2] = x[, 2] %>% as.numeric
    x[, 1] = x[, 1] %>% toupper
    return (x)
  })
  
  test = do.call(rbind.fill, 
                 lapply(race_df, 
                        function(x) {
                          dat = data.frame(cat = x[,1] %>% trimws, val = 1) 
                          rownames(dat) = dat$cat; dat$cat = NULL 
                          dat = dat %>% t %>% as.data.frame
                          return (dat)}))
  
  test = test[,order(colnames(test))]
  race = data.frame(original = colnames(test),
                    count = colSums(test, na.rm = T) %>% as.vector)
  race$original = as.character(race$original)
  race$new = NA
  
  # dictionary 
  try({
    # black
    race[intersect(grep("AFR|BLACK|BLK", race$original),
                   grep("NH|HISPANIC", race$original, invert = TRUE)), ]$new = "BLACK"}, silent = TRUE)
  try({
    # NH black
    race[intersect(grep("AFR|BLACK|BLK", race$original),
                   grep("NH|HISPANIC", race$original)), ]$new = "NH BLACK"}, silent = TRUE)
  try({
    # white
    race[intersect(grep("WHITE|WHT|CAUCASIAN", race$original),
                   grep("NH|HISPANIC", race$original, invert = TRUE)), ]$new = "WHITE"}, silent = TRUE)
  try({
    # NH white
    race[intersect(grep("WHITE|WHT|CAUCASIAN", race$original),
                   grep("NH|HISPANIC", race$original)), ]$new = "NH WHITE"}, silent = TRUE)
  try({
    # Multi
    race[intersect(grep("MULT|TWO", race$original),
                   grep("NH|HISPANIC|(OR OTHER)", race$original, invert = TRUE)), ]$new = "MULTI"}, silent = TRUE)
  try({
    # NH Multi
    race[intersect(grep("MULT", race$original),
                   grep("NH|HISPANIC", race$original)), ]$new = "NH MULTI"}, silent = TRUE)
  try({
    # ASIAN/
    race[intersect(grep("ASIAN", race$original),
                   grep("NH|HISPANIC|CAUCASIAN|PACIFIC", race$original, invert = TRUE)), ]$new = "ASIAN"}, silent = TRUE)
  try({
    # NH ASIAN/
    race[Reduce(intersect, list(grep("ASIAN", race$original),
                                grep("NH|HISPANIC", race$original),
                                grep("PACIFIC", race$original, invert = T))),]$new = "NH ASIAN"}, silent = TRUE)
  try({
    # AI/AN
    race[intersect(grep("ALASKA|AI/AN|AIAN|NATA", race$original),
                   grep("NH|HISPANIC|PACIFIC", race$original, invert = TRUE)), ]$new = "AI/AN"}, silent = TRUE)
  try({
    # NH AI/AN
    race[Reduce(intersect, list(grep("ALASKA|AI/AN|AIAN|NATA", race$original),
                                grep("NH|HISPANIC", race$original),
                                grep("PACIFIC", race$original, invert = T))),]$new = "NH AI/AN"}, silent = TRUE)
  try({
    # UNKNOWN
    race[Reduce(intersect, list(grep("MISS|BLANK|UNKNOWN|AVAIL|DISCLOSE|REPORT|UNK|REFUSE", race$original),
                                which(is.na(race$new)),
                                grep("OTHER", race$original, invert = T))),]$new = "UNKNOWN" }, silent = TRUE)
  try({
    # PENDING
    race[Reduce(intersect, list(grep("PEND|UNDER", race$original),
                                which(is.na(race$new)))),]$new = "PENDING"}, silent = TRUE)
  
  try({
    # NH/PI
    race[Reduce(intersect, list(which(is.na(race$new)),
                                grep("PACIFIC|PI", race$original),
                                grep("NATIVE|NH", race$original),
                                grep("INDIAN|ASIAN|HISPANIC", race$original, invert = TRUE))),]$new = "NH/PI"}, silent = TRUE)
  
  try({
    # NH NH/PI
    race[Reduce(intersect, list(which(is.na(race$new)),
                                grep("PACIFIC|PI", race$original),
                                grep("NATIVE|NH", race$original),
                                grep("INDIAN|ASIAN", race$original, invert = TRUE))),]$new = "NH NH/PI"}, silent = TRUE)
  
  try({
    # OTHER
    race[Reduce(intersect, list(which(is.na(race$new)),
                                grep("OTHER", race$original),
                                grep("MULT|NH|HISPANIC", race$original, invert = T))),]$new = "OTHER"}, silent = TRUE)
  
  try({
    # NH OTHER
    race[Reduce(intersect, list(which(is.na(race$new)),
                                grep("OTHER", race$original),
                                grep("NH|HISPANIC", race$original))),]$new = "NH OTHER"}, silent = TRUE)
  try({
    # HISPANIC
    race[Reduce(intersect, list(which(is.na(race$new)),
                                grep("HISPANIC|LATINO", race$original))),]$new = "HISPANIC"}, silent = TRUE)
  
  try({
    # random ones
    # ASIAN/PACIFIC ISLANDER
    race[Reduce(intersect, list(which(is.na(race$new)),
                                grep("ASIAN", race$original),
                                grep("NH", race$original, invert = TRUE))),]$new = "ASIAN/PI"}, silent = TRUE)
  
  try({
    # NH ASIAN/PACIFIC ISLANDER
    race[Reduce(intersect, list(which(is.na(race$new)),
                                grep("ASIAN", race$original),
                                grep("NH", race$original))),]$new = "NH ASIAN/PI"}, silent = TRUE)
  
  try({
    # AMERICAN INDIAN
    race[Reduce(intersect, list(which(is.na(race$new)),
                                grep("AMERI", race$original),
                                grep("ALASKA", race$original, invert = TRUE))),]$new = "AI/AN"}, silent = TRUE)
  try({
    # PACIFIC ISLANDER
    race[Reduce(intersect, list(which(is.na(race$new)),
                                grep("PAC", race$original),
                                grep("ALASKA", race$original, invert = TRUE))),]$new = "NH/PI"}, silent = TRUE)
  try({
    # HAWAIIAN
    race[Reduce(intersect, list(which(is.na(race$new)),
                                grep("HAWAIIAN", race$original),
                                grep("ALASKA", race$original, invert = TRUE))),]$new = "NH/PI"}, silent = TRUE)
  
  try({
    # AI/AN/H/PI
    race[Reduce(intersect, list(which(is.na(race$new)),
                                grep("HAWAIIAN", race$original))),]$new = "AI/AN/NH/PI"}, silent = TRUE)
  
  # THE REST OF UNKNOWN/OTHER
  try({
    race[Reduce(intersect, list(which(is.na(race$new)),
                                grep("MULTIPLE", race$original))),]$new = "MULTI/OTHERS"}, silent = TRUE)
  try({
    race[Reduce(intersect, list(which(is.na(race$new)),
                                grep("NATIV", race$original))),]$new = "NH/PI"}, silent = TRUE)
  try({
    race[Reduce(intersect, list(which(is.na(race$new)),
                                grep("PAC", race$original))),]$new = "MH/PI"}, silent = TRUE)
  try({
    race[Reduce(intersect, list(which(is.na(race$new)),
                                grep("M", race$original))),]$new =  "UNKNOWN"}, silent = TRUE)
  
  df[,race_var] = as.character(df[,race_var])
  df[df$state_name %in% race_name, race_var] = 
    lapply(race_df, 
           function(x) {
             new_df = left_join(x, race, by = c("V1" = "original")) %>% 
               select(new, V2) %>% group_by(new) %>% 
               summarise(V2 = sum(V2, na.rm = T)) %>% 
               as.data.frame
             return (paste(paste0(new_df$new, ":" ,new_df$V2), collapse = "; "))
           }) %>% unlist
  return (df)
}
df = race_standard("positive_race")
df = race_standard("death_race")
df = race_standard("hosp_race")


### age

age_standard = function(age_var){
  # browser()
  age_name = df %>% 
    filter(!is.na(get(age_var))) %>%
    select(state_name) %>% 
    unlist %>% 
    as.vector
  
  # get it into a dataframe
  
  age_df = df %>%
    filter(!is.na(get(age_var))) %>% 
    select(age_var) %>% 
    unlist %>% 
    as.character %>% 
    strsplit("; |:") %>%
    lapply(function(x){
      dat = matrix(x, ncol = 2, byrow = TRUE) %>%
        as.data.frame
      dat$V2 = dat$V2 %>% gsub("<", "",.)
      return (dat)})
  
  # convert % to decimal and clean up labels
  age_df = lapply(age_df, function(x) {
    # percent
    x[, 2] = x[, 2] %>% as.character
    x[grep("%", x[, 2]), ] = x[grep("%", x[, 2]), ] %>%
      mutate(V2 = gsub("%|<", "", V2) %>% as.numeric) %>%
      mutate(V2 = V2 / 100)
    x[, 2] = x[, 2] %>% as.numeric
    
    # labels
    x[, 1] = x[,1] %>% as.character
    x[, 1] = x[, 1] %>% 
      toupper %>% 
      gsub("_","-", .) %>% 
      gsub("YEAR|YEARS","", .) %>% 
      gsub(" ", "", .) %>% 
      gsub("TO", "-",.) %>% 
      gsub("UNDER","<" ,.) %>% 
      gsub("ANDOVER|UP|ANDOLDER|PLUS", "+",.) %>% 
      gsub("^(.*>=)([0-9]+)$", "\\2+", ., perl=T) #%>% 
      #gsub("<", "0-",.)
    # deal with "<" in labels
    x[grep("<",x[,1]),1] = paste0("0-",as.numeric(gsub("<", "", x[grep("<",x[,1]),1]) %>% gsub("<","",.))-1)
    # 20s, 30s, etc
    new_format = gsub("S", "", x[,1][grep("^([0-9]+)(S)$",x[,1])])
    upper_bound = as.numeric(new_format) + 9
    x[,1][grep("^([0-9]+)(S)$", x[,1])] = paste0(new_format,"-",upper_bound)
    # no dash 
    test = x[,1][grep("^[0-9]+$",x[,1])]
    test = ifelse(nchar(test) == 4, paste0(substr(test,1,2),"-", substr(test,3, 4)),
                  ifelse(nchar(test) == 3 | 
                           (nchar(test) == 2 & substr(test, 1, 1) == 0), 
                         paste0(substr(test,1,1),"-", substr(test,2, nchar(test))), test))
    x[,1][grep("^[0-9]+$",x[,1])] = test
    # UNKNOWN
    x[,1][grep("MISS|AVAIL|UNK|UNCONFI", x[,1])] = "UNKNOWN"
    # PENDING
    x[,1][grep("PEND", x[,1])] = "PENDING"
    rbind(x[x$V1 != "UNKNOWN" & x$V1 != "PENDING",], 
          x[x$V1 == "UNKNOWN"| x$V1 == "PENDING",])
  })
  
  df[,age_var] = as.character(df[,age_var])
  df[df$state_name %in% age_name, age_var] = 
    lapply(age_df, 
           function(x) {
             return (paste(paste0(x$V1, ":" , x$V2), collapse = "; "))
           }) %>% unlist
  return (df)
}
df = age_standard("positive_age")
df = age_standard("death_age")
df = age_standard("hosp_age")



### gender 

gender_standard = function(gen_var){
  gender_name = df %>% 
    filter(!is.na(get(gen_var))) %>%
    select(state_name) %>% 
    unlist %>% 
    as.vector
  
  # get it into a dataframe
  gender_df = df %>%
    filter(!is.na(get(gen_var))) %>% 
    select(all_of(gen_var)) %>% 
    unlist %>% 
    as.character %>% 
    strsplit("; |:") %>%
    lapply(function(x)
      matrix(x, ncol = 2, byrow = TRUE) %>%
        as.data.frame)
  
  # convert % to decimal and clean up labels
  gender_df = lapply(gender_df, function(x) {
    # percent
    x[, 2] = x[, 2] %>% as.character
    x[grep("%", x[, 2]), ] = x[grep("%", x[, 2]), ] %>%
      mutate(V2 = gsub("%|<", "", V2) %>% as.numeric) %>%
      mutate(V2 = V2 / 100)
    x[, 2] = x[, 2] %>% as.numeric
    
    # labels
    x[, 1] = x[,1] %>% as.character
    x[, 1] = x[, 1] %>% 
      toupper
    # PENDING
    x[,1][grep("PEND", x[,1])] = "PENDING"
    # UNKNOWN
    x[,1][grep("UNK|U|NOT|NEITHER|BINARY|MISS", x[,1])] = "UNKNOWN/OTHER"
    x[,1][x[,1] == "F"] = "FEMALE"
    x[,1][x[,1] == "M"] = "MALE"
    x[,1][x[,1] == "MALE TO FEMALE"] = "FEMALE" 
    x %>% 
      group_by(V1) %>% 
      summarise(V2 = sum(V2, na.rm = TRUE)) %>% 
      as.data.frame
  })
  
  df[,gen_var] = as.character(df[,gen_var])
  df[df$state_name %in% gender_name, gen_var] = 
    lapply(gender_df, 
           function(x) {
             return (paste(paste0(x$V1, ":" , x$V2), collapse = "; "))
           }) %>% unlist
  return (df)
}
df = gender_standard("positive_gender")
df = gender_standard("death_gender")
df = gender_standard("hosp_gender")







### 3. data processing ----

full_data = df
full_data = janitor::clean_names(full_data)
final_data = full_data %>% 
  select(-state_name, -last_update,-platform,
         -total_tested_today, -total_positive_today, -total_death_today) %>% 
  mutate_all(function(x){
    map(x, function(data) {
      if (is.na(data)) {
        return(NA)
      } else if (str_detect(data, ";|:")) {
        tib = data %>%
          str_split("; ", simplify = TRUE) %>%
          str_split(":", simplify = TRUE) %>%
          as_tibble() %>%
          rename(category = V1, count = V2)
        if (tib$count[1] == ""){
          tib$count[1] = tib$category[1]
          tib$category[1] = "total"
        }
        tib %>% mutate(category = as.character(category),
                       count = as.character(count))
      } else {
        tib = data.frame(V1 = "total", V2 = data) %>% 
          as_tibble() %>% 
          mutate(V1 = as.character(V1)) %>% 
          rename(category = V1, count = V2)
        tib
      }
    })}
  )

final_data = cbind(full_data[, c("state_name", "last_update")], final_data)

extra = function(var, label){
  # browser()
  final_data %>% 
    select(state_name, var, last_update) %>% 
    unnest_wider(var) %>%
    unnest(c(category, count)) %>% 
    .[,colSums(is.na(.)) < nrow(.)] %>% 
    # select(category) %>% 
    mutate(strata_type = lapply(.$category, function(x){
      ifelse(str_detect(x, "_"), 
             x %>% 
               regmatches(regexpr("_", .), invert = TRUE)  %>% 
               lapply("[[", 1) %>% unlist, x)}) %>% unlist,
      category = lapply(.$category, function(x){
        ifelse(str_detect(x, "_"), 
               x %>% 
                 regmatches(regexpr("_", .), invert = TRUE)  %>% 
                 lapply("[[", 2) %>% unlist, NA)}) %>% unlist,
      data_type = label) %>% 
    filter(!is.na(count))
}

agrc = function(var, label){
  final_data %>% 
    select(state_name, var, last_update) %>% 
    unnest_wider(var) %>%
    unnest(c(category, count)) %>% 
    .[,colSums(is.na(.)) < nrow(.)] %>% 
    mutate(strata_type = sub(".*?_", "", var),
           data_type = label,
           count = gsub("<", "", count)) %>% 
    filter(!is.na(count))
}
agr = do.call(rbind, c(lapply(grep("positive_", names(final_data), value = TRUE),
                              function(x) agrc(x, "case")),
                       lapply(grep("death_", names(final_data), value = TRUE),
                              function(x) agrc(x, "death")),
                       lapply(grep("hosp_", names(final_data), value = TRUE),
                              function(x) agrc(x, "hosp"))))

extra = rbind(extra("total_tested", "test"),
              extra("total_case", "case"),
              extra("total_death", "death"),
              extra("total_hosp", "hosp"))#,
#extra("comments", "extra"),
#agrc("comorbidities", "comorbidities"))
final = bind_rows(agr, extra) %>% as.data.frame
final = final[final$strata_type %in% c("age", "gender", "total", "race", "eth"),]
final$metric = ifelse(str_detect(final$count, "0\\.|%"), "Percent", "Count")
final$category = toupper(final$category)
final[is.na(final)] = ""
final[grep("%", final$count), ] = final[grep("%", final$count), ] %>%
  mutate(count = gsub("%|<", "", count) %>% as.numeric) %>%
  mutate(count = count / 100)
final$count = as.numeric(final$count)
final = final[order(final$state_name),]
#final[final$category == "0-0-4",]$category = "0-4"




### 4. incorporate census data ----

census = read.csv("../Data/census.csv")

### age

final$category = final$category %>% gsub("<", "0-", .)
final$category[grep("<",final$category)] = 
  paste0("0-", as.numeric(gsub("<", "", final$category[grep("<",final$category)]) %>% gsub("<","",.))-1)
final = final %>% filter(category != ".") # georgia: age, death: cat = .
# separating age lower and upper bound
age_bound = final %>% 
  filter(str_detect(.$strata_type, "age") & 
           str_detect(.$category, "[0-9]{1,3}")) %>%  
  mutate(
    lower = str_extract(.$category, "^[0-9]{1,3}"),
    upper = sub('.*(\\-|\\+)', '', .$category)
  ) 

# calculate age population from census
for (i in 1:nrow(age_bound)){
  if (age_bound[i,]$upper != ""){
    lower_col = which(age_bound[i,]$lower==gsub("AGE_", "", names(census)))
    upper_col = which(age_bound[i,]$upper==gsub("AGE_", "", names(census)))
    if (length(lower_col) == 0 | length(upper_col) == 0){
      age_bound$pop_est[i] = NA
    } else {
      age_bound$pop_est[i] = census[census$NAME == age_bound[i,]$state_name,] %>% 
        select(lower_col:upper_col) %>% sum}
  }
  else{
    lower_col = which(age_bound[i,]$lower==gsub("AGE_", "", names(census)))
    if (length(lower_col) == 0){
      age_bound$pop_est[i] = NA
    } else{
      age_bound$pop_est[i] = census[census$NAME == age_bound[i,]$state_name,] %>% 
        select(lower_col:ncol(census)) %>% sum}
  }
}
age_bound = age_bound %>% select(-lower, -upper)
age_bound[age_bound$state_name == "Georgia" & age_bound$category == "85",]$pop_est = NA

### gender

final$category = final$category %>% gsub("UNKNOWN/LEFT BLANK", "UNKNOWN/OTHER", .)
gender_dat = final %>% 
  filter(str_detect(.$strata_type, "gender") & 
           str_detect(.$category, "FEMALE|MALE"))

for (i in 1:nrow(gender_dat)){
  if (gender_dat[i,]$category == "MALE"){
    gender_dat$pop_est[i] = census[census$NAME == gender_dat[i,]$state_name, "MALE"]
  }
  else{
    gender_dat$pop_est[i] = census[census$NAME == gender_dat[i,]$state_name, "FEMALE"]
  }
}


### total
tot_dat = final %>% 
  filter(str_detect(.$strata_type, "total"))# & 
#!str_detect(.$data_type, "extra"))

for (i in 1:nrow(tot_dat)){
  tot_dat$pop_est[i] = census[census$NAME == tot_dat[i,]$state_name, "TOTAL_POP"]
}

### ethnicity


final[final$strata_type == "eth" & str_detect(final$category, "^NO") &
        str_detect(final$category, "HI"),]$category = "NON HISPANIC"
final[final$strata_type == "eth" &
        str_detect(final$category, "^HI"),]$category = "HISPANIC"
final[final$strata_type == "eth" & final$category != "NON HISPANIC" &
        final$category != "HISPANIC",]$category = "UNKNOWN"

eth_dat = final %>% filter(strata_type == "eth" & 
                             str_detect(category, "UNKNOWN", negate = TRUE))
for (i in 1:nrow(eth_dat)){
  if (eth_dat[i,]$category == "HISPANIC"){
    eth_dat$pop_est[i] = census[census$NAME == eth_dat[i,]$state_name, "ETH_HISPANIC"]
  }
  else{
    eth_dat$pop_est[i] = census[census$NAME == eth_dat[i,]$state_name, "ETH_NH"]
  }
}

### race
# all: alabama, alaska, dc, fl, ga, hawaii, idaho, in, ia, ks, ky, me, mi, mn, 
# miss, montana, nc, ok,pa,sc,tn,vt,va,wv,wi, la
# nh:arizona, ca, co, ct, de, il, md, ma, nevada, nh, nj, nm, ri, sd,tx,wa,wy, ny
# none: nebraska, nd,oh,or
# ut ?
all = c("Alabama", "Alaska", "District of Columbia", "Florida", "Georgia",
        "Hawaii", "Idaho", "Indiana", "Iowa", "Kansas", "Kentucky", "Maine", 
        "Michigan", "Minnesota", "Mississippi", "Montana", "North Carolina", "Oklahoma", 
        "Pennsylvania", "South Carolina", "Tennessee", "Vermont", "Virginia", 
        "West Virginia", "Wyoming", "Louisiana", "Arkansas", "Missouri")
none = c("Nebraska", "North Dakota", "Ohio", "Oregon")
nh = setdiff(state.name, c(all, none))
final$category = final$category %>% 
  gsub("\\*", "", .) %>% 
  gsub("NON-HISPANIC", "NH", .) %>% 
  gsub("HISPANIC/LATINO", "HISPANIC", .) %>% 
  gsub("ANOTHER/MULTIPLE", "MULTI/OTHERS", .) %>% 
  gsub("LEFT BLANK", "UNKNOWN", .) %>% 
  gsub("ASIAN/PACIFIC ISLANDER", "ASIAN/PI", .)
for (i in 1:nrow(final)){
  if (final$state_name[i] %in% nh & final$strata_type[i] == "race"){
    final$category[i] = final$category[i] %>% 
      gsub("^ASIAN", "NH ASIAN", .) %>% 
      gsub("^BLACK", "NH BLACK", .) %>% 
      gsub("^WHITE", "NH WHITE", .) %>% 
      gsub("^AI/AN", "NH AI/AN", .) %>% 
      gsub("^MULTI", "NH MULTI", .) %>% 
      gsub("^NH/PI", "NH NH/PI", .) %>% 
      gsub("^ASIAN/PI", "NH ASIAN/PI", .) %>% 
      gsub("^AI/AN/NH/PI", "NH AI/AN/NH/PI", .)
  }
}

race_dat = final %>% filter(strata_type == "race" & 
                              str_detect(category, "UNKNOWN|OTHER|PENDING", negate = TRUE))
for (i in 1:nrow(race_dat)){
  if (race_dat[i,]$category == "HISPANIC"){
    race_dat$pop_est[i] = census[census$NAME == race_dat[i,]$state_name, "ETH_HISPANIC"]
  }
  if (race_dat[i,]$category == "ASIAN"){
    race_dat$pop_est[i] = sum(census[census$NAME == race_dat[i,]$state_name, c("NH.ASIAN","HISPANIC.ASIAN")])
  }
  if (race_dat[i,]$category == "BLACK"){
    race_dat$pop_est[i] = sum(census[census$NAME == race_dat[i,]$state_name, c("NH.BLACK","HISPANIC.BLACK")])
  }
  if (race_dat[i,]$category == "WHITE"){
    race_dat$pop_est[i] = sum(census[census$NAME == race_dat[i,]$state_name, c("NH.WHITE","HISPANIC.WHITE")])
  }
  if (race_dat[i,]$category == "AI/AN"){
    race_dat$pop_est[i] = sum(census[census$NAME == race_dat[i,]$state_name, c("NH.AI.AN","HISPANIC.AI.AN")])
  }
  if (race_dat[i,]$category == "MULTI"){
    race_dat$pop_est[i] = sum(census[census$NAME == race_dat[i,]$state_name, c("NH.MULTI","HISPANIC.MULTI")])
  }
  if (race_dat[i,]$category == "NH/PI"){
    race_dat$pop_est[i] = sum(census[census$NAME == race_dat[i,]$state_name, c("NH.NH.PI","HISPANIC.NH.PI")])
  }
  if (race_dat[i,]$category == "NH ASIAN"){
    race_dat$pop_est[i] = census[census$NAME == race_dat[i,]$state_name, "NH.ASIAN"]
  }
  if (race_dat[i,]$category == "NH BLACK"){
    race_dat$pop_est[i] = census[census$NAME == race_dat[i,]$state_name, "NH.BLACK"]
  }
  if (race_dat[i,]$category == "NH WHITE"){
    race_dat$pop_est[i] = census[census$NAME == race_dat[i,]$state_name, "NH.WHITE"]
  }
  if (race_dat[i,]$category == "NH AI/AN"){
    race_dat$pop_est[i] = census[census$NAME == race_dat[i,]$state_name, "NH.AI.AN"]
  }
  if (race_dat[i,]$category == "NH MULTI"){
    race_dat$pop_est[i] = census[census$NAME == race_dat[i,]$state_name, "NH.MULTI"]
  }
  if (race_dat[i,]$category == "NH NH/PI"){
    race_dat$pop_est[i] = census[census$NAME == race_dat[i,]$state_name, "NH.NH.PI"]
  }
  if (race_dat[i,]$category == "NH ASIAN/PI"){
    race_dat$pop_est[i] = sum(census[census$NAME == race_dat[i,]$state_name, c("NH.ASIAN","NH.NH.PI")])
  }
  if (race_dat[i,]$category == "ASIAN/PI"){
    race_dat$pop_est[i] = sum(census[census$NAME == race_dat[i,]$state_name, c("NH.ASIAN","NH.NH.PI","HISPANIC.ASIAN","HISPANIC.NH.PI")])
  }
  if (race_dat[i,]$category == "AI/AN/NH/PI"){
    race_dat$pop_est[i] = sum(census[census$NAME == race_dat[i,]$state_name, c("NH.AI.AN","NH.NH.PI","HISPANIC.AI.AN","HISPANIC.NH.PI")])
  }
}
### aggregate

new_pop = bind_rows(age_bound, gender_dat, race_dat, eth_dat, tot_dat)
final = full_join(final, new_pop)

### approx percentages and calculate normalized values
final$count2 = NA
for (i in 1:nrow(final)){
  if (final$metric[i] == "Percent"){
    final$count2[i] = final$count[i] *
      final$count[final$state_name == final$state_name[i] & 
                    final$strata_type == "total" &
                    final$data_type == final$data_type[i]]
    final$count2[i] = round(final$count2[i])
  }
  if(final$metric[i] == "Count"){
    final$count2[i] = final$count[i]
  }
}

final$normalized = final$count2 / final$pop_est * 100000
final = final[!is.na(final$count),]



### 5. save file ----
# save raw file
write.csv(df,
          file = paste0("../Data/raw_states/meta_final_", file_date_name, ".csv"),
          row.names = F)
# save processed file
write.csv(final, 
          file = paste0("../data/processed_states/processed_state_data_", 
                        file_date_name, 
                        ".csv"), row.names = F)



