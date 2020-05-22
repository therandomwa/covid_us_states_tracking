library(plyr)
library(dplyr)
library(data.table)
library(magrittr)
library(tidyverse)
library(stringr)

df = read.csv("meta_final_2020-05-18.csv")
df[df == ""] = NA

##################################################################
##                             Race                             ##
##################################################################

race_standard = function(race_var){
  #browser()
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
                   grep("NH|HISPANIC", race$original, invert = TRUE)), ]$new = "BLACK"
    # NH black
    race[intersect(grep("AFR|BLACK|BLK", race$original),
                   grep("NH|HISPANIC", race$original)), ]$new = "NH BLACK"
    
    # white
    race[intersect(grep("WHITE|WHT|CAUCASIAN", race$original),
                   grep("NH|HISPANIC", race$original, invert = TRUE)), ]$new = "WHITE"
    # NH white
    race[intersect(grep("WHITE|WHT|CAUCASIAN", race$original),
                   grep("NH|HISPANIC", race$original)), ]$new = "NH WHITE"
    
    # Multi
    race[intersect(grep("MULT|TWO", race$original),
                   grep("NH|HISPANIC|(OR OTHER)", race$original, invert = TRUE)), ]$new = "MULTI"
    
    # NH Multi
    race[intersect(grep("MULT", race$original),
                   grep("NH|HISPANIC", race$original)), ]$new = "NH MULTI"
    
    # ASIAN/
    race[intersect(grep("ASIAN", race$original),
                   grep("NH|HISPANIC|CAUCASIAN|PACIFIC", race$original, invert = TRUE)), ]$new = "ASIAN"
    
    # NH ASIAN/
    race[Reduce(intersect, list(grep("ASIAN", race$original),
                                grep("NH|HISPANIC", race$original),
                                grep("PACIFIC", race$original, invert = T))),]$new = "NH ASIAN"
    
    # AI/AN
    race[intersect(grep("ALASKA|AI/AN|AIAN|NATA", race$original),
                   grep("NH|HISPANIC|PACIFIC", race$original, invert = TRUE)), ]$new = "AI/AN"
    
    # NH AI/AN
    race[Reduce(intersect, list(grep("ALASKA|AI/AN|AIAN", race$original),
                                grep("NH|HISPANIC", race$original),
                                grep("PACIFIC", race$original, invert = T))),]$new = "NH AI/AN"
    
    # UNKNOWN
    race[Reduce(intersect, list(grep("MISS|BLANK|UNKNOWN|AVAIL|DISCLOSE|REPORT|UNK", race$original),
                                which(is.na(race$new)),
                                grep("OTHER", race$original, invert = T))),]$new = "UNKNOWN"
    
    # PENDING
    race[Reduce(intersect, list(grep("PEND|UNDER", race$original),
                                which(is.na(race$new)))),]$new = "PENDING"
    
    # NH/PI
    race[Reduce(intersect, list(which(is.na(race$new)),
                                grep("PACIFIC|PI", race$original),
                                grep("NATIVE|NH", race$original),
                                grep("INDIAN|ASIAN|HISPANIC", race$original, invert = TRUE))),]$new = "NH/PI"
    
    # NH NH/PI
    race[Reduce(intersect, list(which(is.na(race$new)),
                                grep("PACIFIC|PI", race$original),
                                grep("NATIVE|NH", race$original),
                                grep("INDIAN|ASIAN", race$original, invert = TRUE))),]$new = "NH NH/PI"
    
    # OTHER
    race[Reduce(intersect, list(which(is.na(race$new)),
                                grep("OTHER", race$original),
                                grep("MULT|NH|HISPANIC", race$original, invert = T))),]$new = "OTHER"
    
    # NH OTHER
    race[Reduce(intersect, list(which(is.na(race$new)),
                                grep("OTHER", race$original),
                                grep("NH|HISPANIC", race$original))),]$new = "NH OTHER"
    # HISPANIC
    race[Reduce(intersect, list(which(is.na(race$new)),
                                grep("HISPANIC|LATINO", race$original))),]$new = "HISPANIC"
    
    # random ones
    # ASIAN/PACIFIC ISLANDER
    race[Reduce(intersect, list(which(is.na(race$new)),
                                grep("ASIAN", race$original),
                                grep("NH", race$original, invert = TRUE))),]$new = "ASIAN/PI"
    
    # NH ASIAN/PACIFIC ISLANDER
    race[Reduce(intersect, list(which(is.na(race$new)),
                                grep("ASIAN", race$original),
                                grep("NH", race$original))),]$new = "NH ASIAN/PI"
    
    # AMERICAN INDIAN
    race[Reduce(intersect, list(which(is.na(race$new)),
                                grep("AMERI", race$original),
                                grep("ALASKA", race$original, invert = TRUE))),]$new = "AI"
    # PACIFIC ISLANDER
    race[Reduce(intersect, list(which(is.na(race$new)),
                                grep("PAC", race$original),
                                grep("ALASKA", race$original, invert = TRUE))),]$new = "PI"
    # HAWAIIAN
    race[Reduce(intersect, list(which(is.na(race$new)),
                                grep("HAWAIIAN", race$original),
                                grep("ALASKA", race$original, invert = TRUE))),]$new = "NH"
    # AI/AN/H/PI
    race[Reduce(intersect, list(which(is.na(race$new)),
                                grep("HAWAIIAN", race$original))),]$new = "AI/AN/NH/PI"
    # THE REST OF UNKNOWN/OTHER
    
    race[Reduce(intersect, list(which(is.na(race$new)),
                                grep("MULTIPLE", race$original))),]$new = "MULTI/OTHERS"
    race[Reduce(intersect, list(which(is.na(race$new)),
                                grep("NATIV", race$original))),]$new = "NH"
    race[Reduce(intersect, list(which(is.na(race$new)),
                                grep("PAC", race$original))),]$new = "PI"
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

###### check code correctness: race_case
# race_sum = df %>%
#   select(positive_race) %>% 
#   unlist %>% 
#   as.character %>% 
#   strsplit("; |:") %>%
#   lapply(function(x){
#     ha = matrix(x, ncol = 2, byrow = TRUE) %>% as.data.frame 
#     ha = ha$V2 %>% as.character %>% as.numeric
#     return(ha %>% sum(na.rm = TRUE))}) %>% unlist
# 
# data_sum = df %>% select(total.case) %>% unlist %>% 
#   sapply(., function(x) {x = x %>% as.character %>% strsplit("; "); return (x[[1]][1])})
# data.frame(round(race_sum,1), data_sum)


#################################################################
##                             Age                             ##
#################################################################

age_standard = function(age_var){
  
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
    lapply(function(x)
      matrix(x, ncol = 2, byrow = TRUE) %>%
        as.data.frame)
  
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
      gsub("^(.*>=)([0-9]+)$", "\\2+", ., perl=T) %>% 
      gsub("<", "0-",.)
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

##################################################################
##                            Gender                            ##
##################################################################

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
    x %>% group_by(V1) %>% summarise(V2 = sum(V2, na.rm = TRUE)) %>% as.data.frame
    # rbind(x[x$V1 != "UNKNOWN" & x$V1 != "PENDING",], 
    #       x[x$V1 == "UNKNOWN"| x$V1 == "PENDING",])
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



write.csv(df, file = paste0("meta_final_", Sys.Date(), ".csv"),
          row.names = F)
