full_data = read_csv("../Data/meta_final_2020-05-19.csv")
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
           data_type = label) %>% 
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
              extra("total_hosp", "hosp"),
              extra("comments", "extra"),
              agrc("comorbidities", "comorbidities"))
final = bind_rows(agr, extra)
final$metric = ifelse(str_detect(final$count, "0\\.|%"), "percent", "count")
final$category = toupper(final$category)
final = final[order(final$state_name),]

df = read.csv("../Data/census.csv")
##################################################################
##                  Population level data: Age                  ##
##################################################################

final$category = final$category %>% gsub("<", "0-", .)

# separating age lower and upper bound
age_bound = final %>% 
  filter(str_detect(.$strata_type, "age") & 
           str_detect(.$category, "[0-9]{1,2}")) %>%  
  mutate(
    lower = str_extract(.$category, "^[0-9]{1,2}"),
    upper = sub('.*(\\-|\\+)', '', .$category)
  ) 

# calculate age population from census
for (i in 1:nrow(age_bound)){
  if (age_bound[i,]$upper != ""){
    lower_col = which(age_bound[i,]$lower==gsub("AGE_", "", names(df)))
    upper_col = which(age_bound[i,]$upper==gsub("AGE_", "", names(df)))
    if (length(lower_col) == 0){
      age_bound$pop[i] = NA
    } else if (length(upper_col) == 0){
      age_bound$pop[i] = df[df$NAME == age_bound[i,]$state_name,] %>% 
        select(lower_col:ncol(df)) %>% sum
    }
    else{
      age_bound$pop[i] = df[df$NAME == age_bound[i,]$state_name,] %>% 
        select(lower_col:upper_col) %>% sum}
  }
  else{
    lower_col = which(age_bound[i,]$lower==gsub("AGE_", "", names(df)))
    if (length(lower_col) == 0){
      age_bound$pop[i] = NA
    } else{
      age_bound$pop[i] = df[df$NAME == age_bound[i,]$state_name,] %>% 
        select(lower_col:ncol(df)) %>% sum}
  }
}
age_bound = age_bound %>% select(-lower, -upper)

#################################################################
##                Population level data: Gender                ##
#################################################################

final$category = final$category %>% gsub("UNKNOWN/LEFT BLANK", "UNKNOWN/OTHER", .)
gender_dat = final %>% 
  filter(str_detect(.$strata_type, "gender") & 
           str_detect(.$category, "FEMALE|MALE"))

for (i in 1:nrow(gender_dat)){
  if (gender_dat[i,]$category == "MALE"){
    gender_dat$pop[i] = df[df$NAME == gender_dat[i,]$state_name, "MALE"]
  }
  else{
    gender_dat$pop[i] = df[df$NAME == gender_dat[i,]$state_name, "FEMALE"]
  }
}

##################################################################
##                 Population level data: Total                 ##
##################################################################
tot_dat = final %>% 
  filter(str_detect(.$strata_type, "total") & 
           !str_detect(.$data_type, "extra"))

for (i in 1:nrow(tot_dat)){
  tot_dat$pop[i] = df[df$NAME == tot_dat[i,]$state_name, "TOTAL_POP"]
}

## agregate
new_pop = bind_rows(age_bound, gender_dat, tot_dat)
final = full_join(final, new_pop)

##################################################################
##               Population level data: Ethnicity               ##
##################################################################
# final %>% filter(strata_type == "eth") %>% 
#   gsub("^NO+", )

write.csv(final, "../Data/processed_state_data_20200519.csv", row.names = F)

