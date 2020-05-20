full_data = read_csv("../Data/meta_final_2020-05-18.csv")
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
              extra("comments", "extra"),
              agrc("comorbidities", "comorbidities"))
final = bind_rows(agr, extra)
final$metric = ifelse(str_detect(final$count, "0\\.|%"), "percent", "count")

