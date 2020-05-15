df = meta

race_name = df %>% 
  filter(!is.na(positive_race)) %>%
  select(state_name) %>% 
  unlist %>% 
  as.vector

# get it into a dataframe
race_df = df %>%
  filter(!is.na(positive_race)) %>% 
  select(positive_race) %>% 
  unlist %>% 
  as.character %>% 
  strsplit(";|:") %>%
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
  return (x)
})

test = do.call(rbind.fill, lapply(race_df, function(x) {dat = data.frame(cat = x[,1] %>% trimws, val = 1); 
rownames(dat) = dat$cat; dat$cat = NULL; dat = dat %>% t %>% as.data.frame}))
test[is.na(test)] = ""
test = test[,order(colnames(test))]
View(test)
write.csv(test, "race_sum.csv")
race_modify = data.frame(original = colnames(test))
