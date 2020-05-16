df = read.csv("../Data/meta_final_2020-05-14.csv")

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
  x[, 1] = x[, 1] %>% toupper
  return (x)
})

test = do.call(rbind.fill, lapply(race_df, 
                                  function(x) {
                                    dat = data.frame(cat = x[,1] %>% trimws, val = 1) 
                                    rownames(dat) = dat$cat; dat$cat = NULL 
                                    dat = dat %>% t %>% as.data.frame}))
# test[is.na(test)] = ""
test = test[,order(colnames(test))]
View(test)
# write.csv(test, "race_sum.csv")
race = data.frame(original = colnames(test),
                  count = colSums(test, na.rm = T) %>% as.vector)
race$original = as.character(race$original)
race$new = NA

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
race[intersect(grep("MULT", race$original),
               grep("NH|HISPANIC|OTHER", race$original, invert = TRUE)), ]$new = "MULTI"

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
race[Reduce(intersect, list(grep("MISS|BLANK|UNKNOWN|AVAIL|DISCLOSE|REPORT", race$original),
                            which(is.na(race$new)),
                            grep("ETH|RACE", race$original, invert = T))),]$new = "UNKNOWN"

# PENDING
race[Reduce(intersect, list(grep("PEND|UNDER", race$original),
                            which(is.na(race$new)))),]$new = "PENDING"

# OTHER 
race[Reduce(intersect, list(which(is.na(race$new)),
                            grep("PACIFIC|PI", race$original),
                            grep("NATIVE|NH", race$original),
                            grep("INDIAN|ASIAN|HISPANIC", race$original, invert = TRUE))),]$new = "NH/PI"

