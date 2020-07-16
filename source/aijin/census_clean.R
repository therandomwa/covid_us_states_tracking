df = read.csv("../Data/census_2019.csv")

colnames(df) = colnames(df) %>% 
  toupper() %>% 
  gsub("NON.HISPANIC", "NH", .) %>% 
  gsub("AMERICAN.INDIAN.AND.ALASKAN.NATIVE", "AI.AN", .) %>% 
  gsub("NATIVE.HAWAIIAN.AND.OTHER.PACIFIC.ISLANDER", "NH.PI", .) %>% 
  gsub("TWO.OR.MORE.RACES", "MULTI", .)
write.csv(df, "../Data/census_2019_clean.csv", row.names = F)
