# currently missing total test, death age/gender, hosp race/age/gender
# additional page i can't access: https://www.health.state.mn.us/diseases/coronavirus/situation.html
# hope my code helps at least a bit
# thank you! - aijin

library(jsonlite)
meta = read.csv("../Data/meta_original.csv", skip = 1)
query = "https://services2.arcgis.com/V12PKGiMAH7dktkU/ArcGIS/rest/services/MyMapService/FeatureServer/0/query?where=ObjectID+%3C+10000&objectIds=&time=&resultType=none&outFields=*&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&sqlFormat=none&f=pjson&token="
df = fromJSON(query)$features
df = df[,1]
# df = data.frame(X = colnames(df),
# y = df[1,] %>% unlist %>% as.vector)
date = df$Date %>% strsplit(" ") %>% unlist
date = date[2] %>% as.Date("%m/%d/%Y") %>% format("%m/%d/%Y")

# summary
case = df$TotalCases
death = df$OutcmDied
hosp = df[,grep("EvrHospYes", colnames(df))]

# extra
released_from_iso = df$RlsdFrmIso
risk = df[,grep("Expsr", colnames(df))]
risk = data.frame(X = colnames(risk), 
                  comments = risk[1,] %>% unlist %>% as.vector)
residence = df[,grep("Res", colnames(df))]
residence = data.frame(X = colnames(residence), 
                       comments = residence[1,] %>% unlist %>% as.vector)
healthcare_worker = df$HlthCar
extra = rbind(risk, residence, 
              data.frame(X = c("released_from_iso", "healthcare_worker"), 
                         comments = c(released_from_iso, healthcare_worker)))
# AGR
gender = df[,c("Male", "Female", "SexMsng", "GenderOther")]
gender = data.frame(X = colnames(gender), 
                    positive_gender = gender[1,] %>% unlist %>% as.vector)
gender$X = gsub("SexMsng", "Missing", gender$X)

race = df[,grep("Race", colnames(df))] %>% select(1:2,4:9)
race = data.frame(X = colnames(race), 
                  positive_race = race[1,] %>% unlist %>% as.vector)
race$X = gsub("Race", "", race$X)
race$X = gsub("Unk", "Unknown", race$X)
drace = df[,c(57:63,108)]
drace = data.frame(X = colnames(drace), 
                   death_race = drace[1,] %>% unlist %>% as.vector)
drace$X = gsub("Death|Race", "", drace$X)
drace$X = gsub("Native", "AmerIndAlaNativ", drace$X)
drace$X = gsub("M", "Multiple", drace$X)
race = full_join(race, drace)

eth = df[,grep("Ethn", colnames(df))]
eth = data.frame(X = paste0("eth_", colnames(eth)), 
                 total.case = eth[1,] %>% unlist %>% as.vector)
eth$X = gsub("Ethn", "", eth$X)
eth$X = gsub("Unk", "Unknown", eth$X)
deth = df[,64:66]
deth = data.frame(X = paste0("eth_", colnames(deth)), 
                  total.death = deth[1,] %>% unlist %>% as.vector)
deth$X = gsub("Death", "", deth$X)
deth$X = gsub("HispU", "Unknown", deth$X)
eth = full_join(eth, deth, by = "X")

age = df[,grep("Age", colnames(df))][c("Age05","Age619","Age2029","Age3039",
                                       "Age4049","Age5059","Age6069",
                                       "Age70_79","Age80_89","Age90_99",
                                       "Age100_up")]
age = data.frame(X = colnames(age), 
                 positive_age = age[1,] %>% unlist %>% as.vector)
age$X = gsub("Age", "", age$X)


# get data
mn = meta[meta$X == "Minnesota",]
mn$last.update = date
mn$total.case = case
mn$total.death = death
mn$total_hosp = hosp
mn = rbind.fill(mn, age, gender, race, eth, extra)
mn = setDT(mn)[, lapply(.SD, function(x) toString(na.omit(x))), by = X]