# currently missing total test, death age/gender, hosp race/age/gender
# additional page i can't access: https://www.health.state.mn.us/diseases/coronavirus/situation.html
# hope my code helps at least a bit
# thank you! - aijin

library(jsonlite)
library(magrittr)
library(rvest) # Added by Christian
library(stringr) # Added by Christian
library(pdftools) # Added by Christian

meta = read.csv("./data/meta_original.csv", skip = 1)
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

### Grabbing total test and death by age from the page

# Get the total test counts
url = "https://www.health.state.mn.us/diseases/coronavirus/situation.html"

total_tests = read_html(url) %>% 
  html_nodes("body #container #content .site-wrap #body #accordion p") %>% 
  html_text() %>% .[4] %>% 
  str_split(" ", simplify = TRUE) %>% .[1, 6] %>% 
  str_split(":", simplify = TRUE) %>% .[1, 2] %>% 
  str_trim %>% str_replace(",", "") %>% as.numeric

# Get the death counts by age category
death_age_table = read_html(url) %>% 
  html_nodes("body #container #content .site-wrap #body #accordion .panel") %>% 
  html_nodes("#ageg .panel-body #agetable") %>% 
  html_table() %>% .[[1]] %>% 
  dplyr::select(`Age Group`, `Number of Deaths`)

colnames(death_age_table) = c("X", "death_age")

# Make the row names match what Aijin has
death_age_table$X = c(age$X, "unknown") # Adding unknown since it's here

### Grabbing tdeath by gender, and hosp information from the weekly report

# Get the link to the pdf itself on the page
pdf_url = read_html(url) %>% 
  html_nodes("body #container #content .site-wrap #body #accordion .well ul") %>% 
  html_nodes("li a") %>% .[[1]] %>% 
  html_attr("href")

pdf_data = pdf_text(paste0("https://www.health.state.mn.us/", pdf_url))

death_by_gender = pdf_data %>% .[9] %>% 
  str_split("\n") %>% .[[1]] %>% 
  str_squish %>% .[31:34] %>% 
  str_split(" ", simplify = TRUE) %>% .[,1] %>% 
  str_replace(",", "") %>% as.numeric

death_gender_table = data.frame(
  X = c("Male", "Female", "GenderOther", "Missing"),
  death_gender = death_by_gender
)

hosp_by_age = pdf_data %>% .[8] %>% 
  str_split("\n") %>% .[[1]] %>% 
  str_squish %>% .[75:86] %>% 
  str_split(" ", simplify = TRUE) %>% .[,1] %>% as.numeric

# Convert it into table like Aijin's
hosp_age_table = data.frame(
  X = death_age_table$X,
  hosp_age = hosp_by_age
)

hosp_by_gender = pdf_data %>% .[9] %>% 
  str_split("\n") %>% .[[1]] %>% 
  str_squish %>% .[26:29] %>% 
  str_split(" ", simplify = TRUE) %>% .[,1] %>% 
  str_replace(",", "") %>% as.numeric

hosp_gender_table = data.frame(
  X = c("Male", "Female", "GenderOther", "Missing"),
  hosp_gender = hosp_by_gender
)

hosp_by_race = pdf_data %>% .[10] %>% 
  str_split("\n") %>% .[[1]] %>% 
  str_squish %>% .[69:79] %>% 
  str_split(" ", simplify = TRUE) %>% .[,1] %>% 
  str_replace(",", "") %>% as.numeric

hosp_race_table = data.frame(
  X = c("Wht", "Blk", "Asian", "Nativ", "Pacif", "Multiple", "Other", "Unkno"),
  hosp_race = hosp_by_race[1:8]
)

hosp_eth_table = data.frame(
  X = eth$X,
  total.case = hosp_by_race[9:11]
)

# Variables that store what you need
# total_tests
# death_age_table
# death_gender_table
# hosp_age_table
# hosp_gender_table
# hosp_race_table
# hosp_eth_table


# get data
mn = meta[meta$X == "Minnesota",]
mn$last.update = date
mn$total.case = case
mn$total.death = death
mn$total_hosp = hosp
mn = rbind.fill(mn, age, gender, race, eth, extra)
mn = setDT(mn)[, lapply(.SD, function(x) toString(na.omit(x))), by = X]