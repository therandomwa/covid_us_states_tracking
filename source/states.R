##########################
# DATA STRUCTURE FUNCTIONS
##########################

skeleton_col = function(cnames) {
  # Need to make a named vector to make it easier to insert data
  # in a cel-wise fashion
  skeleton = rep(NA, length = length(cnames))
  names(skeleton) = cnames
  return(skeleton)
}

skeleton_table = function(cnames) {
  # Create a table of blanks using named vectors
  # Easy insert of data
  skeleton = list(
    "data" = names(skeleton_col(cnames)),
    "tested" = skeleton_col(cnames),
    "cases" = skeleton_col(cnames),
    "negatives" = skeleton_col(cnames),
    "recovered" = skeleton_col(cnames),
    "deaths" = skeleton_col(cnames),
    "hospitalized" = skeleton_col(cnames))
  return(skeleton)
}

standardize = function(skeleton) {
  tibble(
    total.tested = skeleton[["tested"]][skeleton[["data"]] == "total"],
    total.case = skeleton[["cases"]][skeleton[["data"]] == "total"],
    total.death = skeleton[["deaths"]][skeleton[["data"]] == "total"],
    total_hosp = skeleton[["hospitalized"]][skeleton[["data"]] == "total"],
    total.tested.today = NA,
    total.positive.today = NA,
    total.death.today = NA,
    positivity.rate = NA,
    county.details = NA,
    comorbidities = NA
  ) %>% 
    mutate(
      positive_race = map(1, function(s) {
        skeleton %>% filter(str_detect(data, 'race')) %>% select(data, cases)
      }),
      positive_eth = map(1, function(s) {
        skeleton %>% filter(str_detect(data, 'ethnicity')) %>% select(data, cases)
      }),
      positive_age = map(1, function(s) {
        skeleton %>% filter(str_detect(data, 'age')) %>% select(data, cases)
      }),
      positive_gender = map(1, function(s) {
        skeleton %>% filter(str_detect(data, 'sex')) %>% select(data, cases) 
      }),
      death_race = map(1, function(s) {
        skeleton %>% filter(str_detect(data, 'race')) %>% select(data, deaths)
      }),
      death_eth = map(1, function(s) {
        skeleton %>% filter(str_detect(data, 'ethnicity')) %>% select(data, deaths)
      }),
      death_age = map(1, function(s) {
        skeleton %>% filter(str_detect(data, 'age')) %>% select(data, deaths)
      }),
      death_gender = map(1, function(s) {
        skeleton %>% filter(str_detect(data, 'sex')) %>% select(data, deaths)
      }),
      hosp_race = map(1, function(s) {
        skeleton %>% filter(str_detect(data, 'race')) %>% select(data, hospitalized)
      }),
      hosp_eth = map(1, function(s) {
        skeleton %>% filter(str_detect(data, 'ethnicity')) %>% select(data, hospitalized)
      }),
      hosp_age = map(1, function(s) {
        skeleton %>% filter(str_detect(data, 'age')) %>% select(data, hospitalized)
      }),
      hosp_gender = map(1, function(s) {
        skeleton %>% filter(str_detect(data, 'sex')) %>% select(data, hospitalized)
      })
    )
}

###########################
# STATE SCRAPING FUNCTIONS
###########################

### FUNCTIONS FOR AUTOMATED STATES

get_oklahoma = function() {
  case_url = "https://storage.googleapis.com/ok-covid-gcs-public-download/oklahoma_cases_county.csv"
  
  case_by_county = read_csv(case_url)
  oklahoma = skeleton_table(ok_cols)
  
  # Push items into the skeleton  
  oklahoma[["cases"]][["total"]] = case_by_county %>% 
    pull(Cases) %>% sum
  oklahoma[["deaths"]][["total"]] = case_by_county %>% 
    pull(Deaths) %>% sum
  
  browseURL("https://coronavirus.health.ok.gov/")
  
  oklahoma[["hospitalized"]][["total"]] = get_information("OK, hosp total: ") 
  
  oklahoma[["cases"]][["age_0_4"]] = get_information("OK, cases age_0_4: ") 
  oklahoma[["cases"]][["age_5_17"]] = get_information("OK, cases age_5_17: ") 
  oklahoma[["cases"]][["age_18_35"]] = get_information("OK, cases age_18_35: ") 
  oklahoma[["cases"]][["age_36_49"]] = get_information("OK, cases age_36_49: ")
  oklahoma[["cases"]][["age_50_64"]] = get_information("OK, cases age_50_64: ") 
  oklahoma[["cases"]][["age_65+"]] = get_information("OK, cases age_65+: ")
  
  # Race and ethnicity are mixed together
  oklahoma[["cases"]][["race_white"]] = get_information("OK, cases white: ") 
  oklahoma[["cases"]][["race_unk"]] = get_information("OK, cases race_unk: ")
  oklahoma[["cases"]][["race_AfrA"]] = get_information("OK, cases AfrA: ") 
  oklahoma[["cases"]][["race_NatA"]] = get_information("OK, cases NatA: ") 
  oklahoma[["cases"]][["race_multi_other"]] = get_information("OK, cases multi/other: ")
  oklahoma[["cases"]][["race_asian_pac"]] = get_information("OK, cases asian: ")
   
  oklahoma[["cases"]][["sex_male"]] = get_information("OK, cases sex_male: ") 
  oklahoma[["cases"]][["sex_female"]] = get_information("OK, cases sex_female: ")
  oklahoma[["cases"]][["sex_unk"]] = get_information("OK, cases sex_unk: ")
  
  oklahoma[["deaths"]][["age_0_4"]] = get_information("OK, death age_0_4: ") 
  oklahoma[["deaths"]][["age_5_17"]] = get_information("OK, death age_5_17: ") 
  oklahoma[["deaths"]][["age_18_35"]] = get_information("OK, death age_18_35: ") 
  oklahoma[["deaths"]][["age_36_49"]] = get_information("OK, death age_36_49: ")
  oklahoma[["deaths"]][["age_50_64"]] = get_information("OK, death age_50_64: ") 
  oklahoma[["deaths"]][["age_65+"]] = get_information("OK, deathage_65+: ")
  
  oklahoma[["deaths"]][["race_white"]] = get_information("OK, death white: ") 
  oklahoma[["deaths"]][["race_AfrA"]] = get_information("OK, death AfrA: ") 
  oklahoma[["deaths"]][["race_NatA"]] = get_information("OK, death NatA: ") 
  oklahoma[["deaths"]][["race_unk"]] = get_information("OK, death race_unk: ")
  oklahoma[["deaths"]][["race_multi_other"]] = get_information("OK, death multi/other: ") 
  oklahoma[["deaths"]][["race_asian_pac"]] = get_information("OK, death asian: ")
  
  oklahoma[["deaths"]][["sex_male"]] = get_information("OK, death sex_male: ") 
  oklahoma[["deaths"]][["sex_female"]] = get_information("OK, death sex_female: ")
  
  final_oklahoma = as_tibble(oklahoma) %>% 
    standardize %>% 
    mutate(
      state_name = "Oklahoma",
      Link = "https://coronavirus.health.ok.gov",
      platform = "pdf",
      comments = "Demographic and age data available, but age and demo data manually entered.",
      last.update = Sys.time() %>% as_date) %>% 
    select(
      state_name, Link,
      total.tested:hosp_gender,
      platform, comments, last.update)
  
  return(final_oklahoma)
}

get_mississippi = function() {
  
  mississippi = skeleton_table(ms_cols)
  browseURL("https://msdh.ms.gov/msdhsite/_static/14,0,420.html")
  
  demographics_pdf_path = read_html("https://msdh.ms.gov/msdhsite/_static/14,0,420,884.html") %>% 
    html_nodes("#page_top #pageContainer #pageContent .assetNow_pageAndFileList") %>% 
    html_nodes("li.assetNow_fileListing a") %>% 
    html_attr("href") %>% unique %>% .[2] # most updated file is the second link
  
  # cases by race and ethnicity
  rc = paste0("https://msdh.ms.gov/msdhsite/_static/", demographics_pdf_path) %>% 
    pdf_text %>% .[2] %>% str_split("\n") %>% .[[1]] %>% .[18] %>% str_squish %>% 
    str_split(" ", simplify = TRUE) %>% c() %>% .[2:20] %>% as.numeric
  
  # cases by race and ethnicity
  dc = paste0("https://msdh.ms.gov/msdhsite/_static/", demographics_pdf_path) %>% 
    pdf_text %>% .[4] %>% str_split("\n") %>% .[[1]] %>% .[18] %>% str_squish %>% 
    str_split(" ", simplify = TRUE) %>% c() %>% .[2:20] %>% as.numeric
  
  mississippi[["cases"]][["total"]] = rc[1]
  mississippi[["deaths"]][["total"]] = dc[1]
  
  mississippi[["cases"]][["race_AfrA"]] = rc[2] + rc[8] + rc[14]
  mississippi[["cases"]][["race_white"]] = rc[3] + rc[9] + rc[15]
  mississippi[["cases"]][["race_NatA"]] = rc[4] + rc[10] + rc[16]
  mississippi[["cases"]][["race_asian"]] = rc[5] + rc[11] + rc[17]
  mississippi[["cases"]][["race_other"]] = rc[6] + rc[12] + rc[18]
  mississippi[["cases"]][["race_unk"]] = rc[7] + rc[13] + rc[19]
  mississippi[["cases"]][["ethnicity_non_hispanic"]] = rc[2:7] %>% sum
  mississippi[["cases"]][["ethnicity_hispanic"]] = rc[8:13] %>% sum
  mississippi[["cases"]][["ethnicity_unk"]] = rc[14:19] %>% sum
  
  mississippi[["deaths"]][["race_AfrA"]] = dc[2] + dc[8] + dc[14]
  mississippi[["deaths"]][["race_white"]] = dc[3] + dc[9] + dc[15]
  mississippi[["deaths"]][["race_NatA"]] = dc[4] + dc[10] + dc[16]
  mississippi[["deaths"]][["race_asian"]] = dc[5] + dc[11] + dc[17]
  mississippi[["deaths"]][["race_other"]] = dc[6] + dc[12] + dc[18]
  mississippi[["deaths"]][["race_unk"]] = dc[7] + dc[13] + dc[19]
  mississippi[["deaths"]][["ethnicity_non_hispanic"]] = dc[2:7] %>% sum
  mississippi[["deaths"]][["ethnicity_hispanic"]] = dc[8:13] %>% sum
  mississippi[["deaths"]][["ethnicity_unk"]] = dc[14:19] %>% sum
  
  mississippi[["cases"]][["age_0_17"]] = get_information("MS, cases age_0_17: ") 
  mississippi[["cases"]][["age_18_29"]] = get_information("MS, cases age_18_29: ")
  mississippi[["cases"]][["age_30_39"]] = get_information("MS, cases age_30_39: ")
  mississippi[["cases"]][["age_40_49"]] = get_information("MS, cases age_40_49: ")
  mississippi[["cases"]][["age_50_59"]] = get_information("MS, cases age_50_59: ")
  mississippi[["cases"]][["age_60_69"]] = get_information("MS, cases age_60_69: ")
  mississippi[["cases"]][["age_70_79"]] = get_information("MS, cases age_70_79: ")
  mississippi[["cases"]][["age_80_89"]] = get_information("MS, cases age_80_89: ")
  mississippi[["cases"]][["age_90+"]] = get_information("MS, cases age_90+: ")
  
  mississippi[["deaths"]][["age_0_17"]] = get_information("MS, deaths age_0_17: ") 
  mississippi[["deaths"]][["age_18_29"]] = get_information("MS, deaths age_18_29: ")
  mississippi[["deaths"]][["age_30_39"]] = get_information("MS, deaths age_30_39: ")
  mississippi[["deaths"]][["age_40_49"]] = get_information("MS, deaths age_40_49: ")
  mississippi[["deaths"]][["age_50_59"]] = get_information("MS, deaths age_50_59: ")
  mississippi[["deaths"]][["age_60_69"]] = get_information("MS, deaths age_60_69: ")
  mississippi[["deaths"]][["age_70_79"]] = get_information("MS, deaths age_70_79: ")
  mississippi[["deaths"]][["age_80_89"]] = get_information("MS, deaths age_80_89: ")
  mississippi[["deaths"]][["age_90+"]] = get_information("MS, deaths age_90+: ")
  
  mississippi[["hospitalized"]][["age_0_17"]] = get_information(prompt = "MS, hosp age_0_17: ") 
  mississippi[["hospitalized"]][["age_18_29"]] = get_information(prompt = "MS, hosp age_18_29: ")
  mississippi[["hospitalized"]][["age_30_39"]] = get_information(prompt = "MS, hosp age_30_39: ")
  mississippi[["hospitalized"]][["age_40_49"]] = get_information(prompt = "MS, hosp age_40_49: ")
  mississippi[["hospitalized"]][["age_50_59"]] = get_information(prompt = "MS, hosp age_50_59: ")
  mississippi[["hospitalized"]][["age_60_69"]] = get_information(prompt = "MS, hosp age_60_69: ")
  mississippi[["hospitalized"]][["age_70_79"]] = get_information(prompt = "MS, hosp age_70_79: ")
  mississippi[["hospitalized"]][["age_80_89"]] = get_information(prompt = "MS, hosp age_80_89: ")
  mississippi[["hospitalized"]][["age_90+"]] = get_information(prompt = "MS, hosp age_90+: ")
  mississippi[["hospitalized"]][["total"]] = mississippi[["hospitalized"]][["age_0_17"]] +
    mississippi[["hospitalized"]][["age_18_29"]] +
    mississippi[["hospitalized"]][["age_30_39"]] +
    mississippi[["hospitalized"]][["age_40_49"]] +
    mississippi[["hospitalized"]][["age_50_59"]] +
    mississippi[["hospitalized"]][["age_60_69"]] +
    mississippi[["hospitalized"]][["age_70_79"]] +
    mississippi[["hospitalized"]][["age_80_89"]] +
    mississippi[["hospitalized"]][["age_90+"]]
  
  male_afra = get_information("MS, male deaths AfrA: ")
  male_white = get_information("MS, male deaths white: ")
  male_nata = get_information("MS, male deaths NatA: ")
  male_other = get_information("MS, male deaths other_race: ")

  female_afra = get_information("MS, female deaths AfrA: ")
  female_white = get_information("MS, female deaths white: ")
  female_nata = get_information("MS, female deaths NatA: ")
  female_other = get_information("MS, female deaths other_race: ")
  
  mississippi[["deaths"]][["sex_male"]] = male_afra + male_white + male_nata + male_other
  mississippi[["deaths"]][["sex_female"]] = female_afra + female_white + female_nata + female_other
  
  mississippi[["deaths"]][["race_AfrA"]] = male_afra + female_afra
  mississippi[["deaths"]][["race_white"]] = male_white + female_white
  mississippi[["deaths"]][["race_NatA"]] = male_nata + female_nata
  mississippi[["deaths"]][["race_other"]] = male_other + female_other
  
  mississippi[["cases"]][["sex_male"]] = get_information2("MS, cases gender male (whole %): ")
  mississippi[["cases"]][["sex_female"]] = get_information2("MS, cases gender female (whole %): ")
  mississippi[["cases"]][["sex_unk"]] = get_information2("MS, cases gender unk (whole %): ")
  
  mississippi[["tested"]][["total"]] = get_information("MS, total tested: ")
  
  final_mississippi = as_tibble(mississippi) %>% 
    standardize %>% 
    mutate(
      state_name = "Mississippi",
      Link = "https://msdh.ms.gov/msdhsite/_static/14,0,420.html",
      platform = "pdf",
      comments = "Age data manually entered",
      last.update = Sys.time() %>% as_date) %>% 
    select(
      state_name, Link,
      total.tested:hosp_gender,
      platform, comments, last.update)
  
  return(final_mississippi)
  
}

get_florida = function() {
  # Get pdf from: https://www.floridadisaster.org/covid19/covid-19-data-reports/
  url = "https://www.floridadisaster.org/covid19/"
  
  pdf_path = read_html(url) %>% 
    html_nodes("body .l-surround #mainContent #text-page-wrap .row") %>% 
    html_nodes(".main-column .panel .panel-collapse  .panel-body p a") %>% 
    .[[2]] %>% html_attr("href")

  data = paste0("https://www.floridadisaster.org", pdf_path) %>% pdf_text
  
  # Initialize skeleton
  skeleton = skeleton_table(fl_cols)
  
  # Demographic data is on
  demographic_data = data %>% .[3] %>% 
    str_split(., "\n") %>% .[[1]] %>% 
    str_squish()
  
  tested = data %>% .[1] %>% 
    str_split(., "\n") %>% .[[1]] %>% 
    str_squish() %>% .[[22]] %>% 
    str_split(., " ", simplify = TRUE) %>% .[1, 3] %>% 
    str_replace_all(., ",", "") %>% as.numeric
  
  cases = demographic_data %>% .[20] %>% 
    str_split(., " ", simplify = TRUE) %>% .[1, 2] %>% 
    str_replace(., ",", "") %>% as.numeric()
  
  hosp = demographic_data %>% .[20] %>% 
    str_split(., " ", simplify = TRUE) %>% .[1, 3] %>% 
    str_replace(., ",", "") %>% as.numeric()
  
  deaths = demographic_data %>% .[20] %>% 
    str_split(., " ", simplify = TRUE) %>% .[1, 4] %>% 
    str_replace(., ",", "") %>% as.numeric()

  females = demographic_data %>% .[9] %>% 
    str_split(., " ", simplify = TRUE) %>%  .[1, 10] %>% 
    str_replace(., ",", "") %>% as.numeric()
  
  males = demographic_data %>% .[10] %>% 
    str_split(., " ", simplify = TRUE) %>%  .[1, 10] %>% 
    str_replace(., ",", "") %>% as.numeric()

  sex_unk = demographic_data %>% .[11] %>% 
    str_split(., " ", simplify = TRUE) %>%  .[1, 10] %>% 
    str_replace(., ",", "") %>% as.numeric()
  
  # Each row contains data on cases, hosp and death
  between_0_and_4 = demographic_data %>% .[9] %>% 
    str_split(., " ", simplify = TRUE)
  
  between_0_and_4_case = between_0_and_4 %>% .[1, 3] %>% 
    str_replace(",","") %>% as.numeric
  between_0_and_4_hosp = between_0_and_4 %>% .[1, 5] %>% 
    str_replace(",","") %>% as.numeric
  between_0_and_4_death = between_0_and_4 %>% .[1, 7] %>% 
    str_replace(",","") %>% as.numeric
  
  between_5_and_14 = demographic_data %>% .[10] %>% 
    str_split(., " ", simplify = TRUE)
  
  between_5_and_14_case = between_5_and_14 %>% .[1, 3] %>% 
    str_replace(",","") %>% as.numeric
  between_5_and_14_hosp = between_5_and_14 %>% .[1, 5] %>% 
    str_replace(",","") %>% as.numeric
  between_5_and_14_death = between_5_and_14 %>% .[1, 7] %>% 
    str_replace(",","") %>% as.numeric
  
  between_15_and_24 = demographic_data %>% .[11] %>% 
    str_split(., " ", simplify = TRUE)
  
  between_15_and_24_case = between_15_and_24 %>% .[1, 3] %>% 
    str_replace(",","") %>% as.numeric
  between_15_and_24_hosp = between_15_and_24 %>% .[1, 5] %>% 
    str_replace(",","") %>% as.numeric
  between_15_and_24_death = between_15_and_24 %>% .[1, 7] %>% 
    str_replace(",","") %>% as.numeric
  
  between_25_and_34 = demographic_data %>% .[12] %>% 
    str_split(., " ", simplify = TRUE)
  
  between_25_and_34_case = between_25_and_34 %>% .[1, 3] %>% 
    str_replace(",","") %>% as.numeric
  between_25_and_34_hosp = between_25_and_34 %>% .[1, 5] %>% 
    str_replace(",","") %>% as.numeric
  between_25_and_34_death = between_25_and_34 %>% .[1, 7] %>% 
    str_replace(",","") %>% as.numeric
  
  between_35_and_44 = demographic_data %>% .[13] %>% 
    str_split(., " ", simplify = TRUE)
  
  between_35_and_44_case = between_35_and_44 %>% .[1, 3] %>% 
    str_replace(",","") %>% as.numeric
  between_35_and_44_hosp = between_35_and_44 %>% .[1, 5] %>% 
    str_replace(",","") %>% as.numeric
  between_35_and_44_death = between_35_and_44 %>% .[1, 7] %>% 
    str_replace(",","") %>% as.numeric
  
  between_45_and_54 = demographic_data %>% .[14] %>% 
    str_split(., " ", simplify = TRUE)
  
  between_45_and_54_case = between_45_and_54 %>% .[1, 3] %>% 
    str_replace(",","") %>% as.numeric
  between_45_and_54_hosp = between_45_and_54 %>% .[1, 5] %>% 
    str_replace(",","") %>% as.numeric
  between_45_and_54_death = between_45_and_54 %>% .[1, 7] %>% 
    str_replace(",","") %>% as.numeric
  
  between_55_and_64 = demographic_data %>% .[15] %>% 
    str_split(., " ", simplify = TRUE)
  
  between_55_and_64_case = between_55_and_64 %>% .[1, 3] %>% 
    str_replace(",","") %>% as.numeric
  between_55_and_64_hosp = between_55_and_64 %>% .[1, 5] %>% 
    str_replace(",","") %>% as.numeric
  between_55_and_64_death = between_55_and_64 %>% .[1, 7] %>% 
    str_replace(",","") %>% as.numeric
  
  between_65_and_74 = demographic_data %>% .[16] %>% 
    str_split(., " ", simplify = TRUE)
  
  between_65_and_74_case = between_65_and_74 %>% .[1, 3] %>% 
    str_replace(",","") %>% as.numeric
  between_65_and_74_hosp = between_65_and_74 %>% .[1, 5] %>% 
    str_replace(",","") %>% as.numeric
  between_65_and_74_death = between_65_and_74 %>% .[1, 7] %>% 
    str_replace(",","") %>% as.numeric
  
  between_75_and_84 = demographic_data %>% .[17] %>% 
    str_split(., " ", simplify = TRUE)
  
  between_75_and_84_case = between_75_and_84 %>% .[1, 3] %>% 
    str_replace(",","") %>% as.numeric
  between_75_and_84_hosp = between_75_and_84 %>% .[1, 5] %>% 
    str_replace(",","") %>% as.numeric
  between_75_and_84_death = between_75_and_84 %>% .[1, 7] %>% 
    str_replace(",","") %>% as.numeric
  
  older_than_85 = demographic_data %>% .[18] %>% 
    str_split(., " ", simplify = TRUE)
  
  older_than_85_case = older_than_85 %>% .[1, 3] %>% 
    str_replace(",","") %>% as.numeric
  older_than_85_hosp = older_than_85 %>% .[1, 5] %>% 
    str_replace(",","") %>% as.numeric
  older_than_85_death = older_than_85 %>% .[1, 7] %>% 
    str_replace(",","") %>% as.numeric
  
  unknown_age = demographic_data %>% .[19] %>% 
    str_split(., " ", simplify = TRUE)
  
  unknown_age_case = unknown_age %>% .[1, 2] %>% 
    str_replace(",","") %>% as.numeric
  unknown_age_hosp = unknown_age %>% .[1, 4] %>% 
    str_replace(",","") %>% as.numeric
  unknown_age_death = unknown_age %>% .[1, 6] %>% 
    str_replace(",","") %>% as.numeric
  
  hispanic = demographic_data %>% .[25] %>% 
    str_split(., " ", simplify = TRUE)
  
  hispanic_case = hispanic %>% .[1, 2] %>% 
    str_replace(",","") %>% as.numeric
  hispanic_hosp = hispanic %>% .[1, 4] %>% 
    str_replace(",","") %>% as.numeric
  hispanic_death = hispanic %>% .[1, 6] %>% 
    str_replace(",","") %>% as.numeric
  
  non_hispanic = demographic_data %>% .[26] %>% 
    str_split(., " ", simplify = TRUE)
  
  non_hispanic_case = non_hispanic %>% .[1, 2] %>% 
    str_replace(",","") %>% as.numeric
  non_hispanic_hosp = non_hispanic %>% .[1, 4] %>% 
    str_replace(",","") %>% as.numeric
  non_hispanic_death = non_hispanic %>% .[1, 6] %>% 
    str_replace(",","") %>% as.numeric
  
  unknown_ethnicity = demographic_data %>% .[27] %>% 
    str_split(., " ", simplify = TRUE)
  
  unknown_ethnicity_case = unknown_ethnicity %>% .[1, 2] %>% 
    str_replace(",","") %>% as.numeric
  unknown_ethnicity_hosp = unknown_ethnicity %>% .[1, 4] %>% 
    str_replace(",","") %>% as.numeric
  unknown_ethnicity_death = unknown_ethnicity %>% .[1, 6] %>% 
    str_replace(",","") %>% as.numeric
  
  white = demographic_data %>% .[33] %>% 
    str_split(., " ", simplify = TRUE)
  
  white_case = white %>% .[1, 2] %>% 
    str_replace(",","") %>% as.numeric
  white_hosp = white %>% .[1, 4] %>% 
    str_replace(",","") %>% as.numeric
  white_death = white %>% .[1, 6] %>% 
    str_replace(",","") %>% as.numeric
  
  black = demographic_data %>% .[37] %>% 
    str_split(., " ", simplify = TRUE) 
  
  black_case = black %>% .[1, 2] %>% 
    str_replace(",","") %>% as.numeric
  black_hosp = black %>% .[1, 4] %>% 
    str_replace(",","") %>% as.numeric
  black_death = black %>% .[1, 6] %>% 
    str_replace(",","") %>% as.numeric
  
  other_race = demographic_data %>% .[41] %>% 
    str_split(., " ", simplify = TRUE) 
  
  other_race_case = other_race %>% .[1, 3] %>% 
    str_replace(",","") %>% as.numeric
  other_race_hosp = other_race %>% .[1, 5] %>% 
    str_replace(",","") %>% as.numeric
  other_race_death = other_race %>% .[1, 7] %>% 
    str_replace(",","") %>% as.numeric
  
  unk_race = demographic_data %>% .[44] %>% 
    str_split(., " ", simplify = TRUE)
  
  unk_race_case = unk_race %>% .[1, 2] %>% 
    str_replace(",","") %>% as.numeric
  unk_race_hosp = unk_race %>% .[1, 4] %>% 
    str_replace(",","") %>% as.numeric
  unk_race_death = unk_race %>% .[1, 6] %>% 
    str_replace(",","") %>% as.numeric
  
  # Log all of the information
  skeleton[["tested"]][["total"]] = tested
  skeleton[["cases"]][["total"]] = cases
  skeleton[["hospitalized"]][["total"]] = hosp
  skeleton[["death"]][["total"]] = deaths
  
  skeleton[["cases"]][["sex_male"]] = males
  skeleton[["cases"]][["sex_female"]] = females
  skeleton[["cases"]][["sex_unk"]] = sex_unk
  
  skeleton[["cases"]][["age_0_4"]] = between_0_and_4_case
  skeleton[["cases"]][["age_5_14"]] = between_5_and_14_case
  skeleton[["cases"]][["age_15_24"]] = between_15_and_24_case
  skeleton[["cases"]][["age_25_34"]] = between_25_and_34_case
  skeleton[["cases"]][["age_35_44"]] = between_35_and_44_case
  skeleton[["cases"]][["age_45_54"]] = between_45_and_54_case
  skeleton[["cases"]][["age_55_64"]] = between_55_and_64_case
  skeleton[["cases"]][["age_65_74"]] = between_65_and_74_case
  skeleton[["cases"]][["age_75_84"]] = between_75_and_84_case
  skeleton[["cases"]][["age_85+"]] = older_than_85_case
  skeleton[["cases"]][["age_unk"]] = unknown_age_case
  skeleton[["cases"]][["ethnicity_hispanic"]] = hispanic_case
  skeleton[["cases"]][["ethnicity_non_hispanic"]] = non_hispanic_case
  skeleton[["cases"]][["ethnicity_unk"]] = unknown_ethnicity_case
  skeleton[["cases"]][["race_white"]] = white_case
  skeleton[["cases"]][["race_AfrA"]] = black_case
  skeleton[["cases"]][["race_other"]] = other_race_case
  skeleton[["cases"]][["race_unk"]] = unk_race_case
  
  skeleton[["hospitalized"]][["age_0_4"]] = between_0_and_4_hosp
  skeleton[["hospitalized"]][["age_5_14"]] = between_5_and_14_hosp
  skeleton[["hospitalized"]][["age_15_24"]] = between_15_and_24_hosp
  skeleton[["hospitalized"]][["age_25_34"]] = between_25_and_34_hosp
  skeleton[["hospitalized"]][["age_35_44"]] = between_35_and_44_hosp
  skeleton[["hospitalized"]][["age_45_54"]] = between_45_and_54_hosp
  skeleton[["hospitalized"]][["age_55_64"]] = between_55_and_64_hosp
  skeleton[["hospitalized"]][["age_65_74"]] = between_65_and_74_hosp
  skeleton[["hospitalized"]][["age_75_84"]] = between_75_and_84_hosp
  skeleton[["hospitalized"]][["age_85+"]] = older_than_85_hosp
  skeleton[["hospitalized"]][["age_unk"]] = unknown_age_hosp
  skeleton[["hospitalized"]][["ethnicity_hispanic"]] = hispanic_hosp
  skeleton[["hospitalized"]][["ethnicity_non_hispanic"]] = non_hispanic_hosp
  skeleton[["hospitalized"]][["ethnicity_unk"]] = unknown_ethnicity_hosp
  skeleton[["hospitalized"]][["race_white"]] = white_hosp
  skeleton[["hospitalized"]][["race_AfrA"]] = black_hosp
  skeleton[["hospitalized"]][["race_other"]] = other_race_hosp
  skeleton[["hospitalized"]][["race_unk"]] = unk_race_hosp
  
  skeleton[["deaths"]][["total"]] = deaths
  skeleton[["deaths"]][["age_0_4"]] = between_0_and_4_death
  skeleton[["deaths"]][["age_5_14"]] = between_5_and_14_death
  skeleton[["deaths"]][["age_15_24"]] = between_15_and_24_death
  skeleton[["deaths"]][["age_25_34"]] = between_25_and_34_death
  skeleton[["deaths"]][["age_35_44"]] = between_35_and_44_death
  skeleton[["deaths"]][["age_45_54"]] = between_45_and_54_death
  skeleton[["deaths"]][["age_55_64"]] = between_55_and_64_death
  skeleton[["deaths"]][["age_65_74"]] = between_65_and_74_death
  skeleton[["deaths"]][["age_75_84"]] = between_75_and_84_death
  skeleton[["deaths"]][["age_85+"]] = older_than_85_death
  skeleton[["deaths"]][["age_unk"]] = unknown_age_death
  skeleton[["deaths"]][["ethnicity_hispanic"]] = hispanic_death
  skeleton[["deaths"]][["ethnicity_non_hispanic"]] = non_hispanic_death
  skeleton[["deaths"]][["ethnicity_unk"]] = unknown_ethnicity_death
  skeleton[["deaths"]][["race_white"]] = white_death
  skeleton[["deaths"]][["race_AfrA"]] = black_death
  skeleton[["deaths"]][["race_other"]] = other_race_death
  skeleton[["deaths"]][["race_unk"]] = unk_race_death
  
  as_tibble(skeleton) %>% 
    standardize %>% 
    mutate(
      state_name = "Florida",
      Link = url,
      platform = "pdf",
      comments = "Age data only available in images on site",
      last.update = Sys.time() %>% as_date) %>% 
    select(
      state_name, Link,
      total.tested:hosp_gender,
      platform, comments, last.update)
}

get_tennessee = function(date = "today") {
  
  # Just in case you don't start bringing together data until the midnight after
  if (date == "today") {
    date_str = Sys.time() %>% as_date
  } else { # If you need another date to put in
    date_str = date
  }
  
  # Scrape the website to get to the datasets by their source link
  url = "https://www.tn.gov/health/cedep/ncov/data/downloadable-datasets.html"
  
  xlsx_paths = read_html(url) %>% 
    html_nodes("body #main .row .col-lg-8 article div .tn-panel .panel") %>% 
    html_nodes("div .tn-rte p a") %>% 
    html_attr("href")
  
  age_temp = tempfile(fileext = ".xlsx")
  case_temp = tempfile(fileext = ".xlsx")
  demo_temp = tempfile(fileext = ".xlsx")
  
  age_url = "https://www.tn.gov/content/dam/tn/health/documents/cedep/novel-coronavirus/datasets/Public-Dataset-Age.XLSX"
  case_url = "https://www.tn.gov/content/dam/tn/health/documents/cedep/novel-coronavirus/datasets/Public-Dataset-County-New.XLSX"
  demo_url = "https://www.tn.gov/content/dam/tn/health/documents/cedep/novel-coronavirus/datasets/Public-Dataset-RaceEthSex.XLSX"

  download.file(age_url, destfile = age_temp, mode = 'wb')
  download.file(case_url, destfile = case_temp, mode = 'wb')
  download.file(demo_url, destfile = demo_temp, mode = 'wb')
  
  age_data = read_excel(age_temp, sheet = 1) %>%
    filter(DATE == as_date(date_str))
  case_data = read_excel(case_temp, sheet = 1) %>%
    filter(DATE == as_date(date_str))
  demo_data = read_excel(demo_temp, sheet = 1) %>%
    filter(Date == as_date(date_str))
  
  skeleton = skeleton_table(tn_cols)
  
  tests = case_data %>% pull(TOTAL_TESTS) %>% sum
  cases = case_data %>% pull(TOTAL_CASES) %>% sum
  negatives = case_data %>% pull(NEG_TESTS) %>% sum
  deaths = case_data %>% pull(TOTAL_DEATHS) %>% sum
  
  ar_cases = age_data %>% pull(AR_CASECOUNT)
  ar_deaths = age_data %>% pull(AR_TOTALDEATHS)
  
  race_cases = demo_data %>% filter(Category == "RACE")
  ethnicity_cases = demo_data %>% filter(Category == "ETHNICITY") 
  sex_cases = demo_data %>% filter(Category == "SEX")
  
  race_deaths = demo_data %>% filter(Category == "RACE")
  ethnicity_deaths = demo_data %>% filter(Category == "ETHNICITY")
  sex_deaths = demo_data %>% filter(Category == "SEX")
  
  # Log all the data from the Excel sheet
  skeleton[["tested"]][["total"]] = tests
  
  skeleton[["cases"]][["total"]] = cases
  skeleton[["cases"]][["age_0_10"]] = ar_cases[1] 
  skeleton[["cases"]][["age_11_20"]] = ar_cases[2]
  skeleton[["cases"]][["age_21_30"]] = ar_cases[3]
  skeleton[["cases"]][["age_31_40"]] = ar_cases[4]
  skeleton[["cases"]][["age_41_50"]] = ar_cases[5]
  skeleton[["cases"]][["age_51_60"]] = ar_cases[6]
  skeleton[["cases"]][["age_61_70"]] = ar_cases[7]
  skeleton[["cases"]][["age_71_80"]] = ar_cases[8]
  skeleton[["cases"]][["age_81+"]] = ar_cases[9]
  skeleton[["cases"]][["age_unk"]] = ar_cases[10]
  skeleton[["cases"]][["sex_male"]] = sex_cases %>% 
    filter(CAT_DETAIL == "Male") %>% pull(Cat_CaseCount) 
  skeleton[["cases"]][["sex_female"]] = sex_cases %>% 
    filter(CAT_DETAIL == "Female") %>% pull(Cat_CaseCount) 
  skeleton[["cases"]][["sex_unk"]] = sex_cases %>% 
    filter(CAT_DETAIL == "Pending") %>% pull(Cat_CaseCount) 
  skeleton[["cases"]][["race_white"]] = race_cases %>% 
    filter(CAT_DETAIL == "White") %>% pull(Cat_CaseCount) 
  skeleton[["cases"]][["race_AfrA"]] = race_cases %>% 
    filter(CAT_DETAIL == "Black or African American") %>% pull(Cat_CaseCount) 
  skeleton[["cases"]][["race_other"]] = race_cases %>% 
    filter(CAT_DETAIL == "Other/Multiracial") %>% pull(Cat_CaseCount)
  skeleton[["cases"]][["race_asian"]] = race_cases %>% 
    filter(CAT_DETAIL == "Asian") %>% pull(Cat_CaseCount) 
  skeleton[["cases"]][["race_unk"]] = race_cases %>% 
    filter(CAT_DETAIL == "Pending") %>% pull(Cat_CaseCount) 
  skeleton[["cases"]][["ethnicity_hispanic"]] = ethnicity_cases %>% 
    filter(CAT_DETAIL == "Hispanic") %>% pull(Cat_CaseCount)
  skeleton[["cases"]][["ethnicity_non_hispanic"]] = ethnicity_cases %>% 
    filter(CAT_DETAIL == "Not Hispanic or Latino") %>% pull(Cat_CaseCount)
  skeleton[["cases"]][["ethnicity_unk"]] = ethnicity_cases %>% 
    filter(CAT_DETAIL == "Pending") %>% pull(Cat_CaseCount)
  
  skeleton[["negatives"]][["total"]] = negatives
  
  skeleton[["deaths"]][["total"]] = deaths
  skeleton[["deaths"]][["age_0_10"]] = ar_deaths[1] 
  skeleton[["deaths"]][["age_11_20"]] = ar_deaths[2]
  skeleton[["deaths"]][["age_21_30"]] = ar_deaths[3]
  skeleton[["deaths"]][["age_31_40"]] = ar_deaths[4]
  skeleton[["deaths"]][["age_41_50"]] = ar_deaths[5]
  skeleton[["deaths"]][["age_51_60"]] = ar_deaths[6]
  skeleton[["deaths"]][["age_61_70"]] = ar_deaths[7]
  skeleton[["deaths"]][["age_71_80"]] = ar_deaths[8]
  skeleton[["deaths"]][["age_81+"]] = ar_deaths[9]
  skeleton[["deaths"]][["age_unk"]] = ar_deaths[10]
  skeleton[["deaths"]][["sex_male"]] = sex_deaths %>% 
    filter(CAT_DETAIL == "Male") %>% pull(CAT_DEATHCOUNT) 
  skeleton[["deaths"]][["sex_female"]] = sex_deaths %>% 
    filter(CAT_DETAIL == "Female") %>% pull(CAT_DEATHCOUNT) 
  skeleton[["deaths"]][["sex_unk"]] = sex_deaths %>% 
    filter(CAT_DETAIL == "Pending") %>% pull(CAT_DEATHCOUNT) 
  skeleton[["deaths"]][["race_white"]] = race_deaths %>% 
    filter(CAT_DETAIL == "White") %>% pull(CAT_DEATHCOUNT) 
  skeleton[["deaths"]][["race_AfrA"]] = race_deaths %>% 
    filter(CAT_DETAIL == "Black or African American") %>% pull(CAT_DEATHCOUNT) 
  skeleton[["deaths"]][["race_other"]] = race_deaths %>% 
    filter(CAT_DETAIL == "Other/Multiracial") %>% pull(CAT_DEATHCOUNT)
  skeleton[["deaths"]][["race_asian"]] = race_deaths %>% 
    filter(CAT_DETAIL == "Asian") %>% pull(CAT_DEATHCOUNT) 
  skeleton[["deaths"]][["race_unk"]] = race_deaths %>% 
    filter(CAT_DETAIL == "Pending") %>% pull(CAT_DEATHCOUNT) 
  skeleton[["deaths"]][["ethnicity_hispanic"]] = ethnicity_deaths %>% 
    filter(CAT_DETAIL == "Hispanic") %>% pull(CAT_DEATHCOUNT)
  skeleton[["deaths"]][["ethnicity_non_hispanic"]] = ethnicity_deaths %>% 
    filter(CAT_DETAIL == "Not Hispanic or Latino") %>% pull(CAT_DEATHCOUNT)
  skeleton[["deaths"]][["ethnicity_unk"]] = ethnicity_deaths %>% 
    filter(CAT_DETAIL == "Pending") %>% pull(CAT_DEATHCOUNT)
  
  browseURL("https://www.tn.gov/content/tn/health/cedep/ncov/data.html")
  skeleton[["tested"]][["total"]] = get_information("TN: Total tested: ")
  skeleton[["hospitalized"]][["total"]] = get_information("TN: Total hospitalized: ")
  
  as_tibble(skeleton) %>% 
    standardize %>% 
    mutate(
      state_name = "Tennessee",
      Link = url,
      platform = "pdf",
      comments = "",
      last.update = Sys.time() %>% as_date) %>% 
    select(
      state_name, Link,
      total.tested:hosp_gender,
      platform, comments, last.update)
}

get_north_carolina = function() {
  
  north_carolina = skeleton_table(nc_cols)

  browseURL("https://covid19.ncdhhs.gov/dashboard#by-age")
  
  north_carolina[["tested"]][["total"]] = get_information("NC, Total tested: ") 
  
  north_carolina[["cases"]][["total"]] = get_information("NC, Total cases: ") 
  north_carolina[["cases"]][["age_0_17"]] = get_information("NC, case age_0_17: ") 
  north_carolina[["cases"]][["age_18_24"]] = get_information("NC, case age_18_24: ")
  north_carolina[["cases"]][["age_25_49"]] = get_information("NC, case age_25_49: ")
  north_carolina[["cases"]][["age_50_64"]] = get_information("NC, case age_50_64: ")
  north_carolina[["cases"]][["age_65_74"]] = get_information("NC, deaths age_65_74: ")
  north_carolina[["cases"]][["age_75+"]] = get_information("NC, deaths age_75+: ")
  
  north_carolina[["cases"]][["race_NatA"]] = get_information("NC, case race NatA: ")
  north_carolina[["cases"]][["race_asian"]] = get_information("NC, case race asian: ")
  north_carolina[["cases"]][["race_AfrA"]] = get_information("NC, case race AfrA: ")
  north_carolina[["cases"]][["race_pac"]] = get_information("NC, case race Pac Island: ")
  north_carolina[["cases"]][["race_white"]] = get_information("NC, case race white: ")
  north_carolina[["cases"]][["race_other"]] = get_information("NC, case race other: ")
  
  north_carolina[["cases"]][["ethnicity_hispanic"]] = get_information("NC, case hispanic: ")
  north_carolina[["cases"]][["ethnicity_non_hispanic"]] = get_information("NC, case non_hispanic: ")
  
  north_carolina[["cases"]][["sex_male"]] = get_information("NC, case sex male: ")
  north_carolina[["cases"]][["sex_female"]] = get_information("NC, case sex female: ")
  
  north_carolina[["cases"]][["race_unk"]] = get_information("NC, case race unknown: ")
  north_carolina[["cases"]][["ethnicity_unk"]] = get_information("NC, case hisp unknown: ")
  north_carolina[["cases"]][["age_unk"]] = get_information("NC, case age_unk: ")
  north_carolina[["cases"]][["sex_unk"]] = get_information("NC, case sex unknown: ")
  
  north_carolina[["deaths"]][["total"]] = get_information("NC, Total deaths: ") 
  north_carolina[["deaths"]][["age_0_17"]] = get_information("NC, deaths age_0_17: ") 
  north_carolina[["deaths"]][["age_18_24"]] = get_information("NC, deaths age_18_24: ")
  north_carolina[["deaths"]][["age_25_49"]] = get_information("NC, deaths age_25_49: ")
  north_carolina[["deaths"]][["age_50_64"]] = get_information("NC, deaths age_50_64: ")
  north_carolina[["deaths"]][["age_65_74"]] = get_information("NC, deaths age_65_74: ")
  north_carolina[["deaths"]][["age_75+"]] = get_information("NC, deaths age_75+: ")
  
  north_carolina[["deaths"]][["race_NatA"]] = get_information("NC, deaths race NatA: ")
  north_carolina[["deaths"]][["race_asian"]] = get_information("NC, deaths race asian: ")
  north_carolina[["deaths"]][["race_AfrA"]] = get_information("NC, deaths race AfrA: ")
  north_carolina[["deaths"]][["race_pac"]] = get_information("NC, deaths race Pac Island: ")
  north_carolina[["deaths"]][["race_white"]] = get_information("NC, deaths race white: ")
  north_carolina[["deaths"]][["race_other"]] = get_information("NC, deaths race other: ")
  
  north_carolina[["deaths"]][["ethnicity_hispanic"]] = get_information("NC, deaths hispanic: ")
  north_carolina[["deaths"]][["ethnicity_non_hispanic"]] = get_information("NC, deaths non_hispanic: ")
  
  north_carolina[["deaths"]][["sex_male"]] = get_information("NC, deaths sex male: ")
  north_carolina[["deaths"]][["sex_female"]] = get_information("NC, deaths sex female: ")
  
  north_carolina[["deaths"]][["race_unk"]] = get_information("NC, deaths race unknown: ")
  north_carolina[["deaths"]][["ethnicity_unk"]] = get_information("NC, deaths hisp unknown: ")
  north_carolina[["deaths"]][["age_unk"]] = get_information("NC, deaths age_unk: ")
  north_carolina[["deaths"]][["sex_unk"]] = get_information("NC, deaths sex unknown: ")
  
  final_north_carolina = as_tibble(north_carolina) %>% 
    standardize %>% 
    mutate(
      state_name = "North Carolina",
      Link = "https://covid19.ncdhhs.gov/dashboard#by-gender",
      platform = "pdf",
      comments = "Age data manually entered as percentages.",
      last.update = Sys.time() %>% as_date) %>% 
    select(
      state_name, Link,
      total.tested:hosp_gender,
      platform, comments, last.update)
  
  
  return(final_north_carolina)
}

get_dc = function() {
  
  skeleton = skeleton_table(dc_cols)
  
  url = "https://coronavirus.dc.gov/page/coronavirus-data"
  data = read_html(url) %>% 
    html_nodes("body #page #section-content #zone-content-wrapper") %>% 
    html_nodes("#zone-content #region-content .region-inner") %>% 
    html_nodes("#block-system-main .block-inner .content article") %>% 
    html_nodes(".content .field .field-items .field-item ul li a") %>% 
    html_attr("href")
  
  # Get the link for the most recent xlsx file
  most_recent = data[str_detect(data, "xlsx")] %>% .[1]
  data_url = paste0("https://coronavirus.dc.gov", most_recent)
  
  # Download temporarily and extract data
  temp = tempfile(fileext = ".xlsx")
  download.file(data_url, destfile = temp, mode = 'wb')
  overall_stats = read_excel(temp, sheet = 1)
  case_by_race_ethnicity = read_excel(temp, sheet = 3)
  deaths_by_race = read_excel(temp, sheet = 4)
  
  # Log all the totals from this file
  today_stats = overall_stats[, ncol(overall_stats)] %>% unlist()
  
  skeleton[["tested"]][["total"]] = today_stats[2]
  skeleton[["cases"]][["total"]] = today_stats[3]
  skeleton[["deaths"]][["total"]] = today_stats[4]
  skeleton[["recovered"]][["total"]] = today_stats[5]
  
  # Log cases by race and ethnicity
  today_race_ethn = case_by_race_ethnicity[, ncol(case_by_race_ethnicity)] %>% 
    unlist
  
  skeleton[["cases"]][["race_unk"]] = today_race_ethn[3] + 
    today_race_ethn[10]
  skeleton[["cases"]][["race_white"]] = today_race_ethn[4]
  skeleton[["cases"]][["race_AfrA"]] = today_race_ethn[5]
  skeleton[["cases"]][["race_asian"]] = today_race_ethn[6]
  skeleton[["cases"]][["race_NatA"]] = today_race_ethn[7]
  skeleton[["cases"]][["race_pac"]] = today_race_ethn[8]
  skeleton[["cases"]][["race_multi_other"]] = today_race_ethn[9]
  skeleton[["cases"]][["race_unk"]] = today_race_ethn[10]
  skeleton[["cases"]][["ethnicity_unk"]] = today_race_ethn[12] +
    today_race_ethn[15]
  skeleton[["cases"]][["ethnicity_hispanic"]] = today_race_ethn[13]
  skeleton[["cases"]][["ethnicity_non_hispanic"]] = today_race_ethn[14]
  
  # Logging deaths
  today_deaths = deaths_by_race[, ncol(deaths_by_race)] %>% unlist()
  skeleton[["deaths"]][["race_asian"]] = today_deaths[3]
  skeleton[["deaths"]][["race_AfrA"]] = today_deaths[4]
  skeleton[["deaths"]][["race_hispanic"]] = today_deaths[5]
  skeleton[["deaths"]][["race_white"]] = today_deaths[6]
  skeleton[["deaths"]][["race_unk"]] = today_deaths[7]
  
  browseURL("https://coronavirus.dc.gov/page/coronavirus-data")
  skeleton[["cases"]][["age_0_4"]] = get_information("DC: cases age 0-4: ")
  skeleton[["cases"]][["age_5_14"]]  = get_information("DC: cases age 5-14: ")
  skeleton[["cases"]][["age_15_19"]] = get_information("DC: cases age 15-19: ")
  skeleton[["cases"]][["age_20_24"]] = get_information("DC: cases age 20-24: ")
  skeleton[["cases"]][["age_25_34"]] = get_information("DC: cases age 25-34: ")
  skeleton[["cases"]][["age_35_44"]] = get_information("DC: cases age 35-44: ")
  skeleton[["cases"]][["age_45_54"]] = get_information("DC: cases age 45-54: ")
  skeleton[["cases"]][["age_55_64"]] = get_information("DC: cases age 55-64: ")
  skeleton[["cases"]][["age_64_74"]] = get_information("DC: cases age 64-74: ")
  skeleton[["cases"]][["age_75+"]] = get_information("DC: cases age 75+: ")
  
  skeleton[["cases"]][["sex_female"]] = get_information("DC: cases female: ")
  skeleton[["cases"]][["sex_male"]] = get_information("DC: cases male: ")
  skeleton[["cases"]][["sex_unk"]] = get_information("DC: cases sex unk: ")
  
  as_tibble(skeleton) %>% 
    standardize %>% 
    mutate(
      state_name = "District of Columbia",
      Link = url,
      platform = "xlsx",
      comments = "Deaths are combined between race and ethnicity",
      last.update = Sys.time() %>% as_date) %>% 
    select(
      state_name, Link,
      total.tested:hosp_gender,
      platform, comments, last.update)
}

get_south_carolina = function() {

  south_carolina = skeleton_table(sc_cols)
  
  browseURL("https://scdhec.gov/infectious-diseases/viruses/coronavirus-disease-2019-covid-19/sc-testing-data-projections-covid-19")
  south_carolina[["tested"]][["total"]] = get_information("SC, tested total: ")
  
  
  browseURL("https://scdhec.gov/sc-demographic-data-covid-19")
  
  south_carolina[["cases"]][["total"]] = get_information("SC, cases total: ")
  south_carolina[["cases"]][["age_0_10"]] = get_information2("SC, cases age_0_10 (enter as whole %): ")
  south_carolina[["cases"]][["age_11_20"]] = get_information2("SC, cases age_11_20 (enter as whole %): ")
  south_carolina[["cases"]][["age_21_30"]] = get_information2("SC, cases age_21_30 (enter as whole %): ")
  south_carolina[["cases"]][["age_31_40"]] = get_information2("SC, cases age_31_40 (enter as whole %): ")
  south_carolina[["cases"]][["age_41_50"]] = get_information2("SC, cases age_41_50 (enter as whole %): ")
  south_carolina[["cases"]][["age_51_60"]] = get_information2("SC, cases age_51_60 (enter as whole %): ")
  south_carolina[["cases"]][["age_61_70"]] = get_information2("SC, cases age_61_70 (enter as whole %): ")
  south_carolina[["cases"]][["age_71_80"]] = get_information2("SC, cases age_71_80 (enter as whole %): ")
  south_carolina[["cases"]][["age_81+"]] = get_information2("SC, cases age_81+ (enter as whole %): ") 
  
  south_carolina[["cases"]][["race_AfrA"]] = get_information2("SC, cases race_Afra (enter as whole %): ")
  south_carolina[["cases"]][["race_white"]] = get_information2("SC, cases race_white (enter as whole %): ")
  south_carolina[["cases"]][["race_asian"]] = get_information2("SC, cases race_asian (enter as whole %): ")
  south_carolina[["cases"]][["race_unk"]] = get_information2("SC, cases race_unk (enter as whole %): ")
  south_carolina[["cases"]][["race_other"]] = get_information2("SC, cases race_other (enter as whole %): ")
  
  south_carolina[["cases"]][["ethnicity_non_hispanic"]] = get_information2("SC, cases ethnicity_non_hispanic (enter as whole %): ")
  south_carolina[["cases"]][["ethnicity_hispanic"]] = get_information2("SC, cases ethnicity_hispanic (enter as whole %): ")
  south_carolina[["cases"]][["ethnicity_unk"]] = get_information2("SC, cases ethnicity_unk (enter as whole %): ")
  
  south_carolina[["cases"]][["sex_female"]] = get_information2("SC, cases female (enter as whole %): ")
  south_carolina[["cases"]][["sex_male"]] = get_information2("SC, cases male (enter as whole %): ")
  south_carolina[["cases"]][["sex_unk"]] = get_information2("SC, cases unk (enter as whole %): ")
  
  south_carolina[["hospitalized"]][["total"]] = get_information("SC, Total hosp (calc.): ")
  
  south_carolina[["deaths"]][["total"]] = get_information("SC, Total deaths: ")
  south_carolina[["deaths"]][["age_0_10"]] = get_information2("SC, deaths age_0_10 (enter as whole %): ")
  south_carolina[["deaths"]][["age_11_20"]] = get_information2("SC, deaths age_11_20 (enter as whole %): ")
  south_carolina[["deaths"]][["age_21_30"]] = get_information2("SC, deaths age_21_30 (enter as whole %): ")
  south_carolina[["deaths"]][["age_31_40"]] = get_information2("SC, deaths age_31_40 (enter as whole %): ")
  south_carolina[["deaths"]][["age_41_50"]] = get_information2("SC, deaths age_41_50 (enter as whole %): ")
  south_carolina[["deaths"]][["age_51_60"]] = get_information2("SC, deaths age_51_60 (enter as whole %): ")
  south_carolina[["deaths"]][["age_61_70"]] = get_information2("SC, deaths age_61_70 (enter as whole %): ")
  south_carolina[["deaths"]][["age_71_80"]] = get_information2("SC, deaths age_71_80 (enter as whole %): ")
  south_carolina[["deaths"]][["age_81+"]] = get_information2("SC, deaths age_81+ (enter as whole %): ") 
  
  south_carolina[["deaths"]][["race_AfrA"]] = get_information2("SC, deaths race_Afra (enter as whole %): ")
  south_carolina[["deaths"]][["race_white"]] = get_information2("SC, deaths race_white (enter as whole %): ")
  south_carolina[["deaths"]][["race_asian"]] = get_information2("SC, deaths race_asian (enter as whole %): ")
  south_carolina[["deaths"]][["race_unk"]] = get_information2("SC, deaths race_unk (enter as whole %): ")
  south_carolina[["deaths"]][["race_other"]] = get_information2("SC, deaths race_other (enter as whole %): ")
  
  south_carolina[["deaths"]][["ethnicity_hispanic"]] = get_information2("SC, deaths ethnicity_hispanic (enter as whole %): ")
  south_carolina[["deaths"]][["ethnicity_non_hispanic"]] = get_information2("SC, deaths ethnicity_non_hispanic (enter as whole %): ")
  south_carolina[["deaths"]][["ethnicity_unk"]] = get_information2("SC, deaths ethnicity_unk (enter as whole %): ")
  
  south_carolina[["deaths"]][["sex_female"]] = get_information2("SC, deaths female (enter as whole %): ")
  south_carolina[["deaths"]][["sex_male"]] = get_information2("SC, deaths male (enter as whole %): ")
  south_carolina[["deaths"]][["sex_unk"]] = get_information2("SC, deaths unk (enter as whole %): ")
  
  final_south_carolina = as_tibble(south_carolina) %>% 
    standardize %>% 
    mutate(
      state_name = "South Carolina",
      Link = "https://doh.sd.gov/news/coronavirus.aspx#SD",
      platform = "pdf",
      comments = "Demographic and age data manually entered",
      last.update = Sys.time() %>% as_date) %>% 
    select(
      state_name, Link,
      total.tested:hosp_gender,
      platform, comments, last.update)
  
  return(final_south_carolina)
}

get_new_jersey = function() {
  
  skeleton = skeleton_table(nj_cols)
  
  browseURL("https://covid19.nj.gov/#live-updates")
  skeleton[["tested"]][["total"]] = get_information("NJ: Total tested?: ")
  skeleton[["deaths"]][["total"]] = get_information("NJ, Deaths total (conf. + prob.): ")
  
  browseURL("https://www.nj.gov/health/cd/documents/topics/NCOV/COVID_Confirmed_Case_Summary.pdf")
  
  skeleton[["cases"]][["total"]] = get_information("NJ, Cases total: ")
  skeleton[["cases"]][["race_nh_asian"]] = get_information("NJ, Cases race_nh_asian: ")
  skeleton[["cases"]][["race_nh_AfrA"]] = get_information("NJ, Cases race_nh_AfrA: ")
  skeleton[["cases"]][["race_hispanic"]] = get_information("NJ, Cases race_hispanic: ")
  skeleton[["cases"]][["race_nh_other"]] = get_information("NJ, Cases race_nh_other: ")
  skeleton[["cases"]][["race_nh_white"]] = get_information("NJ, Cases race_nh_white: ")
  skeleton[["cases"]][["age_0_4"]] = get_information("NJ, Cases age 0-4: ")
  skeleton[["cases"]][["age_5_17"]] = get_information("NJ, Cases age 5-17: ")
  skeleton[["cases"]][["age_18_29"]] = get_information("NJ, Cases age 18-29: ")
  skeleton[["cases"]][["age_30_49"]] = get_information("NJ, Cases age 30-49: ")
  skeleton[["cases"]][["age_50_64"]] = get_information("NJ, Cases age 50-64: ")
  skeleton[["cases"]][["age_65_79"]] = get_information("NJ, Cases age 65-79: ")
  skeleton[["cases"]][["age_80+"]] = get_information("NJ, Cases age 80+: ")
  skeleton[["cases"]][["sex_male"]] = get_information("NJ, Cases sex male: ")
  skeleton[["cases"]][["sex_female"]] = get_information("NJ, Cases sex female: ")
  skeleton[["cases"]][["sex_unk"]] = get_information("NJ, Cases sex unknown: ")

  skeleton[["hospitalized"]][["total"]] = get_information("NJ, Hosp. total: ")
  skeleton[["hospitalized"]][["race_nh_asian"]] = get_information("NJ, Hosp. race_nh_asian: ")
  skeleton[["hospitalized"]][["race_nh_AfrA"]] = get_information("NJ, Hosp. race_nh_AfrA: ")
  skeleton[["hospitalized"]][["race_hispanic"]] = get_information("NJ, Hosp. race_hispanic: ")
  skeleton[["hospitalized"]][["race_nh_other"]] = get_information("NJ, Hosp. race_nh_other: ")
  skeleton[["hospitalized"]][["race_nh_white"]] = get_information("NJ, Hosp. race_nh_white: ")
  skeleton[["hospitalized"]][["age_0_4"]] = get_information("NJ, Hosp. age 0-4: ")
  skeleton[["hospitalized"]][["age_5_17"]] = get_information("NJ, Hosp. age 5-17: ")
  skeleton[["hospitalized"]][["age_18_29"]] = get_information("NJ, Hosp. age 18-29: ")
  skeleton[["hospitalized"]][["age_30_49"]] = get_information("NJ, Hosp. age 30-49: ")
  skeleton[["hospitalized"]][["age_50_64"]] = get_information("NJ, Hosp. age 50-64: ")
  skeleton[["hospitalized"]][["age_65_79"]] = get_information("NJ, Hosp. age 65-79: ")
  skeleton[["hospitalized"]][["age_80+"]] = get_information("NJ, Hosp. age 80+: ")
  skeleton[["hospitalized"]][["sex_male"]] = get_information("NJ, Hosp. sex male: ")
  skeleton[["hospitalized"]][["sex_female"]] = get_information("NJ, Hosp. sex female: ")
  skeleton[["hospitalized"]][["sex_unk"]] = get_information("NJ, Hosp. sex unknown: ")
  
  skeleton[["deaths"]][["race_nh_asian"]] = get_information("NJ, Deaths race_nh_asian: ")
  skeleton[["deaths"]][["race_nh_AfrA"]] = get_information("NJ, Deaths race_nh_AfrA: ")
  skeleton[["deaths"]][["race_hispanic"]] = get_information("NJ, Deaths race_hispanic: ")
  skeleton[["deaths"]][["race_nh_other"]] = get_information("NJ, Deaths race_nh_other: ")
  skeleton[["deaths"]][["race_nh_white"]] = get_information("NJ, Deaths race_nh_white: ")
  skeleton[["deaths"]][["age_0_4"]] = get_information("NJ, Deaths age 0-4: ")
  skeleton[["deaths"]][["age_5_17"]] = get_information("NJ, Deaths age 5-17: ")
  skeleton[["deaths"]][["age_18_29"]] = get_information("NJ, Deaths age 18-29: ")
  skeleton[["deaths"]][["age_30_49"]] = get_information("NJ, Deaths age 30-49: ")
  skeleton[["deaths"]][["age_50_64"]] = get_information("NJ, Deaths age 50-64: ")
  skeleton[["deaths"]][["age_65_79"]] = get_information("NJ, Deaths age 65-79: ")
  skeleton[["deaths"]][["age_80+"]] = get_information("NJ, Deaths age 80+: ")
  skeleton[["deaths"]][["sex_male"]] = get_information("NJ, Deaths sex male: ")
  skeleton[["deaths"]][["sex_female"]] = get_information("NJ, Deaths sex female: ")
  skeleton[["deaths"]][["sex_unk"]] = get_information("NJ, Deaths sex unknown: ")
  
  as_tibble(skeleton) %>% 
    standardize %>% 
    mutate(
      state_name = "New Jersey",
      Link = "https://www.nj.gov/health/cd/documents/topics/NCOV/COVID_Confirmed_Case_Summary.pdf",
      platform = "pdf",
      comments = "Site combined race and ethnicity, so calculation needed",
      last.update = Sys.time() %>% as_date) %>% 
    select(
      state_name, Link,
      total.tested:hosp_gender,
      platform, comments, last.update)
  
}

get_new_hampshire = function() {
  
  browseURL("https://www.nh.gov/covid19/dashboard/summary.htm")
  
  skeleton = skeleton_table(nh_cols)
  
  skeleton[["cases"]][["total"]] = get_information("NH, total cases: ")
  skeleton[["hospitalized"]][["total"]] = get_information("NH, total hosp: ")
  skeleton[["deaths"]][["total"]] = get_information("NH, total death: ")
  
  skeleton[["cases"]][["sex_female"]] = get_information("NH, Cases female: ")
  skeleton[["cases"]][["sex_male"]] = get_information("NH, Cases male: ")
  skeleton[["cases"]][["sex_unk"]] = get_information("NH, Cases sex unk: ")
  
  skeleton[["hospitalized"]][["sex_female"]] = get_information("NH, hospitalized female: ")
  skeleton[["hospitalized"]][["sex_male"]] = get_information("NH, hospitalized male: ")
  skeleton[["hospitalized"]][["sex_unk"]] = get_information("NH, hospitalized sex unk: ")
  
  skeleton[["deaths"]][["sex_female"]] = get_information("NH, deaths female: ")
  skeleton[["deaths"]][["sex_male"]] = get_information("NH, deaths male: ")
  skeleton[["deaths"]][["sex_unk"]] = get_information("NH, deaths sex unk: ")
  
  skeleton[["cases"]][["age_0_9"]] = get_information("NH, Cases ages 0-9: ")
  skeleton[["cases"]][["age_10_19"]] = get_information("NH, Cases ages 10-19: ")
  skeleton[["cases"]][["age_20_29"]] = get_information("NH, Cases ages 20-29: ")
  skeleton[["cases"]][["age_30_39"]] = get_information("NH, Cases ages 30-39: ")
  skeleton[["cases"]][["age_40_49"]] = get_information("NH, Cases ages 40-49: ")
  skeleton[["cases"]][["age_50_59"]] = get_information("NH, Cases ages 50-59: ")
  skeleton[["cases"]][["age_60_69"]] = get_information("NH, Cases ages 60-69: ")
  skeleton[["cases"]][["age_70_79"]] = get_information("NH, Cases ages 70-79: ")
  skeleton[["cases"]][["age_80+"]] = get_information("NH, Cases ages 80+: ")
  skeleton[["cases"]][["age_unk"]] = get_information("NH, Cases ages unk: ")
  
  skeleton[["hospitalized"]][["age_0_9"]] = get_information("NH, hospitalized ages 0-9: ")
  skeleton[["hospitalized"]][["age_10_19"]] = get_information("NH, hospitalized ages 10-19: ")
  skeleton[["hospitalized"]][["age_20_29"]] = get_information("NH, hospitalized ages 20-29: ")
  skeleton[["hospitalized"]][["age_30_39"]] = get_information("NH, hospitalized ages 30-39: ")
  skeleton[["hospitalized"]][["age_40_49"]] = get_information("NH, hospitalized ages 40-49: ")
  skeleton[["hospitalized"]][["age_50_59"]] = get_information("NH, hospitalized ages 50-59: ")
  skeleton[["hospitalized"]][["age_60_69"]] = get_information("NH, hospitalized ages 60-69: ")
  skeleton[["hospitalized"]][["age_70_79"]] = get_information("NH, hospitalized ages 70-79: ")
  skeleton[["hospitalized"]][["age_80+"]] = get_information("NH, hospitalized ages 80+: ")
  skeleton[["hospitalized"]][["age_unk"]] = get_information("NH, hospitalized ages unk: ")

  skeleton[["deaths"]][["age_0_9"]] = get_information("NH, deaths ages 0-9: ")
  skeleton[["deaths"]][["age_10_19"]] = get_information("NH, deaths ages 10-19: ")
  skeleton[["deaths"]][["age_20_29"]] = get_information("NH, deaths ages 20-29: ")
  skeleton[["deaths"]][["age_30_39"]] = get_information("NH, deaths ages 30-39: ")
  skeleton[["deaths"]][["age_40_49"]] = get_information("NH, deaths ages 40-49: ")
  skeleton[["deaths"]][["age_50_59"]] = get_information("NH, deaths ages 50-59: ")
  skeleton[["deaths"]][["age_60_69"]] = get_information("NH, deaths ages 60-69: ")
  skeleton[["deaths"]][["age_70_79"]] = get_information("NH, deaths ages 70-79: ")
  skeleton[["deaths"]][["age_80+"]] = get_information("NH, deaths ages 80+: ")
  skeleton[["deaths"]][["age_unk"]] = get_information("NH, deaths ages unk: ")
  
  skeleton[["cases"]][["race_white"]] = get_information("NH, cases race white: ")
  skeleton[["cases"]][["race_hispanic"]] = get_information("NH, cases race hispanic: ")
  skeleton[["cases"]][["race_AfrA"]] = get_information("NH, cases race AfrA: ")
  skeleton[["cases"]][["race_other"]] = get_information("NH, cases race other: ")
  skeleton[["cases"]][["race_asian"]] = get_information("NH, cases race asian: ")
  
  skeleton[["hospitalized"]][["race_white"]] = get_information("NH, hospitalized race white: ")
  skeleton[["hospitalized"]][["race_hispanic"]] = get_information("NH, hospitalized race hispanic: ")
  skeleton[["hospitalized"]][["race_AfrA"]] = get_information("NH, hospitalized race AfrA: ")
  skeleton[["hospitalized"]][["race_other"]] = get_information("NH, hospitalized race other: ")
  skeleton[["hospitalized"]][["race_asian"]] = get_information("NH, hospitalized race asian: ")
  
  skeleton[["deaths"]][["race_white"]] = get_information("NH, deaths race white: ")
  skeleton[["deaths"]][["race_hispanic"]] = get_information("NH, deaths race hispanic: ")
  skeleton[["deaths"]][["race_AfrA"]] = get_information("NH, deaths race AfrA: ")
  skeleton[["deaths"]][["race_other"]] = get_information("NH, deaths race other: ")
  skeleton[["deaths"]][["race_asian"]] = get_information("NH, deaths race asian: ")
  
  race_case_known = get_information("NH, cases race known: ")
  race_hosp_known = get_information("NH, hospitalized race known: ")
  race_death_known = get_information("NH, death race known: ")
  
  skeleton[["cases"]][["race_unk"]] = skeleton[["cases"]][["total"]] - race_case_known
  skeleton[["hospitalized"]][["race_unk"]] = skeleton[["hospitalized"]][["total"]] - race_hosp_known
  skeleton[["deaths"]][["race_unk"]] = skeleton[["deaths"]][["total"]] - race_death_known
  
  as_tibble(skeleton) %>% 
    standardize %>% 
    mutate(
      state_name = "New Hampshire",
      Link = "https://www.nh.gov/covid19/dashboard/summary.htm",
      platform = "pdf",
      comments = "Race/ethnicity combined",
      last.update = Sys.time() %>% as_date) %>% 
    select(
      state_name, Link,
      total.tested:hosp_gender,
      platform, comments, last.update)
}

get_guam = function() {
  url = "https://ghs.guam.gov/news"
  
  # Gather all of the blog posts on their sites
  data = read_html(url) %>% 
    html_nodes("#nakainer #primary-content .container .main_content") %>% 
    html_nodes(".row .col-md-12 .left-side-content-area .region") %>% 
    html_nodes("#block-system-main .content .view .view-content") %>% 
    html_nodes("#singlepage .main .widget .row .col-md-12 .post-outer") %>% 
    html_nodes("article .col-lg-12 .post-title span a") %>% 
    html_attr("href")
  
  # Look for the most recent set of results on cases
  most_recent_link = data[str_detect(data, "end-day-results")] %>% .[1]
  page_url = paste0("https://ghs.guam.gov/", most_recent_link)
  
  data_page = read_html(page_url) %>% 
    html_nodes("#nakainer #primary-content .container .main_content") %>% 
    html_nodes(".row .col-md-12 .left-side-content-area .region") %>% 
    html_nodes(".view .view-content .views-row .post-outer article") %>% 
    html_nodes(".text-left .post-body div table") %>% 
    html_table() %>% .[[1]] %>% tail(1) %>% .[-1] %>% unlist() %>% 
    str_split(":", simplify = TRUE) %>% .[,2] %>% c() %>% 
    str_replace_all("\n", "") %>% str_replace_all("\t", "") %>% 
    str_replace(",", "") %>% str_trim() %>% as.numeric
  
  negatives = data_page[2]
  cases = data_page[3]
  
  skeleton = skeleton_table(default_cols)
  
  skeleton[["tested"]][["total"]] = negatives + cases
  skeleton[["negatives"]][["total"]] = negatives
  skeleton[["cases"]][["total"]] = cases
  
  as_tibble(skeleton) %>% 
    standardize %>% 
    mutate(
      state_name = "Guam",
      Link = url,
      platform = "pdf",
      comments = "",
      last.update = Sys.time() %>% as_date) %>% 
    select(
      state_name, Link,
      total.tested:hosp_gender,
      platform, comments, last.update)
}

get_minnesota = function() {
  
  query = "https://services2.arcgis.com/V12PKGiMAH7dktkU/ArcGIS/rest/services/MyMapService/FeatureServer/0/query?where=ObjectID+%3C+10000&objectIds=&time=&resultType=none&outFields=*&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&sqlFormat=none&f=pjson&token="
  df = fromJSON(query)$features %>% .[,1]

  date = df$Date %>% strsplit(" ") %>% unlist
  date = date[2] %>% as.Date("%m/%d/%Y") %>% format("%m/%d/%Y")
  
  gender = df %>% 
    select("Male", "Female", "SexMsng", "GenderOther")
  race = df[,grep("Race", colnames(df))] %>% 
    select(RaceWht:RaceMultip)
  
  drace = df %>% 
    select(DeathWht, DeathBlk, DeathAsian, DeathPacif, 
           DeathNativ, DeathOther, DeathUnkno, DeathRaceM)
  eth = df %>% select(EthnHisp, EthnNonHis, EthnUnk)
  deth = df %>% select(DeathHisp, DeathNonHi, DeathHispU)
  age = df %>% 
    select(Age05, Age619, Age2029, Age3039,
           Age4049, Age5059, Age6069, Age70_79,
           Age80_89, Age90_99, Age100_up, AgeMissing)
  
  url = "https://www.health.state.mn.us/diseases/coronavirus/situation.html"
  
  total_tests = read_html(url) %>% 
    html_nodes("body #container #content .site-wrap #body #accordion p") %>%
    html_text() %>% .[4] %>%
    str_split(" ", simplify = TRUE) %>% .[1, 6] %>%
    str_split(":", simplify = TRUE) %>% .[1, 2] %>% 
    str_trim %>% str_replace_all(",", "") %>% as.numeric

  death_age_table = read_html(url) %>% 
    html_nodes("body #container #content .site-wrap #body #accordion .panel") %>% 
    html_nodes("#ageg .panel-body #agetable") %>% 
    html_table() %>% .[[1]] %>% 
    dplyr::select(`Age Group`, `Number of Deaths`)
  colnames(death_age_table) = c("X", "death_age")
  
  pdf_url = read_html(url) %>% 
    html_nodes("body #container #content .site-wrap #body #accordion .well ul") %>% 
    html_nodes("li a") %>% .[[1]] %>% 
    html_attr("href")
  
  pdf_data = pdf_text(paste0("https://www.health.state.mn.us/", pdf_url))
  
  browseURL(paste0("https://www.health.state.mn.us/", pdf_url))
  
  # CHECK THE INDICES AND MAKE SURE THEY WORK OUT
  death_by_gender = pdf_data %>% .[9] %>% 
    str_split("\n") %>% .[[1]] %>% 
    str_squish %>% .[33:36] %>% 
    str_split(" ", simplify = TRUE) %>% .[,1] %>% 
    str_replace(",", "") %>% as.numeric
  
  hosp_by_age = pdf_data %>% .[8] %>% 
    str_split("\n") %>% .[[1]] %>% 
    str_squish %>% .[80:91] %>% 
    str_split(" ", simplify = TRUE) %>% .[,1] %>% as.numeric
  
  hosp_by_gender = pdf_data %>% .[9] %>% 
    str_split("\n") %>% .[[1]] %>% 
    str_squish %>% .[29:31] %>%
    str_split(" ", simplify = TRUE) %>% .[,1] %>%
    str_replace(",", "") %>% as.numeric
  
  hosp_by_race = pdf_data %>% .[10] %>% 
    str_split("\n") %>% .[[1]] %>% 
    str_squish %>% .[70:80] %>%
    str_split(" ", simplify = TRUE) %>% .[,1] %>%
    str_replace(",", "") %>% as.numeric
  
  View(death_by_gender)
  View(hosp_by_gender)
  View(hosp_by_age)
  View(hosp_by_race)
  
  minnesota = skeleton_table(mn_cols)
  
  minnesota[["cases"]][["total"]] = df %>% pull(TotalCases)
  minnesota[["deaths"]][["total"]] = df %>% pull(OutcmDied)
  minnesota[["hospitalized"]][["total"]] = df$EvrICUYes + df$EvrHospYes
  
  minnesota[["cases"]][["sex_male"]] = gender %>% pull(Male)
  minnesota[["cases"]][["sex_female"]] = gender %>% pull(Female)
  minnesota[["cases"]][["sex_other"]] = gender %>% pull(GenderOther)
  minnesota[["cases"]][["sex_unk"]] = gender %>% pull(SexMsng)
  
  minnesota[["cases"]][["race_white"]] = race %>% pull(RaceWht)
  minnesota[["cases"]][["race_AfrA"]] = race %>% pull(RaceBlk)
  minnesota[["cases"]][["race_NatA"]] = race %>% pull(RaceAmerIn)
  minnesota[["cases"]][["race_other"]] = race %>% pull(RaceOther)
  minnesota[["cases"]][["race_unk"]] = race %>% pull(RaceUnk)
  minnesota[["cases"]][["race_asian"]] = race %>% pull(RaceAsian)
  minnesota[["cases"]][["race_pac"]] = race %>% pull(RacePacifi)
  minnesota[["cases"]][["race_multi"]] = race %>% pull(RaceMultip)
  
  minnesota[["cases"]][["ethnicity_hispanic"]] = eth %>% pull(EthnHisp)
  minnesota[["cases"]][["ethnicity_non_hispanic"]] = eth %>% pull(EthnNonHis)
  minnesota[["cases"]][["ethnicity_unk"]] = eth %>% pull(EthnUnk)
  
  minnesota[["cases"]][["age_0_5"]] = age %>% pull(Age05)
  minnesota[["cases"]][["age_6_19"]] = age %>% pull(Age619)
  minnesota[["cases"]][["age_20_29"]] = age %>% pull(Age2029)
  minnesota[["cases"]][["age_30_39"]] = age %>% pull(Age3039)
  minnesota[["cases"]][["age_40_49"]] = age %>% pull(Age4049)
  minnesota[["cases"]][["age_50_59"]] = age %>% pull(Age5059)
  minnesota[["cases"]][["age_60_69"]] = age %>% pull(Age6069)
  minnesota[["cases"]][["age_70_79"]] = age %>% pull(Age70_79)
  minnesota[["cases"]][["age_80_89"]] = age %>% pull(Age80_89)
  minnesota[["cases"]][["age_90_99"]] = age %>% pull(Age90_99)
  minnesota[["cases"]][["age_100+"]] = age %>% pull(Age100_up)
  minnesota[["cases"]][["age_unk"]] = age %>% pull(AgeMissing)
  
  minnesota[["deaths"]][["race_white"]] = drace %>% pull(DeathWht)
  minnesota[["deaths"]][["race_AfrA"]] = drace %>% pull(DeathBlk)
  minnesota[["deaths"]][["race_NatA"]] = drace %>% pull(DeathNativ)
  minnesota[["deaths"]][["race_other"]] = drace %>% pull(DeathOther)
  minnesota[["deaths"]][["race_unk"]] = drace %>% pull(DeathUnkno)
  minnesota[["deaths"]][["race_asian"]] = drace %>% pull(DeathAsian)
  minnesota[["deaths"]][["race_pac"]] = drace %>% pull(DeathPacif)
  minnesota[["deaths"]][["race_multi"]] = drace %>% pull(DeathRaceM)
  
  minnesota[["deaths"]][["ethnicity_hispanic"]] = deth %>% pull(DeathHisp)
  minnesota[["deaths"]][["ethnicity_non_hispanic"]] = deth %>% pull(DeathNonHi)
  minnesota[["deaths"]][["ethnicity_unk"]] = deth %>% pull(DeathHispU)
  
  minnesota[["deaths"]][["age_0_5"]] = death_age_table$death_age[1]
  minnesota[["deaths"]][["age_6_19"]] = death_age_table$death_age[2]
  minnesota[["deaths"]][["age_20_29"]] = death_age_table$death_age[3]
  minnesota[["deaths"]][["age_30_39"]] = death_age_table$death_age[4]
  minnesota[["deaths"]][["age_40_49"]] = death_age_table$death_age[5]
  minnesota[["deaths"]][["age_50_59"]] = death_age_table$death_age[6]
  minnesota[["deaths"]][["age_60_69"]] = death_age_table$death_age[7]
  minnesota[["deaths"]][["age_70_79"]] = death_age_table$death_age[8]
  minnesota[["deaths"]][["age_80_89"]] = death_age_table$death_age[9]
  minnesota[["deaths"]][["age_90_99"]] = death_age_table$death_age[10]
  minnesota[["deaths"]][["age_100+"]] = death_age_table$death_age[11]
  minnesota[["deaths"]][["age_unk"]] = death_age_table$death_age[12]
  
  minnesota[["deaths"]][["sex_male"]] = death_by_gender[1]
  minnesota[["deaths"]][["sex_female"]] = death_by_gender[2]
  minnesota[["deaths"]][["sex_unk"]] = death_by_gender[4]
  
  minnesota[["hospitalized"]][["age_0_5"]] = hosp_by_age[1]
  minnesota[["hospitalized"]][["age_6_19"]] = hosp_by_age[2]
  minnesota[["hospitalized"]][["age_20_29"]] = hosp_by_age[3]
  minnesota[["hospitalized"]][["age_30_39"]] = hosp_by_age[4]
  minnesota[["hospitalized"]][["age_40_49"]] = hosp_by_age[5]
  minnesota[["hospitalized"]][["age_50_59"]] = hosp_by_age[6]
  minnesota[["hospitalized"]][["age_60_69"]] = hosp_by_age[7]
  minnesota[["hospitalized"]][["age_70_79"]] = hosp_by_age[8]
  minnesota[["hospitalized"]][["age_80_89"]] = hosp_by_age[9]
  minnesota[["hospitalized"]][["age_90_99"]] = hosp_by_age[10]
  minnesota[["hospitalized"]][["age_100+"]] = hosp_by_age[11]
  minnesota[["hospitalized"]][["age_unk"]] = hosp_by_age[12]
  
  minnesota[["hospitalized"]][["sex_male"]] = hosp_by_gender[1]
  minnesota[["hospitalized"]][["sex_female"]] = hosp_by_gender[2]
  minnesota[["hospitalized"]][["sex_unk"]] = hosp_by_gender[4]
  
  minnesota[["hospitalized"]][["race_white"]] = hosp_by_race[1]
  minnesota[["hospitalized"]][["race_AfrA"]] = hosp_by_race[2]
  minnesota[["hospitalized"]][["race_asian"]] = hosp_by_race[3]
  minnesota[["hospitalized"]][["race_NatA"]] = hosp_by_race[4]
  minnesota[["hospitalized"]][["race_pac"]] = hosp_by_race[5]
  minnesota[["hospitalized"]][["race_multi"]] = hosp_by_race[6]
  minnesota[["hospitalized"]][["race_other"]] = hosp_by_race[7]
  minnesota[["hospitalized"]][["race_unk"]] = hosp_by_race[8]
  
  minnesota[["hospitalized"]][["ethnicity_hispanic"]] = hosp_by_race[9]
  minnesota[["hospitalized"]][["ethnicity_non_hispanic"]] = hosp_by_race[10]
  minnesota[["hospitalized"]][["ethnicity_unk"]] = hosp_by_race[11]
  
  minnesota[["tested"]][["total"]] = unlist(total_tests)
  
  full_skeleton = as_tibble(minnesota) %>% 
    standardize %>% 
    mutate(
      state_name = "Minnesota",
      Link = "https://www.health.state.mn.us/diseases/coronavirus/situation.html",
      platform = "manual",
      comments = "",
      last.update = as_date(date)) %>% 
    select(
      state_name, Link,
      total.tested:hosp_gender,
      platform, comments, last.update)
  
  return(full_skeleton)
  
}

get_arizona = function() {
  browseURL("https://www.azdhs.gov/preparedness/epidemiology-disease-control/infectious-disease-epidemiology/covid-19/dashboards/index.php")
  
  skeleton = skeleton_table(az_cols)
  
  skeleton[["cases"]][["total"]] = get_information("AZ: Total cases?: ")
  skeleton[["deaths"]][["total"]] = get_information("AZ: Total deaths?: ")
  skeleton[["tested"]][["total"]] = get_information("AZ: Total tested?: ")
  
  skeleton[["cases"]][["age_0_19"]] = get_information("AZ: Cases age <20?: ")
  skeleton[["cases"]][["age_20_44"]] = get_information("AZ: Cases age 20 - 44?: ")
  skeleton[["cases"]][["age_45_54"]] = get_information("AZ: Cases age 45 - 54?: ")
  skeleton[["cases"]][["age_55_64"]] = get_information("AZ: Cases age 55 - 64?: ")
  skeleton[["cases"]][["age_65+"]] = get_information("AZ: Cases age 65+?: ")
  skeleton[["cases"]][["age_unk"]] = get_information("AZ: Cases age unknown: ")
  skeleton[["cases"]][["sex_male"]] = get_information("AZ: Cases sex male: ")
  skeleton[["cases"]][["sex_female"]] = get_information("AZ: Cases sex female: ")
  skeleton[["cases"]][["race_white"]] = get_information("AZ: Cases white?: ")
  skeleton[["cases"]][["race_hispanic"]] = get_information("AZ: Cases hispanic?: ")
  skeleton[["cases"]][["race_NatA"]] = get_information("AZ: Cases NatA?: ")
  skeleton[["cases"]][["race_AfrA"]] = get_information("AZ: Cases AfrA?: ")
  skeleton[["cases"]][["race_asian"]] = get_information("AZ: Cases asian?: ")
  skeleton[["cases"]][["race_other"]] = get_information("AZ: Cases race other?: ")
  skeleton[["cases"]][["race_unk"]] = get_information("AZ: Cases race unknown?: ")
  
  skeleton[["deaths"]][["age_20_44"]] = get_information("AZ: Deaths age 20 - 44?: ")
  skeleton[["deaths"]][["age_45_54"]] = get_information("AZ: Deaths age 45 - 54?: ")
  skeleton[["deaths"]][["age_55_64"]] = get_information("AZ: Deaths age 55 - 64?: ")
  skeleton[["deaths"]][["age_65+"]] = get_information("AZ: Deaths age 65+?: ")
  skeleton[["deaths"]][["age_0_19"]] = get_information("AZ: Deaths age <20?: ")
  skeleton[["deaths"]][["sex_male"]] = get_information("AZ: Deaths sex male: ")
  skeleton[["deaths"]][["sex_female"]] = get_information("AZ: Deaths sex female: ")
  skeleton[["deaths"]][["race_white"]] = get_information("AZ: Deaths white?: ")
  skeleton[["deaths"]][["race_hispanic"]] = get_information("AZ: Deaths hispanic?: ")
  skeleton[["deaths"]][["race_NatA"]] = get_information("AZ: Deaths NatA?: ")
  skeleton[["deaths"]][["race_AfrA"]] = get_information("AZ: Deaths AfrA?: ")
  skeleton[["deaths"]][["race_asian"]] = get_information("AZ: Deaths asian?: ")
  skeleton[["deaths"]][["race_other"]] = get_information("AZ: Deaths race other?: ")
  skeleton[["deaths"]][["race_unk"]] = get_information("AZ: Deaths race unknown?: ")
  
  skeleton[["hospitalized"]][["total"]] = get_information("AZ: Total hospitalized?: ")
  skeleton[["hospitalized"]][["age_0_19"]] = get_information("AZ: Hospitalized age <20?: ")
  skeleton[["hospitalized"]][["age_20_44"]] = get_information("AZ: Hospitalized age 20 - 44?: ")
  skeleton[["hospitalized"]][["age_45_54"]] = get_information("AZ: Hospitalized age 45 - 54?: ")
  skeleton[["hospitalized"]][["age_55_64"]] = get_information("AZ: Hospitalized age 55 - 64?: ")
  skeleton[["hospitalized"]][["age_65+"]] = get_information("AZ: Hospitalized age 65+?: ")
  skeleton[["hospitalized"]][["age_unk"]] = get_information("AZ: Hospitalized age unknown: ")
  skeleton[["hospitalized"]][["sex_male"]] = get_information("AZ: Hospitalized sex male: ")
  skeleton[["hospitalized"]][["sex_female"]] = get_information("AZ: Hospitalized sex female: ")
  skeleton[["hospitalized"]][["race_white"]] = get_information("AZ: Hospitalized white?: ")
  skeleton[["hospitalized"]][["race_hispanic"]] = get_information("AZ: Hospitalized hispanic?: ")
  skeleton[["hospitalized"]][["race_NatA"]] = get_information("AZ: Hospitalized NatA?: ")
  skeleton[["hospitalized"]][["race_AfrA"]] = get_information("AZ: Hospitalized AfrA?: ")
  skeleton[["hospitalized"]][["race_asian"]] = get_information("AZ: Hospitalized asian?: ")
  skeleton[["hospitalized"]][["race_other"]] = get_information("AZ: Hospitalized race other?: ")
  skeleton[["hospitalized"]][["race_unk"]] = get_information("AZ: Hospitalized race unknown?: ")
  
  full_skeleton = as_tibble(skeleton) %>% 
    standardize %>% 
    mutate(
      state_name = "Arizona",
      Link = "https://www.azdhs.gov/preparedness/epidemiology-disease-control/infectious-disease-epidemiology/covid-19/dashboards/index.php",
      platform = "manual",
      comments = "",
      last.update = Sys.time() %>% as_date) %>% 
    select(
      state_name, Link,
      total.tested:hosp_gender,
      platform, comments, last.update)
  
  return(full_skeleton)
}

get_hawaii = function() {
  browseURL("https://health.hawaii.gov/coronavirusdisease2019/what-you-should-know/current-situation-in-hawaii/")
  
  skeleton = skeleton_table(hi_cols)
  
  skeleton[["cases"]][["total"]] = get_information("HI: Total cases?: ")
  skeleton[["hospitalized"]][["total"]] = get_information("HI: Total hospitalized?: ")
  skeleton[["deaths"]][["total"]] = get_information("HI: Total deaths?: ")
  
  skeleton[["cases"]][["age_0_19"]] = get_information("HI: Cases age 0 - 19?: ")
  skeleton[["cases"]][["age_20_39"]] = get_information("HI: Cases age 20 - 39?: ")
  skeleton[["cases"]][["age_40_59"]] = get_information("HI: Cases age 40 - 59?: ")
  skeleton[["cases"]][["age_60+"]] = get_information("HI: Cases age 60+?: ")
  
  skeleton[["cases"]][["race_asian"]] = get_information("HI: Cases race asian %?: ")
  skeleton[["cases"]][["race_white"]] = get_information("HI: Cases race white %?: ")
  skeleton[["cases"]][["race_pac"]] = get_information("HI: Cases race Native Hawaiian %?: ")
  skeleton[["cases"]][["race_AfrA"]] = get_information("HI: Cases race AfrA %?: ")
  skeleton[["cases"]][["race_other"]] = get_information("HI: Cases race other %?: ")
  skeleton[["cases"]][["race_multi"]] = get_information("HI: Cases race multirace %?: ")
  
  full_skeleton = as_tibble(skeleton) %>% 
    standardize %>% 
    mutate(
      state_name = "Hawaii",
      Link = "https://health.hawaii.gov/coronavirusdisease2019/what-you-should-know/current-situation-in-hawaii/",
      platform = "manual",
      comments = "",
      last.update = Sys.time() %>% as_date) %>% 
    select(
      state_name, Link,
      total.tested:hosp_gender,
      platform, comments, last.update)
  
  return(full_skeleton)
}

get_idaho = function() {
  browseURL("https://public.tableau.com/profile/idaho.division.of.public.health#!/vizhome/DPHIdahoCOVID-19Dashboard_V2/Story1")
  
  skeleton = skeleton_table(id_cols)
  
  skeleton[["cases"]][["total"]] = get_information("ID: Total cases?: ")
  
  skeleton[["cases"]][["age_0_17"]] = get_information("ID: Cases age <18: ")
  skeleton[["cases"]][["age_18_29"]] = get_information("ID: Cases age 18 - 29: ")
  skeleton[["cases"]][["age_30_39"]] = get_information("ID: Cases age 30 - 39?: ")
  skeleton[["cases"]][["age_40_49"]] = get_information("ID: Cases age 40 - 49?: ")
  skeleton[["cases"]][["age_50_59"]] = get_information("ID: Cases age 50 - 59?: ")
  skeleton[["cases"]][["age_60_69"]] = get_information("ID: Cases age 60 - 69?: ")
  skeleton[["cases"]][["age_70_79"]] = get_information("ID: Cases age 70 - 79?: ")
  age_80_89 = get_information("ID: Cases age 80 - 89?: ")
  age_90_99 = get_information("ID: Cases age 90 - 99?: ")
  age_100_plus = get_information("ID: Cases age 100+?: ")
  skeleton[["cases"]][["age_80+"]] = age_80_89 + age_90_99 + age_100_plus
  
  skeleton[["cases"]][["sex_female"]] = get_information("ID: Cases sex female?: ")
  skeleton[["cases"]][["sex_male"]] = get_information("ID: Cases sex male?: ")
  
  skeleton[["cases"]][["ethnicity_unk"]] = get_information("ID: Cases ethnicity unknown? (calc #): ")
  
  case_hisp = get_information2("ID: Cases hispanic (whole %)?: ")
  skeleton[["cases"]][["ethnicity_hispanic"]] = ((skeleton[["cases"]][["total"]] - skeleton[["cases"]][["ethnicity_unk"]]) * case_hisp) %>% floor
  
  case_non_hisp = get_information2("ID: Cases not hispanic (whole %)?: ")
  skeleton[["cases"]][["ethnicity_non_hispanic"]] = ((skeleton[["cases"]][["total"]] - skeleton[["cases"]][["ethnicity_unk"]]) * case_non_hisp) %>% floor
  
  skeleton[["cases"]][["race_unk"]] = get_information("ID: Cases race unknown? (calc. #): ")
  
  case_white = get_information2("ID: Cases race white (whole %)?: ")
  skeleton[["cases"]][["race_white"]] = ((skeleton[["cases"]][["total"]] - skeleton[["cases"]][["race_unk"]]) * case_white) %>% floor
    
  case_race_other = get_information2("ID: Cases race other (whole %)?: ")
  skeleton[["cases"]][["race_other"]] = ((skeleton[["cases"]][["total"]] - skeleton[["cases"]][["race_unk"]]) * case_race_other) %>% floor
    
  case_asian = get_information2("ID: Cases asian (whole %)?: ")
  skeleton[["cases"]][["race_asian"]] = ((skeleton[["cases"]][["total"]] - skeleton[["cases"]][["race_unk"]]) * case_asian) %>% floor
  
  case_afra = get_information2("ID: Cases AfrA (whole %)?: ")
  skeleton[["cases"]][["race_AfrA"]] = ((skeleton[["cases"]][["total"]] - skeleton[["cases"]][["race_unk"]]) * case_afra) %>% floor
  
  case_race_multi = get_information2("ID: Cases race_multi (whole %)?: ")
  skeleton[["cases"]][["race_multi"]] = ((skeleton[["cases"]][["total"]] - skeleton[["cases"]][["race_unk"]]) * case_race_multi) %>% floor
  
  case_nata = get_information2("ID: Cases NatA (whole %)?: ")
  skeleton[["cases"]][["race_NatA"]] = ((skeleton[["cases"]][["total"]] - skeleton[["cases"]][["race_unk"]]) * case_nata) %>% floor
    
  case_pac = get_information2("ID: Cases pac (whole %)?: ")
  skeleton[["cases"]][["race_pac"]] = ((skeleton[["cases"]][["total"]] - skeleton[["cases"]][["race_unk"]]) * case_pac) %>% floor
  
  skeleton[["hospitalized"]][["total"]] = get_information("ID: Total hospitalized?: ")
  
  skeleton[["deaths"]][["total"]] = get_information("ID: Total deaths?: ")
  skeleton[["deaths"]][["age_0_17"]] = get_information("ID: Deaths age <18: ")
  skeleton[["deaths"]][["age_18_29"]] = get_information("ID: Deaths age 18 - 29: ")
  skeleton[["deaths"]][["age_30_39"]] = get_information("ID: Deaths age 30 - 39?: ")
  skeleton[["deaths"]][["age_40_49"]] = get_information("ID: Deaths age 40 - 49?: ")
  skeleton[["deaths"]][["age_50_59"]] = get_information("ID: Deaths age 50 - 59?: ")
  skeleton[["deaths"]][["age_60_69"]] = get_information("ID: Deaths age 60 - 69?: ")
  skeleton[["deaths"]][["age_70_79"]] = get_information("ID: Deaths age 70 - 79?: ")
  skeleton[["deaths"]][["age_80+"]] = get_information("ID: Deaths age 80+?: ")
  skeleton[["deaths"]][["sex_male"]] = get_information("ID: Deaths sex male?: ")
  skeleton[["deaths"]][["sex_female"]] = get_information("ID: Deaths sex female?: ")
  skeleton[["deaths"]][["ethnicity_hispanic"]] = get_information("ID: Deaths hispanic?: ")
  skeleton[["deaths"]][["ethnicity_non_hispanic"]] = get_information("ID: Deaths not hispanic?: ")
  
  skeleton[["deaths"]][["race_white"]] = get_information("ID: Deaths race white?: ")
  skeleton[["deaths"]][["race_NatA"]] = get_information("ID: Deaths race NatA?: ")
  skeleton[["deaths"]][["race_AfrA"]] = get_information("ID: Deaths race AfrA?: ")
  skeleton[["deaths"]][["race_other"]] = get_information("ID: Deaths race other?: ")
  skeleton[["deaths"]][["race_asian"]] = get_information("ID: Deaths race asian?: ")
  

  skeleton[["tested"]][["total"]] = get_information("ID: Total tested?: ")
  
  full_skeleton = as_tibble(skeleton) %>%
    standardize %>% 
    mutate(
      state_name = "Idaho",
      Link = "https://public.tableau.com/profile/idaho.division.of.public.health#!/vizhome/DPHIdahoCOVID-19Dashboard_V2/Story1",
      platform = "manual",
      comments = "Percentages present. Manual entry.",
      last.update = Sys.time() %>% as_date) %>% 
    select(
      state_name, Link,
      total.tested:hosp_gender,
      platform, comments, last.update)
  
  return(full_skeleton)
}

get_iowa = function() {
  browseURL("https://coronavirus.iowa.gov/pages/case-counts")
  
  skeleton = skeleton_table(ia_cols)
  
  skeleton[["tested"]][["total"]] = get_information("IA: Total tested?: ")
  skeleton[["cases"]][["total"]] = get_information("IA: Total cases?: ")
  
  skeleton[["cases"]][["age_0_17"]] = get_information2("IA: Cases age 0-17 (whole %)?: ")
  skeleton[["cases"]][["age_18_40"]] = get_information2("IA: Cases age 18-40 (whole %)?: ")
  skeleton[["cases"]][["age_41_60"]] = get_information2("IA: Cases age 41-60 (whole %)?: ")
  skeleton[["cases"]][["age_61_80"]] = get_information2("IA: Cases age 61-80 (whole %)?: ")
  skeleton[["cases"]][["age_80+"]] = get_information2("IA: Cases age 80+ (whole %)?: ")
  
  skeleton[["cases"]][["sex_female"]] = get_information2("IA: Cases sex female (whole %)?: ")
  skeleton[["cases"]][["sex_male"]] = get_information2("IA: Cases sex male (whole %)?: ")
  skeleton[["cases"]][["sex_unk"]] = get_information2("IA: Cases sex unknown/pending (whole %)?: ")
  
  skeleton[["cases"]][["race_asian"]] = get_information2("IA: Cases race asian (whole %)?: ")
  skeleton[["cases"]][["race_AfrA"]] = get_information2("IA: Cases race AfrA (whole %)?: ")
  skeleton[["cases"]][["race_other"]] = get_information2("IA: Cases race other (whole %)?: ")
  skeleton[["cases"]][["race_white"]] = get_information2("IA: Cases race white (whole %)?: ")
  
  skeleton[["cases"]][["ethnicity_hispanic"]] = get_information2("IA: Cases ethnicity hispanic (whole %)?: ")
  skeleton[["cases"]][["ethnicity_non_hispanic"]] = get_information2("IA: Cases ethnicity not hispanic (whole %)?: ")
  skeleton[["cases"]][["ethnicity_unk"]] = get_information2("IA: Cases ethnicity pending (whole %)?: ")
  
  skeleton[["deaths"]][["total"]] = get_information("IA: Total deaths?: ")
  
  skeleton[["deaths"]][["age_0_17"]] = get_information2("IA: Deaths age 0-17 (whole %, CHECK)?: ")
  skeleton[["deaths"]][["age_18_40"]] = get_information2("IA: Deaths age 18-40 (whole %)?: ")
  skeleton[["deaths"]][["age_41_60"]] = get_information2("IA: Deaths age 41-60 (whole %)?: ")
  skeleton[["deaths"]][["age_61_80"]] = get_information2("IA: Deaths age 61-80 (whole %)?: ")
  skeleton[["deaths"]][["age_80+"]] = get_information2("IA: Deaths age 80+ (whole %)?: ")
  
  skeleton[["deaths"]][["sex_female"]] = get_information2("IA: Deaths sex female (whole %)?: ")
  skeleton[["deaths"]][["sex_male"]] = get_information2("IA: Deaths sex male (whole %)?: ")
  
  skeleton[["deaths"]][["race_asian"]] = get_information2("IA: Deaths race asian (whole %)?: ")
  skeleton[["deaths"]][["race_AfrA"]] = get_information2("IA: Deaths race AfrA (whole %)?: ")
  skeleton[["deaths"]][["race_other"]] = get_information2("IA: Deaths race other (whole %)?: ")
  skeleton[["deaths"]][["race_white"]] = get_information2("IA: Deaths race white (whole %)?: ")
  
  skeleton[["deaths"]][["ethnicity_hispanic"]] = get_information2("IA: Deaths ethnicity hispanic (whole %)?: ")
  skeleton[["deaths"]][["ethnicity_non_hispanic"]] = get_information2("IA: Deaths ethnicity not hispanic (whole %)?: ")
  skeleton[["deaths"]][["ethnicity_unk"]] = get_information2("IA: Deaths ethnicity pending (whole %)?: ")
  
  full_skeleton = as_tibble(skeleton) %>% 
    standardize %>% 
    mutate(
      state_name = "Iowa",
      Link = "https://coronavirus.iowa.gov/pages/case-counts",
      platform = "manual",
      comments = "",
      last.update = Sys.time() %>% as_date) %>% 
    select(
      state_name, Link,
      total.tested:hosp_gender,
      platform, comments, last.update)
  
  return(full_skeleton)
}

get_kansas = function() {
  browseURL("https://www.coronavirus.kdheks.gov/160/COVID-19-in-Kansas")
  
  skeleton = skeleton_table(ks_cols)
  
  skeleton[["cases"]][["total"]] = get_information("KS: Total cases?: ")
  skeleton[["hospitalized"]][["total"]] = get_information("KS: Total hospitalized?: ")
  skeleton[["deaths"]][["total"]] = get_information("KS: Total deaths?: ")
  
  skeleton[["cases"]][["sex_female"]] = get_information("KS: Cases sex female?: ")
  skeleton[["cases"]][["sex_male"]] = get_information("KS: Cases sex male?: ")
  skeleton[["cases"]][["sex_unk"]] = get_information("KS: Cases sex unknown?: ")
  
  skeleton[["cases"]][["age_0_9"]] = get_information("KS: Cases age 0 - 9?: ")
  skeleton[["cases"]][["age_10_17"]] = get_information("KS: Cases age 10 - 17?: ")
  skeleton[["cases"]][["age_18_24"]] = get_information("KS: Cases age 18 - 24?: ")
  skeleton[["cases"]][["age_25_34"]] = get_information("KS: Cases age 25 - 34?: ")
  skeleton[["cases"]][["age_45_54"]] = get_information("KS: Cases age 35 - 44?: ")
  skeleton[["cases"]][["age_35_44"]] = get_information("KS: Cases age 45 - 54?: ")
  skeleton[["cases"]][["age_55_64"]] = get_information("KS: Cases age 55 - 64?: ")
  skeleton[["cases"]][["age_65_74"]] = get_information("KS: Cases age 65 - 74?: ")
  skeleton[["cases"]][["age_75_84"]] = get_information("KS: Cases age 75 - 84?: ")
  skeleton[["cases"]][["age_85+"]] = get_information("KS: Cases age 85+?: ")
  skeleton[["cases"]][["age_unk"]] = get_information("KS: Cases age not reported?: ")
  
  skeleton[["cases"]][["race_white"]] = get_information("KS: Cases race white?: ")
  skeleton[["cases"]][["race_AfrA"]] = get_information("KS: Cases race AfrA?: ")
  skeleton[["cases"]][["race_NatA"]] = get_information("KS: Cases race NatA?: ")
  skeleton[["cases"]][["race_asian"]] = get_information("KS: Cases race asian?: ")
  skeleton[["cases"]][["race_other"]] = get_information("KS: Cases race other?: ")
  skeleton[["cases"]][["race_unk"]] = get_information("KS: Cases race not reported/missing?: ")
  
  skeleton[["cases"]][["ethnicity_hispanic"]] = get_information("KS: Cases ethnicity hispanic?: ")
  skeleton[["cases"]][["ethnicity_non_hispanic"]] = get_information("KS: Cases ethnicity not hispanic?: ")
  skeleton[["cases"]][["ethnicity_unk"]] = get_information("KS: Cases ethnicity unknown/missing?: ")
  
  skeleton[["deaths"]][["age_0_9"]] = get_information("KS: Deaths age 0 - 9?: ")
  skeleton[["deaths"]][["age_10_17"]] = get_information("KS: Deaths age 10 - 17?: ")
  skeleton[["deaths"]][["age_18_24"]] = get_information("KS: Deaths age 18 - 24?: ")
  skeleton[["deaths"]][["age_25_34"]] = get_information("KS: Deaths age 25 - 34?: ")
  skeleton[["deaths"]][["age_45_54"]] = get_information("KS: Deaths age 35 - 44?: ")
  skeleton[["deaths"]][["age_35_44"]] = get_information("KS: Deaths age 45 - 54?: ")
  skeleton[["deaths"]][["age_55_64"]] = get_information("KS: Deaths age 55 - 64?: ")
  skeleton[["deaths"]][["age_65_74"]] = get_information("KS: Deaths age 65 - 74?: ")
  skeleton[["deaths"]][["age_75_84"]] = get_information("KS: Deaths age 75 - 84?: ")
  skeleton[["deaths"]][["age_85+"]] = get_information("KS: Deaths age 85+?: ")
  
  skeleton[["deaths"]][["sex_female"]] = get_information("KS: Deaths sex female?: ")
  skeleton[["deaths"]][["sex_male"]] = get_information("KS: Deaths sex male?: ")
  
  skeleton[["deaths"]][["race_white"]] = get_information("KS: Deaths race white?: ")
  skeleton[["deaths"]][["race_AfrA"]] = get_information("KS: Deaths race AfrA?: ")
  skeleton[["deaths"]][["race_NatA"]] = get_information("KS: Deaths race NatA?: ")
  skeleton[["deaths"]][["race_asian"]] = get_information("KS: Deaths race asian?: ")
  skeleton[["deaths"]][["race_other"]] = get_information("KS: Deaths race other?: ")
  skeleton[["deaths"]][["race_unk"]] = get_information("KS: Deaths race not reported/missing?: ")
  
  skeleton[["deaths"]][["ethnicity_hispanic"]] = get_information("KS: Deaths ethnicity hispanic?: ")
  skeleton[["deaths"]][["ethnicity_non_hispanic"]] = get_information("KS: Deaths ethnicity not hispanic?: ")
  skeleton[["deaths"]][["ethnicity_unk"]] = get_information("KS: Deaths ethnicity unknown/missing?: ")
  
  skeleton[["hospitalized"]][["age_0_9"]] = get_information("KS: Hospitalized age 0 - 9?: ")
  skeleton[["hospitalized"]][["age_10_17"]] = get_information("KS: Hospitalized age 10 - 17? (CHECK): ")
  skeleton[["hospitalized"]][["age_18_24"]] = get_information("KS: Hospitalized age 18 - 24?: ")
  skeleton[["hospitalized"]][["age_25_34"]] = get_information("KS: Hospitalized age 25 - 34?: ")
  skeleton[["hospitalized"]][["age_45_54"]] = get_information("KS: Hospitalized age 35 - 44?: ")
  skeleton[["hospitalized"]][["age_35_44"]] = get_information("KS: Hospitalized age 45 - 54?: ")
  skeleton[["hospitalized"]][["age_55_64"]] = get_information("KS: Hospitalized age 55 - 64?: ")
  skeleton[["hospitalized"]][["age_65_74"]] = get_information("KS: Hospitalized age 65 - 74?: ")
  skeleton[["hospitalized"]][["age_75_84"]] = get_information("KS: Hospitalized age 75 - 84?: ")
  skeleton[["hospitalized"]][["age_85+"]] = get_information("KS: Hospitalized age 85+?: ")
  skeleton[["hospitalized"]][["age_unk"]] = get_information("KS: Hospitalized age not reported?: ")
  
  skeleton[["hospitalized"]][["race_white"]] = get_information("KS: Hospitalized race white?: ")
  skeleton[["hospitalized"]][["race_AfrA"]] = get_information("KS: Hospitalized race AfrA?: ")
  skeleton[["hospitalized"]][["race_NatA"]] = get_information("KS: Hospitalized race NatA?: ")
  skeleton[["hospitalized"]][["race_asian"]] = get_information("KS: Hospitalized race asian?: ")
  skeleton[["hospitalized"]][["race_other"]] = get_information("KS: Hospitalized race other?: ")
  skeleton[["hospitalized"]][["race_unk"]] = get_information("KS: Hospitalized race not reported/missing?: ")
  
  skeleton[["hospitalized"]][["ethnicity_hispanic"]] = get_information("KS: Hospitalized ethnicity hispanic?: ")
  skeleton[["hospitalized"]][["ethnicity_non_hispanic"]] = get_information("KS: Hospitalized ethnicity not hispanic?: ")
  skeleton[["hospitalized"]][["ethnicity_unk"]] = get_information("KS: Hospitalized ethnicity unknown/missing?: ")
  
  skeleton[["tested"]][["total"]] = get_information("KS: Total tested?: ")
  
  full_skeleton = as_tibble(skeleton) %>% 
    standardize %>% 
    mutate(
      state_name = "Kansas",
      Link = "https://www.coronavirus.kdheks.gov/160/COVID-19-in-Kansas",
      platform = "manual",
      comments = "",
      last.update = Sys.time() %>% as_date) %>% 
    select(
      state_name, Link,
      total.tested:hosp_gender,
      platform, comments, last.update)
  
  return(full_skeleton)
}

get_nevada = function() {
  browseURL("https://nvhealthresponse.nv.gov/")
  
  skeleton = skeleton_table(nv_cols)
  
  skeleton[["tested"]][["total"]] = get_information("NV: Total tested?: ")
  skeleton[["cases"]][["total"]] = get_information("NV: Total cases?: ")
  skeleton[["deaths"]][["total"]] = get_information("NV: Total deaths?: ")
  
  skeleton[["cases"]][["age_0_9"]] = get_information("NV: Cases ages < 10?: ")
  skeleton[["cases"]][["age_10_19"]] = get_information("NV: Cases ages 10-19?: ")
  skeleton[["cases"]][["age_20_29"]] = get_information("NV: Cases ages 20-29?: ")
  skeleton[["cases"]][["age_30_39"]] = get_information("NV: Cases ages 30-39?: ")
  skeleton[["cases"]][["age_40_49"]] = get_information("NV: Cases ages 40-49?: ")
  skeleton[["cases"]][["age_50_59"]] = get_information("NV: Cases ages 50-59?: ")
  skeleton[["cases"]][["age_60_69"]] = get_information("NV: Cases ages 60-69?: ")
  skeleton[["cases"]][["age_70+"]] = get_information("NV: Cases ages 70+?: ")
  skeleton[["cases"]][["age_unk"]] = get_information("NV: Cases ages not reported?: ")
  skeleton[["cases"]][["sex_male"]] = get_information("NV: Cases sex male?: ")
  skeleton[["cases"]][["sex_female"]] = get_information("NV: Cases sex female?: ")
  skeleton[["cases"]][["sex_unk"]] = get_information("NV: Cases sex not reported?: ")
  skeleton[["cases"]][["race_white"]] = get_information("NV: Cases race white?: ")
  skeleton[["cases"]][["race_hispanic"]] = get_information("NV: Cases race hispanic?: ")
  skeleton[["cases"]][["race_AfrA"]] = get_information("NV: Cases race AfrA?: ")
  skeleton[["cases"]][["race_asian"]] = get_information("NV: Cases race asian?: ")
  skeleton[["cases"]][["race_NatA"]] = get_information("NV: Cases race NatA?: ")

  skeleton[["deaths"]][["age_0_9"]] = get_information("NV: Deaths ages < 10?: ")
  skeleton[["deaths"]][["age_10_19"]] = get_information("NV: Deaths ages 10-19?: ")
  skeleton[["deaths"]][["age_20_29"]] = get_information("NV: Deaths ages 20-29?: ")
  skeleton[["deaths"]][["age_30_39"]] = get_information("NV: Deaths ages 30-39?: ")
  skeleton[["deaths"]][["age_40_49"]] = get_information("NV: Deaths ages 40-49?: ")
  skeleton[["deaths"]][["age_50_59"]] = get_information("NV: Deaths ages 50-59?: ")
  skeleton[["deaths"]][["age_60_69"]] = get_information("NV: Deaths ages 60-69?: ")
  skeleton[["deaths"]][["age_70+"]] = get_information("NV: Deaths ages 70+?: ")
  skeleton[["deaths"]][["age_unk"]] = get_information("NV: Deaths ages not reported?: ")
  skeleton[["deaths"]][["sex_male"]] = get_information("NV: Deaths sex male?: ")
  skeleton[["deaths"]][["sex_female"]] = get_information("NV: Deaths sex female?: ")
  skeleton[["deaths"]][["sex_unk"]] = get_information("NV: Deaths sex not reported?: ")
  skeleton[["deaths"]][["race_white"]] = get_information("NV: Deaths race white?: ")
  skeleton[["deaths"]][["race_hispanic"]] = get_information("NV: Deaths race hispanic?: ")
  skeleton[["deaths"]][["race_AfrA"]] = get_information("NV: Deaths race AfrA?: ")
  skeleton[["deaths"]][["race_asian"]] = get_information("NV: Deaths race asian?: ")
  skeleton[["deaths"]][["race_NatA"]] = get_information("NV: Deaths race NatA?: ")
  
  full_skeleton = as_tibble(skeleton) %>% 
    standardize %>% 
    mutate(
      state_name = "Nevada",
      Link = "https://nvhealthresponse.nv.gov/",
      platform = "manual",
      comments = "",
      last.update = Sys.time() %>% as_date) %>% 
    select(
      state_name, Link,
      total.tested:hosp_gender,
      platform, comments, last.update)
  
  return(full_skeleton)
}

get_new_york = function() {
  browseURL("https://covid19tracker.health.ny.gov/views/NYS-COVID19-Tracker/NYSDOHCOVID-19Tracker-DailyTracker?%3Aembed=yes&%3Atoolbar=no#/views/NYS-COVID19-Tracker/NYSDOHCOVID-19Tracker-Map?%253Aembed=yes&%253Atoolbar=no")
  
  skeleton = skeleton_table(ny_cols)
  
  skeleton[["tested"]][["total"]] = get_information("NY: Total tested?: ")
  skeleton[["cases"]][["total"]] = get_information("NY: Total cases?: ")
  
  skeleton[["cases"]][["sex_male"]] = get_information("NY: Cases sex male %?: ")
  skeleton[["cases"]][["sex_female"]] = get_information("NY: Cases sex female %?: ")
  skeleton[["cases"]][["sex_unk"]] = get_information("NY: Cases sex unknown %?: ")
  
  skeleton[["deaths"]][["total"]] = get_information("NY: Total deaths?: ")
  
  skeleton[["deaths"]][["race_hispanic"]] = get_information("NY: Deaths hispanic %?: ")
  skeleton[["deaths"]][["race_AfrA"]] = get_information("NY: Deaths race AfrA %?: ")
  skeleton[["deaths"]][["race_white"]] = get_information("NY: Deaths race white %?: ")
  skeleton[["deaths"]][["race_asian"]] = get_information("NY: Deaths race asian %?: ")
  skeleton[["deaths"]][["race_other"]] = get_information("NY: Deaths race other %?: ")
  
  skeleton[["deaths"]][["age_0_9"]] = get_information("NY: Deaths age 0 to 9?: ")
  skeleton[["deaths"]][["age_10_19"]] = get_information("NY: Deaths age 10 to 19?: ")
  skeleton[["deaths"]][["age_20_29"]] = get_information("NY: Deaths age 20 to 29?: ")
  skeleton[["deaths"]][["age_30_39"]] = get_information("NY: Deaths age 30 to 39?: ")
  skeleton[["deaths"]][["age_40_49"]] = get_information("NY: Deaths age 40 to 49?: ")
  skeleton[["deaths"]][["age_50_59"]] = get_information("NY: Deaths age 50 to 59?: ")
  skeleton[["deaths"]][["age_60_69"]] = get_information("NY: Deaths age 60 to 69?: ")
  skeleton[["deaths"]][["age_70_79"]] = get_information("NY: Deaths age 70 to 79?: ")
  skeleton[["deaths"]][["age_80_89"]] = get_information("NY: Deaths age 80 to 89?: ")
  skeleton[["deaths"]][["age_90+"]] = get_information("NY: Deaths age 90+?: ")
  skeleton[["deaths"]][["age_unk"]] = get_information("NY: Deaths age unknown?: ")
  
  full_skeleton = as_tibble(skeleton) %>% 
    standardize %>% 
    mutate(
      state_name = "New York",
      Link = "https://covid19tracker.health.ny.gov/views/NYS-COVID19-Tracker/NYSDOHCOVID-19Tracker-TableView?%3Aembed=yes&%3Atoolbar=no#/views/NYS%2dCOVID19%2dTracker/NYSDOHCOVID%2d19Tracker%2dMap?%253Aembed=yes&%253Atoolbar=no",
      platform = "manual",
      comments = "Percentages present. Manual entry.",
      last.update = Sys.time() %>% as_date) %>% 
    select(
      state_name, Link,
      total.tested:hosp_gender,
      platform, comments, last.update)
  
  return(full_skeleton)
}

get_south_dakota = function() {
  browseURL("https://doh.sd.gov/news/coronavirus.aspx#SD")
  
  skeleton = skeleton_table(sd_cols)
  
  skeleton[["tested"]][["total"]] = get_information("SD: Total tested?: ")
  skeleton[["cases"]][["total"]] = get_information("SD: Total cases?: ")
  skeleton[["hospitalized"]][["total"]] = get_information("SD: Total hospitalized?: ")
  skeleton[["deaths"]][["total"]] = get_information("SD: Total deaths?: ")
  
  skeleton[["cases"]][["age_0_19"]] = get_information("SD: Cases age 0-19?: ")
  skeleton[["cases"]][["age_20_29"]] = get_information("SD: Cases age 20-29?: ")
  skeleton[["cases"]][["age_30_39"]] = get_information("SD: Cases age 30-39?: ")  
  skeleton[["cases"]][["age_40_49"]] = get_information("SD: Cases age 40-49?: ")
  skeleton[["cases"]][["age_50_59"]] = get_information("SD: Cases age 50-59?: ")
  skeleton[["cases"]][["age_60_69"]] = get_information("SD: Cases age 60-69?: ")
  skeleton[["cases"]][["age_70_79"]] = get_information("SD: Cases age 70-79?: ")
  skeleton[["cases"]][["age_80+"]] = get_information("SD: Cases age 80+?: ")
  
  skeleton[["hospitalized"]][["age_0_19"]] = get_information("SD: Hospitalized age 0-19?: ")
  skeleton[["hospitalized"]][["age_20_29"]] = get_information("SD: Hospitalized age 20-29?: ")
  skeleton[["hospitalized"]][["age_30_39"]] = get_information("SD: Hospitalized age 30-39?: ")  
  skeleton[["hospitalized"]][["age_40_49"]] = get_information("SD: Hospitalized age 40-49?: ")
  skeleton[["hospitalized"]][["age_50_59"]] = get_information("SD: Hospitalized age 50-59?: ")
  skeleton[["hospitalized"]][["age_60_69"]] = get_information("SD: Hospitalized age 60-69?: ")
  skeleton[["hospitalized"]][["age_70_79"]] = get_information("SD: Hospitalized age 70-79?: ")
  skeleton[["hospitalized"]][["age_80+"]] = get_information("SD: Hospitalized age 80+?: ")
  
  skeleton[["deaths"]][["age_0_19"]] = get_information("SD: Deaths age 0-19?: ")
  skeleton[["deaths"]][["age_20_29"]] = get_information("SD: Deaths age 20-29?: ")
  skeleton[["deaths"]][["age_30_39"]] = get_information("SD: Deaths age 30-39?: ")  
  skeleton[["deaths"]][["age_40_49"]] = get_information("SD: Deaths age 40-49?: ")
  skeleton[["deaths"]][["age_50_59"]] = get_information("SD: Deaths age 50-59?: ")
  skeleton[["deaths"]][["age_60_69"]] = get_information("SD: Deaths age 60-69?: ")
  skeleton[["deaths"]][["age_70_79"]] = get_information("SD: Deaths age 70-79?: ")
  skeleton[["deaths"]][["age_80+"]] = get_information("SD: Deaths age 80+?: ")
  
  skeleton[["cases"]][["sex_male"]] = get_information("SD: Cases sex male?: ")
  skeleton[["cases"]][["sex_female"]] = get_information("SD: Cases sex female?: ")
  
  skeleton[["cases"]][["race_nh_white"]] = get_information("SD: Cases race white?: ")
  skeleton[["cases"]][["race_nh_AfrA"]] = get_information("SD: Cases race AfrA?: ")
  skeleton[["cases"]][["race_hispanic"]] = get_information("SD: Cases hispanic?: ")
  skeleton[["cases"]][["race_other"]] = get_information("SD: Cases race other?: ")
  skeleton[["cases"]][["race_nh_asian"]] = get_information("SD: Cases race asian?: ")
  skeleton[["cases"]][["race_nh_NatA"]] = get_information("SD: Cases race NatA?: ")
  
  full_skeleton = as_tibble(skeleton) %>% 
    standardize %>% 
    mutate(
      state_name = "South Dakota",
      Link = "https://doh.sd.gov/news/coronavirus.aspx#SD",
      platform = "manual",
      comments = "",
      last.update = Sys.time() %>% as_date) %>% 
    select(
      state_name, Link,
      total.tested:hosp_gender,
      platform, comments, last.update)
  
  return(full_skeleton)
}

get_west_virginia = function() {
  browseURL("https://dhhr.wv.gov/COVID-19/Pages/default.aspx")
  
  skeleton = skeleton_table(wv_cols)
  
  skeleton[["tested"]][["total"]] = get_information("WV: Total tested?: ")
  skeleton[["cases"]][["total"]] = get_information("WV:  Total cases?: ")
  skeleton[["deaths"]][["total"]] = get_information("WV: Total deaths?: ")
  
  skeleton[["cases"]][["race_white"]] = get_information("WV: Cases race white %?: ")
  skeleton[["cases"]][["race_AfrA"]] = get_information("WV: Cases race AfrA %?: ")
  skeleton[["cases"]][["race_other"]] = get_information("WV: Cases race other %?: ")
  skeleton[["cases"]][["age_0_9"]] = get_information("WV: Cases age 0-9 %?: ")
  skeleton[["cases"]][["age_10_19"]] = get_information("WV: Cases age 10-19 %?: ")
  skeleton[["cases"]][["age_20_29"]] = get_information("WV: Cases age 20-29 %?: ")
  skeleton[["cases"]][["age_30_39"]] = get_information("WV: Cases age 30-39 %?: ")
  skeleton[["cases"]][["age_40_49"]] = get_information("WV: Cases age 40-49 %?: ")
  skeleton[["cases"]][["age_50_59"]] = get_information("WV: Cases age 50-59 %?: ")
  skeleton[["cases"]][["age_60_69"]] = get_information("WV: Cases age 60-69 %?: ")
  skeleton[["cases"]][["age_70+"]] = get_information("WV: Cases age 70+ %?: ")
  
  skeleton[["cases"]][["sex_male"]] = get_information("WV: Cases male %?: ")
  skeleton[["cases"]][["sex_female"]] = get_information("WV: Cases female %?: ")
  skeleton[["cases"]][["sex_unk"]] = get_information("WV: Cases sex unknown %?: ")
  
  full_skeleton = as_tibble(skeleton) %>% 
    standardize %>% 
    mutate(
      state_name = "West Virginia",
      Link = "https://dhhr.wv.gov/COVID-19/Pages/default.aspx",
      platform = "manual",
      comments = "Percentages present. Manual entry.",
      last.update = Sys.time() %>% as_date) %>% 
    select(
      state_name, Link,
      total.tested:hosp_gender,
      platform, comments, last.update)
  
  return(full_skeleton)
}

get_wyoming = function() {
  browseURL("https://health.wyo.gov/publichealth/infectious-disease-epidemiology-unit/disease/novel-coronavirus/covid-19-map-and-statistics/")
  
  skeleton = skeleton_table(wy_cols)
  
  skeleton[["tested"]][["total"]] = get_information("WY: Total tested?: ")
  skeleton[["cases"]][["total"]] = get_information("WY: Total cases?: ")
  skeleton[["deaths"]][["total"]] = get_information("WY: Total deaths?: ")
  skeleton[["cases"]][["age_0_18"]] = get_information("WY: Cases age <= 18?: ")
  skeleton[["cases"]][["age_19_29"]] = get_information("WY: Cases age 19-29?: ")
  skeleton[["cases"]][["age_30_39"]] = get_information("WY: Cases age 30-39?: ")            
  skeleton[["cases"]][["age_40_49"]] = get_information("WY: Cases age 40-49?: ")             
  skeleton[["cases"]][["age_50_59"]] = get_information("WY: Cases age 50-59?: ")             
  skeleton[["cases"]][["age_60_69"]] = get_information("WY: Cases age 60-69?: ")             
  skeleton[["cases"]][["age_70_79"]] = get_information("WY: Cases age 70-79?: ")            
  skeleton[["cases"]][["age_80+"]] = get_information("WY: Cases age 80+?: ")
  skeleton[["cases"]][["sex_male"]] = get_information("WY: Cases male?: ")
  skeleton[["cases"]][["sex_female"]] = get_information("WY: Cases female?: ")
  skeleton[["cases"]][["race_white"]] = get_information("WY: Cases race white?: ")
  skeleton[["cases"]][["race_NatA"]] = get_information("WY: Cases race NatA?: ")
  skeleton[["cases"]][["race_hispanic"]] = get_information("WY: Cases hispanic %?: ")
  skeleton[["cases"]][["race_AfrA"]] = get_information("WY: Cases race AfrA?: ")
  skeleton[["cases"]][["race_asian"]] = get_information("WY: Cases race asian?: ")
  skeleton[["cases"]][["race_other"]] = get_information("WY: Cases race other?: ")
  skeleton[["cases"]][["race_unk"]] = get_information("WY: Cases race unknown?: ")
  
  full_skeleton = as_tibble(skeleton) %>% 
    standardize %>% 
    mutate(
      state_name = "Wyoming",
      Link = "https://health.wyo.gov/publichealth/infectious-disease-epidemiology-unit/disease/novel-coronavirus/covid-19-map-and-statistics/",
      platform = "manual",
      comments = "",
      last.update = Sys.time() %>% as_date) %>% 
    select(
      state_name, Link,
      total.tested:hosp_gender,
      platform, comments, last.update)
  
  return(full_skeleton)
  
}

get_information = function(prompt) {
  # Keep asking for information until it is given to prevent typos
  answered = FALSE
  
  while (!answered) {
    answer = readline(prompt = prompt)
    
    if (answer != "") {
      answered = TRUE
      answer = eval(parse(text = answer)) 
      
      if (answer > 0 & answer < 1) {
        processed_answer = answer
      } else {
        processed_answer = answer %>% floor
      }
    } else if (is.na(answer)) {
      answered = TRUE
      processed_answer = NA
    }
  }
  
  return(processed_answer)
}

get_information2 = function(prompt) {
  # Keep asking for information until it is given to prevent typos
  # 05/31: Adjusted this gathering function to let me enter solid percents
  # without the decimal
  answered = FALSE
  
  while (!answered) {
    answer = readline(prompt = prompt)
    
    if (answer != "") {
      answered = TRUE
      answer = eval(parse(text = answer)) 
      processed_answer = answer / 100 # Return the percentage

    } else if (is.na(answer)) {
      answered = TRUE
      processed_answer = NA
    }
  }
  
  return(processed_answer)
}

##########################
# DATA FINALIZATION
##########################

finalize_data = function(data_path) {
  
  # Load in the data created by web scraping
  full_data = read_csv(data_path) %>% 
    mutate(
      total_test = unlist(map(total.tested, function(data) {
        if (is.na(data)) {
          return(NA)
        } else if (str_detect(data, ";")) {
          total = data %>% str_split(";", simplify = TRUE) %>% .[1,1]
          return(as.numeric(total))
        } else {
          as.numeric(data)
        }
      })),
      total_case = unlist(map(total.case, function(data) {
        if (is.na(data)) {
          return(NA)
        } else if (str_detect(data, ";")) {
          total = data %>% str_split(";", simplify = TRUE) %>% .[1,1]
          return(as.numeric(total))
        } else {
          as.numeric(data)
        }
      })),
      total_death = unlist(map(total.death, function(data) {
        if (is.na(data)) {
          return(NA)
        } else if (str_detect(data, ";")) {
          total = data %>% str_split(";", simplify = TRUE) %>% .[1,1]
          return(as.numeric(total))
        } else {
          as.numeric(data)
        }
      })),
      total_hosp = unlist(map(total_hosp, function(data) {
        if (is.na(data)) {
          return(NA)
        } else if (str_detect(data, ";")) {
          total = data %>% str_split(";", simplify = TRUE) %>% .[1,1]
          return(as.numeric(total))
        } else {
          as.numeric(data)
        }
      }))
    )
  
  # Dealing with cases first
  final_case_data = full_data %>% 
    select(state_name, 
           total_test,
           total_case, 
           total_death, 
           total_hosp,
           positive_age, 
           positive_race, 
           positive_gender) %>% 
    mutate(
      case_by_age = map(positive_age, function(data) {
        if (is.na(data)) {
          return(NA)
        } else if (str_detect(data, ";")) {
          age_tib = data %>% 
            str_split(";", simplify = TRUE) %>% 
            str_split(":", simplify = TRUE) %>% 
            as_tibble()
          colnames(age_tib) = c("category", "raw")
          age_tib %>% 
            mutate(
              category = str_trim(category),
              raw = as.numeric(raw)
            )
        }
      }),
      case_by_race = map(positive_race, function(data) {
        if (is.na(data)) {
          return(NA)
        } else if (str_detect(data, ";")) {
          race_tib = data %>% 
            str_split(";", simplify = TRUE) %>% 
            str_split(":", simplify = TRUE) %>% 
            as_tibble()
          colnames(race_tib) = c("category", "raw")
          race_tib %>% 
            mutate(
              category = str_trim(category),
              raw = str_replace(raw, ",", "") %>%  as.numeric(raw)
            )
        }
      }),
      case_by_sex = map(positive_gender, function(data) {
        if (is.na(data)) {
          return(NA)
        } else if (str_detect(data, ";")) {
          sex_tib = data %>% 
            str_split(";", simplify = TRUE) %>% 
            str_split(":", simplify = TRUE) %>% 
            as_tibble()
          colnames(sex_tib) = c("category", "raw")
          sex_tib %>% 
            mutate(
              category = str_trim(category),
              raw = str_replace(raw, ",", "") %>%  as.numeric(raw)
            )
        }
      })
    ) %>% 
    select(
      state_name, 
      total_test,
      total_case, 
      total_death, 
      total_hosp,
      case_by_age,
      case_by_race,
      case_by_sex
    ) 
  
  case_age_data = final_case_data %>% 
    select(state_name, total_test, total_case, total_death, total_hosp, case_by_age) %>% 
    unnest_wider(case_by_age) %>% 
    unnest(c(category, raw)) %>% 
    select(-`...1`) %>% 
    mutate( 
      strata_type = "age",
      data_type = "case"
    )
  
  case_race_data = final_case_data %>% 
    select(state_name, total_test, total_case, total_death, total_hosp, case_by_race) %>% 
    unnest_wider(case_by_race) %>% 
    unnest(c(category, raw)) %>% 
    select(-`...1`) %>% 
    mutate( 
      strata_type = "race",
      data_type = "case" 
    )
  
  case_sex_data = final_case_data %>% 
    select(state_name, total_test, total_case, total_death, total_hosp, case_by_sex) %>% 
    unnest_wider(case_by_sex) %>% 
    unnest(c(category, raw)) %>% 
    select(-`...1`) %>% 
    mutate( 
      strata_type = "sex",
      data_type = "case" 
    )
  
  final_death_data = full_data %>% 
    select(state_name, 
           total_test,
           total_case, 
           total_death, 
           total_hosp,
           death_age, 
           death_race, 
           death_gender) %>% 
    mutate(
      death_by_age = map(death_age, function(data) {
        if (is.na(data)) {
          return(NA)
        } else if (str_detect(data, ";")) {
          age_tib = data %>% 
            str_split(";", simplify = TRUE) %>% 
            str_split(":", simplify = TRUE) %>% 
            as_tibble()
          colnames(age_tib) = c("category", "raw")
          age_tib %>% 
            mutate(
              category = str_trim(category),
              raw = as.numeric(raw)
            )
        }
      }),
      death_by_race = map(death_race, function(data) {
        if (is.na(data)) {
          return(NA)
        } else if (str_detect(data, ";")) {
          race_tib = data %>% 
            str_split(";", simplify = TRUE) %>% 
            str_split(":", simplify = TRUE) %>% 
            as_tibble()
          colnames(race_tib) = c("category", "raw")
          race_tib %>% 
            mutate(
              category = str_trim(category),
              raw = str_replace(raw, ",", "") %>%  as.numeric(raw)
            )
        }
      }),
      death_by_sex = map(death_gender, function(data) {
        if (is.na(data)) {
          return(NA)
        } else if (str_detect(data, ";")) {
          sex_tib = data %>% 
            str_split(";", simplify = TRUE) %>% 
            str_split(":", simplify = TRUE) %>% 
            as_tibble()
          colnames(sex_tib) = c("category", "raw")
          sex_tib %>% 
            mutate(
              category = str_trim(category),
              raw = str_replace(raw, ",", "") %>%  as.numeric(raw)
            )
        }
      })
    ) %>% 
    select(
      state_name, 
      total_test, 
      total_case, 
      total_death, 
      total_hosp,
      death_by_race,
      death_by_age,
      death_by_sex
    ) 
  
  death_age_data = final_death_data %>% 
    select(state_name, total_test, total_case, total_death, total_hosp, death_by_age) %>% 
    unnest_wider(death_by_age) %>% 
    unnest(c(category, raw)) %>% 
    select(-`...1`) %>% 
    mutate( 
      strata_type = "age",
      data_type = "death"
    )
  
  death_race_data = final_death_data %>% 
    select(state_name, total_test, total_case, total_death, total_hosp, death_by_race) %>% 
    unnest_wider(death_by_race) %>% 
    unnest(c(category, raw)) %>% 
    select(-`...1`) %>% 
    mutate( 
      strata_type = "race",
      data_type = "death"
    )
  
  death_sex_data = final_death_data %>% 
    select(state_name, total_test, total_case, total_death, total_hosp, death_by_sex) %>% 
    unnest_wider(death_by_sex) %>% 
    unnest(c(category, raw)) %>% 
    select(-`...1`) %>% 
    mutate( 
      strata_type = "sex",
      data_type = "death" 
    )
  
  final_hosp_data = full_data %>% 
    select(state_name, 
           total_test,
           total_case, 
           total_death, 
           total_hosp, 
           hosp_age, 
           hosp_race, 
           hosp_gender) %>% 
    mutate(
      hosp_by_age = map(hosp_age, function(data) {
        if (is.na(data)) {
          return(NA)
        } else if (str_detect(data, ";")) {
          age_tib = data %>% 
            str_split(";", simplify = TRUE) %>% 
            str_split(":", simplify = TRUE) %>% 
            as_tibble()
          colnames(age_tib) = c("category", "raw")
          age_tib %>% 
            mutate(
              category = str_trim(category),
              raw = as.numeric(raw)
            )
        }
      }),
      hosp_by_race = map(hosp_race, function(data) {
        if (is.na(data)) {
          return(NA)
        } else if (str_detect(data, ";")) {
          race_tib = data %>% 
            str_split(";", simplify = TRUE) %>% 
            str_split(":", simplify = TRUE) %>% 
            as_tibble()
          colnames(race_tib) = c("category", "raw")
          race_tib %>% 
            mutate(
              category = str_trim(category),
              raw = str_replace(raw, ",", "") %>%  as.numeric(raw)
            )
        }
      }),
      hosp_by_sex = map(hosp_gender, function(data) {
        if (is.na(data)) {
          return(NA)
        } else if (str_detect(data, ";")) {
          sex_tib = data %>% 
            str_split(";", simplify = TRUE) %>% 
            str_split(":", simplify = TRUE) %>% 
            as_tibble()
          colnames(sex_tib) = c("category", "raw")
          sex_tib %>% 
            mutate(
              category = str_trim(category),
              raw = str_replace(raw, ",", "") %>%  as.numeric(raw)
            )
        }
      })
    ) %>% 
    select(
      state_name, 
      total_test, 
      total_case, 
      total_death, 
      total_hosp,
      hosp_by_race,
      hosp_by_age,
      hosp_by_sex
    ) 
  
  hosp_age_data = final_hosp_data %>% 
    unnest_wider(hosp_by_age) %>% 
    unnest(c(category, raw)) %>% 
    select(-`...1`) %>% 
    mutate( 
      strata_type = "age",
      data_type = "hosp"
    )
  
  hosp_race_data = final_hosp_data %>% 
    select(state_name, total_test, total_case, total_death, total_hosp, hosp_by_race) %>% 
    unnest_wider(hosp_by_race) %>% 
    unnest(c(category, raw)) %>% 
    select(-`...1`) %>% 
    mutate( 
      strata_type = "race",
      data_type = "hosp"
    )
  
  hosp_sex_data = final_hosp_data %>% 
    select(state_name, total_test, total_case, total_death, total_hosp, hosp_by_sex) %>% 
    unnest_wider(hosp_by_sex) %>% 
    unnest(c(category, raw)) %>% 
    select(-`...1`) %>% 
    mutate( 
      strata_type = "sex",
      data_type = "hosp"
    )
  
  final = bind_rows(
    case_age_data,
    case_race_data,
    case_sex_data,
    death_age_data,
    death_race_data,
    death_sex_data,
    hosp_age_data,
    hosp_race_data,
    hosp_sex_data) 
  
  return(final)
}
