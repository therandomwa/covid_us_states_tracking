##########################
# DATA STRUCTURE FUNCTIONS
##########################

# Default names to use in the skeleton columns
default_cols = c(
  "total",
  "age_0_19", 
  "age_20_29", 
  "age_30_39", 
  "age_40_49",
  "age_50_59", 
  "age_60_69", 
  "age_70_79", 
  "age_80+", 
  "age_unk",
  "sex_male", 
  "sex_female", 
  "sex_unk",
  "ethnicity_hispanic", 
  "ethnicity_non_hispanic",
  "ethnicity_unk",
  "race_white", 
  "race_AfrA",
  "race_NatA",
  "race_asian", 
  "race_other",
  "race_multi", 
  "race_unk")

az_cols = c(
  "total",
  "age_0_19", 
  "age_20_44", 
  "age_45_54",
  "age_55_64", 
  "age_65+", 
  "age_unk",
  "sex_male", 
  "sex_female", 
  "sex_unk",
  "ethnicity_hispanic", 
  "ethnicity_non_hispanic",
  "ethnicity_unk",
  "race_white", 
  "race_AfrA",
  "race_NatA",
  "race_asian", 
  "race_other",
  "race_multi", 
  "race_unk")

fl_cols = c(
  "total",
  "age_0_4", 
  "age_5_14", 
  "age_15_24", 
  "age_25_34",
  "age_35_44", 
  "age_45_54", 
  "age_55_64", 
  "age_65_74", 
  "age_75_84", 
  "age_85+", 
  "age_unk",
  "sex_male", 
  "sex_female", 
  "sex_unk",
  "ethnicity_hispanic", 
  "ethnicity_non_hispanic",
  "ethnicity_unk",
  "race_white", 
  "race_AfrA",
  "race_NatA",
  "race_asian", 
  "race_other",
  "race_multi", 
  "race_unk")

tn_cols = c(
  "total",
  "age_0_10", 
  "age_11_20", 
  "age_21_30",
  "age_31_40",
  "age_41_50",
  "age_51_60",
  "age_61_70",
  "age_71_80",
  "age_81+", 
  "age_unk",
  "sex_male", 
  "sex_female", 
  "sex_unk",
  "ethnicity_hispanic", 
  "ethnicity_non_hispanic",
  "ethnicity_unk",
  "race_white", 
  "race_AfrA",
  "race_NatA",
  "race_asian", 
  "race_other",
  "race_multi", 
  "race_unk")

nj_cols = c(
  "total",
  "age_0_4", 
  "age_5_17", 
  "age_18_29", 
  "age_30_49",
  "age_50_64", 
  "age_65_79", 
  "age_80+", 
  "age_unk",
  "sex_male", 
  "sex_female", 
  "sex_unk",
  "ethnicity_hispanic", 
  "ethnicity_non_hispanic",
  "ethnicity_unk",
  "race_white", 
  "race_AfrA",
  "race_NatA",
  "race_asian", 
  "race_other",
  "race_multi", 
  "race_unk")

nh_cols = c(
  "total",
  "age_0_9", 
  "age_10_19",
  "age_20_29", 
  "age_30_39", 
  "age_40_49",
  "age_50_59", 
  "age_60_69", 
  "age_70_79", 
  "age_80+", 
  "age_unk",
  "sex_male", 
  "sex_female", 
  "sex_unk",
  "ethnicity_hispanic", 
  "ethnicity_non_hispanic",
  "ethnicity_unk",
  "race_white", 
  "race_AfrA",
  "race_NatA",
  "race_asian", 
  "race_other",
  "race_multi", 
  "race_unk")

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
    total.tested = skeleton[["tested"]][["total"]],
    total.case = skeleton[["cases"]][["total"]],
    total.death = skeleton[["deaths"]][["total"]],
    total_hosp = skeleton[["hospitalized"]][["total"]],
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
      positive_age = map(1, function(s) {
        skeleton %>% filter(str_detect(data, 'age')) %>% select(data, cases)
      }),
      positive_gender = map(1, function(s) {
        skeleton %>% filter(str_detect(data, 'sex')) %>% select(data, cases) 
      }),
      death_race = map(1, function(s) {
        skeleton %>% filter(str_detect(data, 'race')) %>% select(data, deaths)
      }),
      death_age = map(1, function(s) {
        skeleton %>% filter(str_detect(data, 'race')) %>% select(data, deaths)
      }),
      death_gender = map(1, function(s) {
        skeleton %>% filter(str_detect(data, 'sex')) %>% select(data, deaths)
      }),
      hosp_race = map(1, function(s) {
        skeleton %>% filter(str_detect(data, 'race')) %>% select(data, hospitalized)
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

get_oklahoma = function() {
  case_url = "https://storage.googleapis.com/ok-covid-gcs-public-download/oklahoma_cases_county.csv"
  
  case_by_county = read_csv(case_url)
  skeleton = skeleton_table(default_cols)
  
  # Push items into the skeleton  
  skeleton[["cases"]][["total"]] = case_by_county %>% 
    pull(Cases) %>% sum
  skeleton[["recovered"]][["total"]] = case_by_county %>% 
    pull(Recovered) %>% sum
  skeleton[["deaths"]][["total"]] = case_by_county %>% 
    pull(Deaths) %>% sum
  
  as_tibble(skeleton) %>% 
    standardize %>% 
    mutate(
      state_name = "Oklahoma",
      Link = "https://coronavirus.health.ok.gov",
      platform = "pdf",
      comments = "Demographic and age data available, but is displayed in undesirable format for pdf scraping. Too fragile.",
      last.updat = Sys.time() %>% as_date) %>% 
    select(
      state_name, Link,
      total.tested:hosp_gender,
      platform, comments, last.updat)
}

get_mississippi = function() {
  
  skeleton = skeleton_table(default_cols)
  
  # Extract pdf locations from the site to account for updates
  site_url = "https://msdh.ms.gov/msdhsite/_static/14,0,420.html"
  
  # Need to extract the paths for the demo cases and deaths data
  # since I presume they will change over time
  demo_path = read_html(site_url) %>% 
    html_nodes("body #pageContainer #pageContent #article .msdh .links") %>% 
    .[[6]] %>% 
    html_nodes("li") %>% .[[1]] %>% 
    html_nodes("a") %>% html_attr("href")
  
  demo_death_path = read_html(site_url) %>% 
    html_nodes("body #pageContainer #pageContent #article .msdh .links") %>% 
    .[[6]] %>% 
    html_nodes("li") %>% .[[2]] %>% 
    html_nodes("a") %>% html_attr("href")
  
  demo_url = paste0("https://msdh.ms.gov/msdhsite/_static/", demo_path)
  demo_death_url = paste0("https://msdh.ms.gov/msdhsite/_static/", demo_death_path)
  
  summary_vals = read_html(site_url) %>% 
    html_nodes("body #pageContainer #pageContent #article .msdh #msdhTotalCovid-19Cases") %>% 
    html_table(fill = TRUE) %>% .[[1]] %>%
    tail(1) %>% unlist() %>% .[-1] %>%
    str_replace(",", "") %>% as.numeric()
  
  skeleton[["cases"]][["total"]] = summary_vals[1]
  skeleton[["deaths"]][["total"]] = summary_vals[2]
  
  demo = pdf_text(demo_url) %>% tail(1) %>% #only want the last page
    str_split("\n") %>% .[[1]] %>% 
    str_squish() %>% .[17] %>% # 17 assumed to be the subtotal row
    str_split(" ") %>% .[[1]] %>% .[-1] %>% 
    as.numeric()
  
  skeleton[["cases"]][["race_AfrA"]] = demo[2] + demo[8] + demo[14]
  skeleton[["cases"]][["race_white"]] = demo[3] + demo[9] + demo[15]
  skeleton[["cases"]][["race_NatA"]] = demo[4] + demo[10] + demo[16]
  skeleton[["cases"]][["race_asian"]] = demo[5] + demo[11] + demo[17]
  skeleton[["cases"]][["race_other"]] = demo[6] + demo[12] + demo[18]
  skeleton[["cases"]][["race_unk"]] = demo[7] + demo[13] + demo[19]
  
  skeleton[["cases"]][["ethnicity_non_hispanic"]] = demo[2:7] %>% sum()
  skeleton[["cases"]][["ethnicity_hispanic"]] = demo[8:13] %>% sum()
  skeleton[["cases"]][["ethnicity_unk"]] = demo[14:19] %>% sum()
  
  demo_death = pdf_text(demo_death_url) %>% tail(1) %>% #only want the last page
    str_split("\n") %>% .[[1]] %>% 
    str_squish() %>% .[17] %>% # 17 assumed to be the subtotal row
    str_split(" ") %>% .[[1]] %>% .[-1] %>% 
    as.numeric()
  
  skeleton[["deaths"]][["race_AfrA"]] = demo_death[2] + demo_death[8] + demo_death[14]
  skeleton[["deaths"]][["race_white"]] = demo_death[3] + demo_death[9] + demo_death[15]
  skeleton[["deaths"]][["race_NatA"]] = demo_death[4] + demo[10] + demo_death[16]
  skeleton[["deaths"]][["race_asian"]] = demo_death[5] + demo_death[11] + demo_death[17]
  skeleton[["deaths"]][["race_other"]] = demo_death[6] + demo_death[12] + demo_death[18]
  skeleton[["deaths"]][["race_unk"]] = demo_death[7] + demo_death[13] + demo_death[19]
  
  skeleton[["deaths"]][["ethnicity_non_hispanic"]] = demo_death[2:7] %>% sum()
  skeleton[["deaths"]][["ethnicity_hispanic"]] = demo_death[8:13] %>% sum()
  skeleton[["deaths"]][["ethnicity_unk"]] = demo_death[14:19] %>% sum()
  
  as_tibble(skeleton) %>% 
    standardize %>% 
    mutate(
      state_name = "Mississippi",
      Link = site_url,
      platform = "pdf",
      comments = "Age data only available in images on site",
      last.updat = Sys.time() %>% as_date) %>% 
    select(
      state_name, Link,
      total.tested:hosp_gender,
      platform, comments, last.updat)
  
}

get_florida = function() {
  # Get pdf from: https://www.floridadisaster.org/covid19/covid-19-data-reports/
  url = "https://www.floridadisaster.org/covid19/covid-19-data-reports/"
  
  pdf_path = read_html(url) %>% 
    html_nodes("body .l-surround #mainContent #text-page-wrap .row") %>% 
    html_nodes("#mainContent .main-column p a") %>% html_attr("href") %>% .[1]
  # Index 1 gets the first pdf in the list, presumed to be the most recent
  
  pdf_url = paste0("https://www.floridadisaster.org/", pdf_path)
  data = pdf_text(pdf_url)
  
  # Demographic data is on
  demographic_data = data %>% .[3] %>% 
    str_split(., "\n") %>% .[[1]] %>% 
    str_squish()
  
  # Initialize skeleton
  skeleton = skeleton_table(fl_cols)
  
  tested = data %>% .[1] %>% 
    str_split(., "\n") %>% .[[1]] %>% 
    str_squish() %>% .[[22]] %>% 
    str_split(., " ", simplify = TRUE) %>% .[1, 3] %>% 
    str_replace(., ",", "") %>% as.numeric()
  
  negatives = data %>% .[1] %>% 
    str_split(., "\n") %>% .[[1]] %>% 
    str_squish() %>% .[[25]] %>% 
    str_split(., " ", simplify = TRUE) %>% .[1, 2] %>% 
    str_replace(., ",", "") %>% as.numeric()
  
  cases = demographic_data %>% .[16] %>% 
    str_split(., " ", simplify = TRUE) %>% .[1, 2] %>% 
    str_replace(., ",", "") %>% as.numeric()
  
  deaths = demographic_data %>% .[16] %>% 
    str_split(., " ", simplify = TRUE) %>% .[1, 4] %>% 
    str_replace(., ",", "") %>% as.numeric()
  
  
  males = demographic_data %>% .[5] %>% 
    str_split(., " ", simplify = TRUE) %>%  .[1, 10] %>% 
    str_replace(., ",", "") %>% as.numeric()
  
  # Each row contains data on cases, hosp and death
  between_0_and_4 = demographic_data %>% .[5] %>% 
    str_split(., " ", simplify = TRUE)
  
  between_0_and_4_case = between_0_and_4 %>% .[1, 3] %>% 
    str_replace(",","") %>% as.numeric
  between_0_and_4_hosp = between_0_and_4 %>% .[1, 5] %>% 
    str_replace(",","") %>% as.numeric
  between_0_and_4_death = between_0_and_4 %>% .[1, 7] %>% 
    str_replace(",","") %>% as.numeric
  
  between_5_and_14 = demographic_data %>% .[6] %>% 
    str_split(., " ", simplify = TRUE)
  
  between_5_and_14_case = between_5_and_14 %>% .[1, 3] %>% 
    str_replace(",","") %>% as.numeric
  between_5_and_14_hosp = between_5_and_14 %>% .[1, 5] %>% 
    str_replace(",","") %>% as.numeric
  between_5_and_14_death = between_5_and_14 %>% .[1, 7] %>% 
    str_replace(",","") %>% as.numeric
  
  between_15_and_24 = demographic_data %>% .[7] %>% 
    str_split(., " ", simplify = TRUE)
  
  between_15_and_24_case = between_15_and_24 %>% .[1, 3] %>% 
    str_replace(",","") %>% as.numeric
  between_15_and_24_hosp = between_15_and_24 %>% .[1, 5] %>% 
    str_replace(",","") %>% as.numeric
  between_15_and_24_death = between_15_and_24 %>% .[1, 7] %>% 
    str_replace(",","") %>% as.numeric
  
  between_25_and_34 = demographic_data %>% .[8] %>% 
    str_split(., " ", simplify = TRUE)
  
  between_25_and_34_case = between_25_and_34 %>% .[1, 3] %>% 
    str_replace(",","") %>% as.numeric
  between_25_and_34_hosp = between_25_and_34 %>% .[1, 5] %>% 
    str_replace(",","") %>% as.numeric
  between_25_and_34_death = between_25_and_34 %>% .[1, 7] %>% 
    str_replace(",","") %>% as.numeric
  
  between_35_and_44 = demographic_data %>% .[9] %>% 
    str_split(., " ", simplify = TRUE)
  
  between_35_and_44_case = between_35_and_44 %>% .[1, 3] %>% 
    str_replace(",","") %>% as.numeric
  between_35_and_44_hosp = between_35_and_44 %>% .[1, 5] %>% 
    str_replace(",","") %>% as.numeric
  between_35_and_44_death = between_35_and_44 %>% .[1, 7] %>% 
    str_replace(",","") %>% as.numeric
  
  between_45_and_54 = demographic_data %>% .[10] %>% 
    str_split(., " ", simplify = TRUE)
  
  between_45_and_54_case = between_45_and_54 %>% .[1, 3] %>% 
    str_replace(",","") %>% as.numeric
  between_45_and_54_hosp = between_45_and_54 %>% .[1, 5] %>% 
    str_replace(",","") %>% as.numeric
  between_45_and_54_death = between_45_and_54 %>% .[1, 7] %>% 
    str_replace(",","") %>% as.numeric
  
  between_55_and_64 = demographic_data %>% .[11] %>% 
    str_split(., " ", simplify = TRUE)
  
  between_55_and_64_case = between_55_and_64 %>% .[1, 3] %>% 
    str_replace(",","") %>% as.numeric
  between_55_and_64_hosp = between_55_and_64 %>% .[1, 5] %>% 
    str_replace(",","") %>% as.numeric
  between_55_and_64_death = between_55_and_64 %>% .[1, 7] %>% 
    str_replace(",","") %>% as.numeric
  
  between_65_and_74 = demographic_data %>% .[12] %>% 
    str_split(., " ", simplify = TRUE)
  
  between_65_and_74_case = between_65_and_74 %>% .[1, 3] %>% 
    str_replace(",","") %>% as.numeric
  between_65_and_74_hosp = between_65_and_74 %>% .[1, 5] %>% 
    str_replace(",","") %>% as.numeric
  between_65_and_74_death = between_65_and_74 %>% .[1, 7] %>% 
    str_replace(",","") %>% as.numeric
  
  between_75_and_84 = demographic_data %>% .[13] %>% 
    str_split(., " ", simplify = TRUE)
  
  between_75_and_84_case = between_75_and_84 %>% .[1, 3] %>% 
    str_replace(",","") %>% as.numeric
  between_75_and_84_hosp = between_75_and_84 %>% .[1, 5] %>% 
    str_replace(",","") %>% as.numeric
  between_75_and_84_death = between_75_and_84 %>% .[1, 7] %>% 
    str_replace(",","") %>% as.numeric
  
  older_than_85 = demographic_data %>% .[14] %>% 
    str_split(., " ", simplify = TRUE)
  
  older_than_85_case = older_than_85 %>% .[1, 3] %>% 
    str_replace(",","") %>% as.numeric
  older_than_85_hosp = older_than_85 %>% .[1, 5] %>% 
    str_replace(",","") %>% as.numeric
  older_than_85_death = older_than_85 %>% .[1, 7] %>% 
    str_replace(",","") %>% as.numeric
  
  unknown_age = demographic_data %>% .[15] %>% 
    str_split(., " ", simplify = TRUE)
  
  unknown_age_case = unknown_age %>% .[1, 2] %>% 
    str_replace(",","") %>% as.numeric
  unknown_age_hosp = unknown_age %>% .[1, 4] %>% 
    str_replace(",","") %>% as.numeric
  unknown_age_death = unknown_age %>% .[1, 6] %>% 
    str_replace(",","") %>% as.numeric
  
  hispanic = demographic_data %>% .[18] %>% 
    str_split(., " ", simplify = TRUE)
  
  hispanic_case = hispanic %>% .[1, 2] %>% 
    str_replace(",","") %>% as.numeric
  hispanic_hosp = hispanic %>% .[1, 4] %>% 
    str_replace(",","") %>% as.numeric
  hispanic_death = hispanic %>% .[1, 6] %>% 
    str_replace(",","") %>% as.numeric
  
  non_hispanic = demographic_data %>% .[19] %>% 
    str_split(., " ", simplify = TRUE)
  
  non_hispanic_case = non_hispanic %>% .[1, 2] %>% 
    str_replace(",","") %>% as.numeric
  non_hispanic_hosp = non_hispanic %>% .[1, 4] %>% 
    str_replace(",","") %>% as.numeric
  non_hispanic_death = non_hispanic %>% .[1, 6] %>% 
    str_replace(",","") %>% as.numeric
  
  unknown_ethnicity = demographic_data %>% .[20] %>% 
    str_split(., " ", simplify = TRUE)
  
  unknown_ethnicity_case = unknown_ethnicity %>% .[1, 2] %>% 
    str_replace(",","") %>% as.numeric
  unknown_ethnicity_hosp = unknown_ethnicity %>% .[1, 4] %>% 
    str_replace(",","") %>% as.numeric
  unknown_ethnicity_death = unknown_ethnicity %>% .[1, 6] %>% 
    str_replace(",","") %>% as.numeric
  
  white = demographic_data %>% .[23] %>% 
    str_split(., " ", simplify = TRUE)
  
  white_case = white %>% .[1, 2] %>% 
    str_replace(",","") %>% as.numeric
  white_hosp = white %>% .[1, 4] %>% 
    str_replace(",","") %>% as.numeric
  white_death = white %>% .[1, 6] %>% 
    str_replace(",","") %>% as.numeric
  
  black = demographic_data %>% .[27] %>% 
    str_split(., " ", simplify = TRUE) 
  
  black_case = black %>% .[1, 2] %>% 
    str_replace(",","") %>% as.numeric
  black_hosp = black %>% .[1, 4] %>% 
    str_replace(",","") %>% as.numeric
  black_death = black %>% .[1, 6] %>% 
    str_replace(",","") %>% as.numeric
  
  other_race = demographic_data %>% .[31] %>% 
    str_split(., " ", simplify = TRUE) 
  
  other_race_case = other_race %>% .[1, 2] %>% 
    str_replace(",","") %>% as.numeric
  other_race_hosp = other_race %>% .[1, 4] %>% 
    str_replace(",","") %>% as.numeric
  other_race_death = other_race %>% .[1, 6] %>% 
    str_replace(",","") %>% as.numeric
  
  unk_race = demographic_data %>% .[35] %>% 
    str_split(., " ", simplify = TRUE)
  
  unk_race_case = unk_race %>% .[1, 3] %>% 
    str_replace(",","") %>% as.numeric
  unk_race_hosp = unk_race %>% .[1, 5] %>% 
    str_replace(",","") %>% as.numeric
  unk_race_death = unk_race %>% .[1, 7] %>% 
    str_replace(",","") %>% as.numeric
  
  # Log all of the information
  skeleton[["tested"]][["total"]] = tested
  skeleton[["negatives"]][["total"]] = negatives
  
  skeleton[["cases"]][["total"]] = cases
  
  skeleton[["cases"]][["sex_male"]] = males
  skeleton[["cases"]][["sex_female"]] = cases - males
  
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
      last.updat = Sys.time() %>% as_date) %>% 
    select(
      state_name, Link,
      total.tested:hosp_gender,
      platform, comments, last.updat)
}

get_tennessee = function(date = "today") {
  
  # Just in case you don't start bringing together data until the midnight after
  if (date == "today") {
    date_str = now = Sys.time() %>% as_date
  } else {
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
  
  age_url = paste0("https://www.tn.gov", xlsx_paths[1])
  case_url = paste0("https://www.tn.gov", xlsx_paths[3])
  demo_url = paste0("https://www.tn.gov", xlsx_paths[5])
  
  
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
  
  tests = case_data %>% pull(TOTAL_TESTS)
  cases = case_data %>% pull(TOTAL_CASES)
  negatives = case_data %>% pull(NEG_TESTS)
  deaths = case_data %>% pull(TOTAL_DEATHS)
  recovered = case_data %>% pull(TOTAL_RECOVERED)
  
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
    filter(Cat_Detail == "Male") %>% pull(Cat_CaseCount) 
  skeleton[["cases"]][["sex_female"]] = sex_cases %>% 
    filter(Cat_Detail == "Female") %>% pull(Cat_CaseCount) 
  skeleton[["cases"]][["sex_unk"]] = sex_cases %>% 
    filter(Cat_Detail == "Pending") %>% pull(Cat_CaseCount) 
  skeleton[["cases"]][["race_white"]] = race_cases %>% 
    filter(Cat_Detail == "White") %>% pull(Cat_CaseCount) 
  skeleton[["cases"]][["race_AfrA"]] = race_cases %>% 
    filter(Cat_Detail == "Black or African American") %>% pull(Cat_CaseCount) 
  skeleton[["cases"]][["race_other"]] = race_cases %>% 
    filter(Cat_Detail == "Other/Multiracial") %>% pull(Cat_CaseCount)
  skeleton[["cases"]][["race_asian"]] = race_cases %>% 
    filter(Cat_Detail == "Asian") %>% pull(Cat_CaseCount) 
  skeleton[["cases"]][["race_unk"]] = race_cases %>% 
    filter(Cat_Detail == "Pending") %>% pull(Cat_CaseCount) 
  skeleton[["cases"]][["ethnicity_hispanic"]] = ethnicity_cases %>% 
    filter(Cat_Detail == "Hispanic") %>% pull(Cat_CaseCount)
  skeleton[["cases"]][["ethnicity_non_hispanic"]] = ethnicity_cases %>% 
    filter(Cat_Detail == "Not Hispanic or Latino") %>% pull(Cat_CaseCount)
  skeleton[["cases"]][["ethnicity_unk"]] = ethnicity_cases %>% 
    filter(Cat_Detail == "Pending") %>% pull(Cat_CaseCount)
  
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
    filter(Cat_Detail == "Male") %>% pull(CAT_DEATHCOUNT) 
  skeleton[["deaths"]][["sex_female"]] = sex_deaths %>% 
    filter(Cat_Detail == "Female") %>% pull(CAT_DEATHCOUNT) 
  skeleton[["deaths"]][["sex_unk"]] = sex_deaths %>% 
    filter(Cat_Detail == "Pending") %>% pull(CAT_DEATHCOUNT) 
  skeleton[["deaths"]][["race_white"]] = race_deaths %>% 
    filter(Cat_Detail == "White") %>% pull(CAT_DEATHCOUNT) 
  skeleton[["deaths"]][["race_AfrA"]] = race_deaths %>% 
    filter(Cat_Detail == "Black or African American") %>% pull(CAT_DEATHCOUNT) 
  skeleton[["deaths"]][["race_other"]] = race_deaths %>% 
    filter(Cat_Detail == "Other/Multiracial") %>% pull(CAT_DEATHCOUNT)
  skeleton[["deaths"]][["race_asian"]] = race_deaths %>% 
    filter(Cat_Detail == "Asian") %>% pull(CAT_DEATHCOUNT) 
  skeleton[["deaths"]][["race_unk"]] = race_deaths %>% 
    filter(Cat_Detail == "Pending") %>% pull(CAT_DEATHCOUNT) 
  skeleton[["deaths"]][["ethnicity_hispanic"]] = ethnicity_deaths %>% 
    filter(Cat_Detail == "Hispanic") %>% pull(CAT_DEATHCOUNT)
  skeleton[["deaths"]][["ethnicity_non_hispanic"]] = ethnicity_deaths %>% 
    filter(Cat_Detail == "Not Hispanic or Latino") %>% pull(CAT_DEATHCOUNT)
  skeleton[["deaths"]][["ethnicity_unk"]] = ethnicity_deaths %>% 
    filter(Cat_Detail == "Pending") %>% pull(CAT_DEATHCOUNT)
  
  skeleton[["recovered"]][["total"]] = recovered
  
  as_tibble(skeleton) %>% 
    standardize %>% 
    mutate(
      state_name = "Tennessee",
      Link = url,
      platform = "pdf",
      comments = "",
      last.updat = Sys.time() %>% as_date) %>% 
    select(
      state_name, Link,
      total.tested:hosp_gender,
      platform, comments, last.updat)
}

get_north_carolina = function() {
  
  skeleton = skeleton_table(default_cols)
  url = "https://www.ncdhhs.gov/divisions/public-health/covid19/covid-19-nc-case-count#by-race-ethnicity"
  
  data = read_html(url)
  
  tables = data %>% 
    html_nodes("body #page-wrapper #page #content-container") %>% 
    html_nodes("#main article .section .region") %>% 
    html_nodes("#block-system-main .content #node-103") %>% 
    html_nodes(".content .field .field-items .field-item") %>% 
    html_nodes("table") %>% html_table() 
  
  heading_table = tables %>% .[[1]] %>% 
    str_replace(",", "") %>% unlist() %>% as.numeric()
  demo_table = tables %>% .[[5]]
  colnames(demo_table) = c("name", "cases", "cases%", "deaths", "deaths%")
  
  nata = demo_table %>% 
    filter(name == "American Indian Alaskan Native") %>% 
    select(cases) %>% pull() %>% 
    str_replace(",", "") %>% unlist() %>% as.numeric
  asian = demo_table %>% 
    filter(name == "Asian") %>% 
    select(cases) %>% pull() %>% 
    str_replace(",", "") %>% unlist() %>% as.numeric
  afra = demo_table %>% 
    filter(name == "Black or African American") %>% 
    select(cases) %>% pull() %>% 
    str_replace(",", "") %>% unlist() %>% as.numeric
  white = demo_table %>% 
    filter(name == "White") %>% 
    select(cases) %>% pull() %>% 
    str_replace(",", "") %>% unlist() %>% as.numeric
  other = demo_table %>% 
    filter(name == "Other") %>% 
    select(cases) %>% pull() %>% 
    str_replace(",", "") %>% unlist() %>% as.numeric
  race_known = demo_table %>% 
    filter(name == "Total with known race1") %>% 
    select(cases) %>% pull() %>% 
    str_replace(",", "") %>% unlist() %>% as.numeric
  
  race_unk = race_known - (nata + asian + afra + white + other)
  
  nata_deaths = demo_table %>% 
    filter(name == "American Indian Alaskan Native") %>% 
    select(deaths) %>% pull() %>% 
    str_replace(",", "") %>% unlist() %>% as.numeric
  asian_deaths = demo_table %>% 
    filter(name == "Asian") %>% 
    select(deaths) %>% pull() %>% 
    str_replace(",", "") %>% unlist() %>% as.numeric
  afra_deaths = demo_table %>% 
    filter(name == "Black or African American") %>% 
    select(deaths) %>% pull() %>% 
    str_replace(",", "") %>% unlist() %>% as.numeric
  white_deaths = demo_table %>% 
    filter(name == "White") %>% 
    select(deaths) %>% pull() %>% 
    str_replace(",", "") %>% unlist() %>% as.numeric
  other_deaths = demo_table %>% 
    filter(name == "Other") %>% 
    select(deaths) %>% pull() %>% 
    str_replace(",", "") %>% unlist() %>% as.numeric
  race_known_deaths = demo_table %>% 
    filter(name == "Total with known race1") %>% 
    select(deaths) %>% pull() %>% as.numeric
  race_unk_deaths = race_known_deaths - 
    (nata_deaths + asian_deaths + afra_deaths + white_deaths + other_deaths)
  
  hispanic = demo_table %>% 
    filter(name == "Hispanic") %>% 
    select(cases) %>% pull() %>% 
    str_replace(",", "") %>% unlist() %>% as.numeric
  non_hispanic = demo_table %>% 
    filter(name == "Non-Hispanic") %>% 
    select(cases) %>% pull() %>% 
    str_replace(",", "") %>% unlist() %>% as.numeric
  eth_known = demo_table %>% 
    filter(name == "Total with known ethnicity2") %>% 
    select(cases) %>% pull() %>% 
    str_replace(",", "") %>% unlist() %>% as.numeric
  eth_unk = eth_known - (hispanic + non_hispanic)
  
  hispanic_deaths = demo_table %>% 
    filter(name == "Hispanic") %>% 
    select(deaths) %>% pull() %>% as.numeric
  non_hispanic_deaths = demo_table %>% 
    filter(name == "Non-Hispanic") %>% 
    select(deaths) %>% pull() %>% as.numeric
  eth_known_deaths = demo_table %>% 
    filter(name == "Total with known ethnicity2") %>% 
    select(deaths) %>% pull() %>% as.numeric
  eth_unk_deaths= eth_known_deaths - (hispanic_deaths + non_hispanic_deaths)
  
  cases = heading_table[1]
  deaths = heading_table[2]
  
  skeleton[["tested"]][["total"]] = heading_table[3]
  
  skeleton[["deaths"]][["total"]] = deaths
  skeleton[["deaths"]][["race_NatA"]] = nata_deaths
  skeleton[["deaths"]][["race_asian"]] = asian_deaths
  skeleton[["deaths"]][["race_AfrA"]] = afra_deaths
  skeleton[["deaths"]][["race_white"]] = white_deaths
  skeleton[["deaths"]][["race_other"]] = other_deaths
  skeleton[["deaths"]][["race_unk"]] = race_unk_deaths
  skeleton[["deaths"]][["ethnicity_hispanic"]] = hispanic_deaths
  skeleton[["deaths"]][["ethnicity_non_hispanic"]] = non_hispanic_deaths
  skeleton[["deaths"]][["ethnicity_unk"]] = eth_unk_deaths
  
  skeleton[["cases"]][["total"]] = cases
  skeleton[["cases"]][["race_NatA"]] = nata
  skeleton[["cases"]][["race_asian"]] = asian
  skeleton[["cases"]][["race_AfrA"]] = afra
  skeleton[["cases"]][["race_white"]] = white
  skeleton[["cases"]][["race_other"]] = other
  skeleton[["cases"]][["race_unk"]] = race_unk
  skeleton[["cases"]][["ethnicity_hispanic"]] = hispanic
  skeleton[["cases"]][["ethnicity_non_hispanic"]] = non_hispanic
  skeleton[["cases"]][["ethnicity_unk"]] = eth_unk
  
  skeleton[["hospitalized"]][["total"]] = heading_table[4]
  
  as_tibble(skeleton) %>% 
    standardize %>% 
    mutate(
      state_name = "North Carolina",
      Link = url,
      platform = "pdf",
      comments = "Age data is only available in images, can't scrape",
      last.updat = Sys.time() %>% as_date) %>% 
    select(
      state_name, Link,
      total.tested:hosp_gender,
      platform, comments, last.updat)
}

get_dc = function() {
  
  skeleton = skeleton_table(default_cols)
  
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
  skeleton[["cases"]][["race_other"]] = today_race_ethn[8] +
    today_race_ethn[9]
  skeleton[["cases"]][["ethnicity_unk"]] = today_race_ethn[12] +
    today_race_ethn[15]
  skeleton[["cases"]][["ethnicity_hispanic"]] = today_race_ethn[13]
  skeleton[["cases"]][["ethnicity_non_hispanic"]] = today_race_ethn[14]
  
  # Logging deaths
  today_deaths = deaths_by_race[, ncol(deaths_by_race)] %>% unlist()
  skeleton[["deaths"]][["race_asian"]] = today_deaths[3]
  skeleton[["deaths"]][["race_AfrA"]] = today_deaths[4]
  skeleton[["deaths"]][["ethnicity_hispanic"]] = today_deaths[5]
  skeleton[["deaths"]][["ethnicity_non_hispanic"]] = today_deaths[6]
  skeleton[["deaths"]][["race_unk"]] = today_deaths[7]
  
  as_tibble(skeleton) %>% 
    standardize %>% 
    mutate(
      state_name = "District of Columbia",
      Link = url,
      platform = "xlsx",
      comments = "Deaths are combined between race and ethnicity",
      last.updat = Sys.time() %>% as_date) %>% 
    select(
      state_name, Link,
      total.tested:hosp_gender,
      platform, comments, last.updat)
}

get_south_carolina = function() {
  url = "https://scdhec.gov/infectious-diseases/viruses/coronavirus-disease-2019-covid-19/sc-testing-data-projections-covid-19"
  html = read_html(url) %>% 
    html_nodes("body") %>% 
    html_nodes("div") %>% .[[2]] %>% 
    html_nodes(".l-container--page") %>% 
    html_nodes("main") %>% 
    html_nodes("article .l-constrain") %>% .[[1]] %>% 
    html_nodes("div section") %>% .[[1]] %>% 
    html_nodes(".l-constrain .resize") %>% .[[1]] %>% 
    html_nodes("table") %>% 
    html_table() %>% .[[1]] %>% 
    pull(X2) %>% 
    str_replace(",", "") %>% 
    as.numeric()
  
  skeleton = skeleton_table(default_cols)
  
  skeleton[["tested"]][["total"]] = html[7]
  skeleton[["negatives"]][["total"]] = html[3]
  skeleton[["cases"]][["total"]] = html[6]
  
  as_tibble(skeleton) %>% 
    standardize %>% 
    mutate(
      state_name = "South Carolina",
      Link = url,
      platform = "pdf",
      comments = "Demographic and age data stored in complicated Tableau viz",
      last.updat = Sys.time() %>% as_date) %>% 
    select(
      state_name, Link,
      total.tested:hosp_gender,
      platform, comments, last.updat)
}

get_new_jersey = function() {
  
  url = "https://www.nj.gov/health/cd/documents/topics/NCOV/COVID_Confirmed_Case_Summary.pdf"
  data = pdf_text(url) %>% .[1] %>% 
    str_split("\n") %>% .[[1]] %>% 
    str_squish()
  skeleton = skeleton_table(nj_cols)
  
  # This pdf has the potential to change a lot in terms 
  # of what appears on what row but this can be easily changed
  cases = data %>% .[9] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 5] %>% 
    str_replace(",", "") %>% .[[1]] %>% as.numeric()
  
  deaths = data %>% .[13] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 1] %>% 
    str_replace(",", "") %>% .[[1]] %>% as.numeric()
  
  hospitalizations = data %>% .[15] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 1] %>% 
    str_replace(",", "") %>% .[[1]] %>% as.numeric()
  
  between_0_and_4 = data %>% .[17] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 4] %>% 
    str_replace(",", "") %>% .[[1]] %>% as.numeric()
  
  between_5_and_17 = data %>% .[18] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 4] %>% 
    str_replace(",", "") %>% .[[1]] %>% as.numeric()
  
  between_18_and_29 = data %>% .[19] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 4] %>% 
    str_replace(",", "") %>% .[[1]] %>% as.numeric()
  
  between_30_and_49 = data %>% .[20] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 4] %>% 
    str_replace(",", "") %>% .[[1]] %>% as.numeric()
  
  between_50_and_64 = data %>% .[21] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 4] %>% 
    str_replace(",", "") %>% .[[1]] %>% as.numeric()
  
  between_65_and_79 = data %>% .[22] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 4] %>% 
    str_replace(",", "") %>% .[[1]] %>% as.numeric()
  
  older_than_80 = data %>% .[23] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 2] %>% 
    str_replace(",", "") %>% .[[1]] %>% as.numeric()
  
  female = data %>% .[28] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 2] %>% 
    str_replace(",", "") %>% .[[1]] %>% as.numeric()
  
  male = data %>% .[29] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 2] %>% 
    str_replace(",", "") %>% .[[1]] %>% as.numeric()
  
  unknown_sex = data %>% .[30] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 2] %>% 
    str_replace(",", "") %>% .[[1]] %>% as.numeric()
  
  white = data %>% .[32] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 3] %>% 
    str_replace(",", "") %>% .[[1]] %>% as.numeric()
  
  hispanic = data %>% .[34] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 1] %>% 
    str_replace(",", "") %>% .[[1]] %>% as.numeric()
  
  black = data %>% .[36] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 3] %>% 
    str_replace(",", "") %>% .[[1]] %>% as.numeric()
  
  asian = data %>% .[37] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 3] %>% 
    str_replace(",", "") %>% .[[1]] %>% as.numeric()
  
  other_race = data %>% .[38] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 3] %>% 
    str_replace(",", "") %>% .[[1]] %>% as.numeric()
  
  non_hispanic = (white + black + asian + other_race)
  
  unknown_ethnicity = cases - (hispanic + non_hispanic)
  
  unknown_race = cases - (non_hispanic)
  
  skeleton[["deaths"]][["total"]] = deaths
  skeleton[["hospitalized"]][["total"]] = hospitalizations
  
  skeleton[["cases"]][["total"]] = cases
  skeleton[["cases"]][["sex_male"]] = male
  skeleton[["cases"]][["sex_female"]] = female
  skeleton[["cases"]][["sex_unk"]] = unknown_sex
  
  skeleton[["cases"]][["age_0_4"]] = between_0_and_4
  skeleton[["cases"]][["age_5_17"]] = between_5_and_17
  skeleton[["cases"]][["age_18_29"]] = between_18_and_29
  skeleton[["cases"]][["age_30_49"]] = between_30_and_49
  skeleton[["cases"]][["age_50_64"]] = between_50_and_64
  skeleton[["cases"]][["age_65_79"]] = between_65_and_79
  skeleton[["cases"]][["age_80+"]] = older_than_80
  
  skeleton[["cases"]][["ethnicity_hispanic"]] = hispanic
  skeleton[["cases"]][["ethnicity_non_hispanic"]] = non_hispanic
  skeleton[["cases"]][["ethnicity_unk"]] = unknown_ethnicity
  
  skeleton[["cases"]][["race_white"]] = white
  skeleton[["cases"]][["race_AfrA"]] = black
  skeleton[["cases"]][["race_asian"]] = asian
  skeleton[["cases"]][["race_other"]] = other_race
  skeleton[["cases"]][["race_unk"]] = unknown_race
  
  as_tibble(skeleton) %>% 
    standardize %>% 
    mutate(
      state_name = "New Jersey",
      Link = url,
      platform = "pdf",
      comments = "Site combined race and ethnicity, so calculation needed",
      last.updat = Sys.time() %>% as_date) %>% 
    select(
      state_name, Link,
      total.tested:hosp_gender,
      platform, comments, last.updat)
  
}

get_new_hampshire = function() {
  
  # URL for getting to weekly reports
  report_url = "https://www.nh.gov/covid19/news/weekly-summary.htm"
  url = read_html(report_url) %>% 
    html_nodes("body #pagecontainer div #bodycontainer main .wide-width") %>% 
    html_nodes("ul li a") %>% 
    html_attr("href") %>% 
    .[str_starts(., "https://www.dhhs.nh.gov/dphs/cdcs/covid19")] %>% 
    .[1] # The first one is the most recent report
  
  first_page = pdf_text(url) %>% .[1] %>% 
    str_split("\n") %>% .[[1]] %>% 
    str_squish()
  second_page = pdf_text(url) %>% .[2] %>% 
    str_split("\n") %>% .[[1]] %>% 
    str_squish()
  skeleton = skeleton_table(nh_cols)
  
  cases = first_page %>% .[7] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 3] %>% 
    str_replace(",", "") %>% as.numeric()
  
  hospitalized = first_page %>% .[7] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 5] %>% 
    str_replace(",", "") %>% as.numeric()
  
  recovered = first_page %>% .[7] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 7] %>% 
    str_replace(",", "") %>% as.numeric()
  
  deaths = first_page %>% .[7] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 8] %>% 
    str_replace(",", "") %>% as.numeric()
  
  males = first_page %>% .[15] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 2] %>% 
    str_replace(",", "") %>% as.numeric()
  
  males_hosp = first_page %>% .[15] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 4] %>% 
    str_replace(",", "") %>% as.numeric()
  
  males_death = first_page %>% .[15] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 6] %>% 
    str_replace(",", "") %>% as.numeric()
  
  females = first_page %>% .[14] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 2] %>% 
    str_replace(",", "") %>% as.numeric()
  
  females_hosp = first_page %>% .[14] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 4] %>% 
    str_replace(",", "") %>% as.numeric()
  
  females_death = first_page %>% .[14] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 6] %>% 
    str_replace(",", "") %>% as.numeric()
  
  between_0_and_9 = first_page %>% .[17] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 2] %>% 
    str_replace(",", "") %>% as.numeric()
  
  between_0_and_9_hosp = first_page %>% .[17] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 4] %>% 
    str_replace(",", "") %>% as.numeric()
  
  between_0_and_9_death = first_page %>% .[17] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 6] %>% 
    str_replace(",", "") %>% as.numeric()
  
  between_10_and_19 = first_page %>% .[18] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 4] %>% 
    str_replace(",", "") %>% as.numeric()
  
  between_10_and_19_hosp = first_page %>% .[18] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 6] %>% 
    str_replace(",", "") %>% as.numeric()
  
  between_10_and_19_death = first_page %>% .[18] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 8] %>% 
    str_replace(",", "") %>% as.numeric()
  
  between_20_and_29 = first_page %>% .[19] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 4] %>% 
    str_replace(",", "") %>% as.numeric()
  
  between_20_and_29_hosp = first_page %>% .[19] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 6] %>% 
    str_replace(",", "") %>% as.numeric()
  
  between_20_and_29_death = first_page %>% .[19] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 8] %>% 
    str_replace(",", "") %>% as.numeric()
  
  between_30_and_39 = first_page %>% .[20] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 4] %>% 
    str_replace(",", "") %>% as.numeric()
  
  between_30_and_39_hosp = first_page %>% .[20] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 6] %>% 
    str_replace(",", "") %>% as.numeric()
  
  between_30_and_39_death = first_page %>% .[20] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 8] %>% 
    str_replace(",", "") %>% as.numeric()
  
  between_40_and_49 = first_page %>% .[21] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 4] %>% 
    str_replace(",", "") %>% as.numeric()
  
  between_40_and_49_hosp = first_page %>% .[21] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 6] %>% 
    str_replace(",", "") %>% as.numeric()
  
  between_40_and_49_death = first_page %>% .[21] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 8] %>% 
    str_replace(",", "") %>% as.numeric()
  
  between_50_and_59 = first_page %>% .[22] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 4] %>% 
    str_replace(",", "") %>% as.numeric()
  
  between_50_and_59_hosp = first_page %>% .[22] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 6] %>% 
    str_replace(",", "") %>% as.numeric()
  
  between_50_and_59_death = first_page %>% .[22] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 8] %>% 
    str_replace(",", "") %>% as.numeric()
  
  between_60_and_69 = first_page %>% .[23] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 4] %>% 
    str_replace(",", "") %>% as.numeric()
  
  between_60_and_69_hosp = first_page %>% .[23] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 6] %>% 
    str_replace(",", "") %>% as.numeric()
  
  between_60_and_69_death = first_page %>% .[23] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 8] %>% 
    str_replace(",", "") %>% as.numeric()
  
  between_70_and_79 = first_page %>% .[24] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 4] %>% 
    str_replace(",", "") %>% as.numeric()
  
  between_70_and_79_hosp = first_page %>% .[24] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 6] %>% 
    str_replace(",", "") %>% as.numeric()
  
  between_70_and_79_death = first_page %>% .[24] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 8] %>% 
    str_replace(",", "") %>% as.numeric()
  
  older_than_80 = first_page %>% .[25] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 3] %>% 
    str_replace(",", "") %>% as.numeric()
  
  older_than_80_hosp = first_page %>% .[25] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 5] %>% 
    str_replace(",", "") %>% as.numeric()
  
  older_than_80_death = first_page %>% .[25] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 7] %>% 
    str_replace(",", "") %>% as.numeric()
  
  has_race_ethn = second_page %>% .[12] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 1] %>% 
    str_replace(",", "") %>% as.numeric()
  
  has_race_ethn_hosp = second_page %>% .[12] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 2] %>% 
    str_replace(",", "") %>% as.numeric()
  
  has_race_ethn_death = second_page %>% .[12] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 3] %>% 
    str_replace(",", "") %>% as.numeric()
  
  white = second_page %>% .[6] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 2] %>% 
    str_replace(",", "") %>% as.numeric()
  
  white_hosp = second_page %>% .[6] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 4] %>% 
    str_replace(",", "") %>% as.numeric()
  
  white_death = second_page %>% .[6] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 6] %>% 
    str_replace(",", "") %>% as.numeric()
  
  black = second_page %>% .[8] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 5] %>% 
    str_replace(",", "") %>% as.numeric()
  
  black_hosp = second_page %>% .[8] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 7] %>% 
    str_replace(",", "") %>% as.numeric()
  
  black_death = second_page %>% .[8] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 9] %>% 
    str_replace(",", "") %>% as.numeric()
  
  other = second_page %>% .[9] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 2] %>% 
    str_replace(",", "") %>% as.numeric()
  
  other_hosp = second_page %>% .[9] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 4] %>% 
    str_replace(",", "") %>% as.numeric()
  
  other_death = second_page %>% .[9] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 6] %>% 
    str_replace(",", "") %>% as.numeric()
  
  asian = second_page %>% .[10] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 2] %>% 
    str_replace(",", "") %>% as.numeric()
  
  asian_hosp = second_page %>% .[10] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 4] %>% 
    str_replace(",", "") %>% as.numeric()
  
  asian_death = second_page %>% .[10] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 6] %>% 
    str_replace(",", "") %>% as.numeric()
  
  hispanic = second_page %>% .[7] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 2] %>% 
    str_replace(",", "") %>% as.numeric()
  
  hispanic_hosp = second_page %>% .[7] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 4] %>% 
    str_replace(",", "") %>% as.numeric()
  
  hispanic_death = second_page %>% .[7] %>% 
    str_split(" ", simplify = TRUE) %>% .[1, 6] %>% 
    str_replace(",", "") %>% as.numeric()
  
  non_hispanic = white + black + other + asian
  non_hispanic_hosp = white_hosp + black_hosp + other_hosp + asian_hosp
  non_hispanic_death = white_death + black_death + other_death + asian_death
  
  skeleton[["cases"]][["total"]] = cases
  skeleton[["cases"]][["sex_male"]] = males
  skeleton[["cases"]][["sex_female"]] = females
  
  skeleton[["cases"]][["age_0_9"]] = between_0_and_9
  skeleton[["cases"]][["age_10_19"]] = between_10_and_19
  skeleton[["cases"]][["age_20_29"]] = between_20_and_29
  skeleton[["cases"]][["age_30_39"]] = between_30_and_39
  skeleton[["cases"]][["age_40_49"]] = between_40_and_49
  skeleton[["cases"]][["age_50_59"]] = between_50_and_59
  skeleton[["cases"]][["age_60_69"]] = between_60_and_69
  skeleton[["cases"]][["age_70_79"]] = between_70_and_79
  skeleton[["cases"]][["age_80+"]] = older_than_80
  
  skeleton[["cases"]][["race_unk"]] = cases - has_race_ethn
  skeleton[["cases"]][["race_white"]] = white
  skeleton[["cases"]][["race_AfrA"]] = black
  skeleton[["cases"]][["race_other"]] = other
  skeleton[["cases"]][["race_asian"]] = asian
  
  skeleton[["cases"]][["ethnicity_hispanic"]] = hispanic
  skeleton[["cases"]][["ethnicity_non_hispanic"]] = non_hispanic
  
  skeleton[["hospitalized"]][["total"]] = hospitalized
  skeleton[["hospitalized"]][["sex_male"]] = males_hosp
  skeleton[["hospitalized"]][["sex_female"]] = females_hosp
  
  skeleton[["hospitalized"]][["age_0_9"]] = between_0_and_9_hosp
  skeleton[["hospitalized"]][["age_10_19"]] = between_10_and_19_hosp
  skeleton[["hospitalized"]][["age_20_29"]] = between_20_and_29_hosp
  skeleton[["hospitalized"]][["age_30_39"]] = between_30_and_39_hosp
  skeleton[["hospitalized"]][["age_40_49"]] = between_40_and_49_hosp
  skeleton[["hospitalized"]][["age_50_59"]] = between_50_and_59_hosp
  skeleton[["hospitalized"]][["age_60_69"]] = between_60_and_69_hosp
  skeleton[["hospitalized"]][["age_70_79"]] = between_70_and_79_hosp
  skeleton[["hospitalized"]][["age_80+"]] = older_than_80_hosp
  
  skeleton[["hospitalized"]][["race_unk"]] = hospitalized - has_race_ethn_hosp
  skeleton[["hospitalized"]][["race_white"]] = white_hosp
  skeleton[["hospitalized"]][["race_AfrA"]] = black_hosp
  skeleton[["hospitalized"]][["race_other"]] = other_hosp
  skeleton[["hospitalized"]][["race_asian"]] = asian_hosp
  
  skeleton[["hospitalized"]][["ethnicity_hispanic"]] = hispanic_hosp
  skeleton[["hospitalized"]][["ethnicity_non_hispanic"]] = non_hispanic_hosp
  
  skeleton[["deaths"]][["total"]] = deaths
  skeleton[["deaths"]][["sex_male"]] = males_death
  skeleton[["deaths"]][["sex_female"]] = females_death
  
  skeleton[["deaths"]][["age_0_9"]] = between_0_and_9_death
  skeleton[["deaths"]][["age_10_19"]] = between_10_and_19_death
  skeleton[["deaths"]][["age_20_29"]] = between_20_and_29_death
  skeleton[["deaths"]][["age_30_39"]] = between_30_and_39_death
  skeleton[["deaths"]][["age_40_49"]] = between_40_and_49_death
  skeleton[["deaths"]][["age_50_59"]] = between_50_and_59_death
  skeleton[["deaths"]][["age_60_69"]] = between_60_and_69_death
  skeleton[["deaths"]][["age_70_79"]] = between_70_and_79_death
  skeleton[["deaths"]][["age_80+"]] = older_than_80_death
  
  skeleton[["deaths"]][["race_unk"]] = deaths - has_race_ethn_death
  skeleton[["deaths"]][["race_white"]] = white_death
  skeleton[["deaths"]][["race_AfrA"]] = black_death
  skeleton[["deaths"]][["race_other"]] = other_death
  skeleton[["deaths"]][["race_asian"]] = asian_death
  
  skeleton[["deaths"]][["ethnicity_hispanic"]] = hispanic_death
  skeleton[["deaths"]][["ethnicity_non_hispanic"]] = non_hispanic_death
  
  skeleton[["recovered"]][["total"]] = recovered
  
  as_tibble(skeleton) %>% 
    standardize %>% 
    mutate(
      state_name = "New Hampshire",
      Link = url,
      platform = "pdf",
      comments = "Race/ethnicity combined",
      last.updat = Sys.time() %>% as_date) %>% 
    select(
      state_name, Link,
      total.tested:hosp_gender,
      platform, comments, last.updat)
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
      last.updat = Sys.time() %>% as_date) %>% 
    select(
      state_name, Link,
      total.tested:hosp_gender,
      platform, comments, last.updat)
}

compile = function(tn_date) {
  
  # tn_date: Must be structured as YEAR-MM-DD ("2020-05-11")
  
  oklahoma = get_oklahoma()
  mississippi = get_mississippi()
  florida = get_florida()
  tennessee = get_tennessee(tn_date)
  north_carolina = get_north_carolina()
  dc = get_dc()
  new_jersey = get_new_jersey()
  south_carolina = get_south_carolina()
  new_hampshire = get_new_hampshire()
  guam = get_guam()
  
  return(bind_rows(
    oklahoma,
    mississippi,
    florida,
    tennessee,
    north_carolina,
    dc,
    new_jersey,
    south_carolina,
    new_hampshire,
    guam
  ))
}