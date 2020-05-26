## Set up
library(tidyverse)
library(RColorBrewer)# Set fixed color
library(plotly)

cols <- brewer.pal(5, "Blues")
var1 = "positive_gender"
var2 = "death_gender"
var3 = "hosp_gender"

total1  = "total_case"
total2 = "total_death"
total3 = "total_hosp"
## data
#covid_demo = read_csv("data/meta_v2_2020-05-13.csv")
covid_demo = read_csv("./data/raw_states/gloria.csv")
census = readxl::read_xlsx("data/census.xlsx")

census_gender = census %>% 
  select(c(name, male, female)) %>% 
  mutate(state_name = name) %>% 
  select(-name)

covid_census = merge(covid_demo, census_gender, 
                     all.x = T,  by = "state_name") %>% 
  as_tibble() %>% 
  janitor::clean_names()

########################## Raw data #######################3####3
##########  raw data cleaning
clean_raw_gender = function(data, var){
  data %>% 
    select(c(state_name, var, male, female,
             total_case, total_death, total_hosp)) %>% 
    ## separte total_case,total_death,total_hosp
    mutate(total_case = map(total_case, 
                            ~str_split(.x, ";")[[1]][1]),
           total_death = map(total_death, 
                             ~str_split(.x, ";")[[1]][1]),
           total_hosp = map(total_hosp, 
                            ~str_split(.x, ";")[[1]][1])) %>%
    unnest(c(total_case,total_death,total_hosp)) %>% 
    ## separate gender single charater to female:a, male:b... 
    mutate(split = map(.[[var]], ~str_split(.x, ";")[[1]])) %>%
    unnest(split) %>% 
    ## separate female and case number into two colmns
    separate(split, c("type", "case"), ":") %>% 
    ## remove leadning and tailing whitespace in string
    mutate_all(str_trim) %>% 
    select(-var) %>% 
    mutate(total_case = as.numeric(total_case),
           total_death = as.numeric(total_death),
           total_hosp = as.numeric(total_hosp)) %>% 
    mutate(type = str_to_lower(type)) %>% 
    mutate(type = str_replace(type,"not.*", "unkown"),
           type = str_replace(type,"miss.*", "unkown"),
           type = str_replace(type,"u.*", "unkown"),
           type = str_replace(type,"pending.*", "pending"),
           ## Colorado	male	0.01%	male to female-other
           type = str_replace(type,"male to female", "other"),
           type = str_replace(type,"neither", "other"),
           type = str_replace(type,"non-binary", "other"),
           type = str_replace(type,"f.*", "female"),
           type = str_replace(type,"m.*", "male")) %>% 
    mutate(type = factor(type, 
                         levels = c("female", "male", "unkown", "other", "pending"))) %>% 
    mutate(percent = str_detect(case, "%"), 
           case  =  parse_number(case)) %>% 
    filter(case != 0) %>% 
    arrange(type) %>%
    mutate(prop = ifelse(percent == FALSE, 
                         case / sum(case) * 100,
                         case)) 
}

##########  raw gender pie visualization
raw_gender_pie = function(data,total, row, column){
  ## add color
  data = data %>% 
    mutate(color = case_when(type == "female" ~ cols[4],
                             type == "male" ~ cols[2],
                             type == "unkown" ~ cols[1],
                             type == "other" ~ cols[3],
                             type == "pending" ~ cols[5],
                             TRUE ~ as.character(type)))
  
  if(nrow(data)>0){
    plot_ly() %>% 
      add_pie(data = data,
              labels = ~str_to_title(type), 
              values = ~round(prop,1),
              sort = F,
              direction = "clockwise",
              textinfo = 'label+percent',
              hoverinfo = 'text',
              text = ~paste0(
                str_to_title(type), ": ",
                round(prop), ' %\n',
                "Raw cases: ",
                ifelse(percent, 
                       round(case*data[[total]]/100), 
                       case)),
              marker = list(colors = data$color,
                            line = list(color = '#FFFFFF', 
                                        width = 1)),
              domain = list(row = row, column = column))  %>% 
      layout(
        grid=list(rows=1, columns=3),
        title = "Raw cases by Gender"
      ) 
  }else{plot_ly() %>% 
      layout(title = "Raw cases by Gender",
             showlegend = T,
             grid=list(rows=1, columns=3),
             xaxis = list(showgrid = FALSE, 
                          zeroline = FALSE, 
                          showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, 
                          zeroline = FALSE, 
                          showticklabels = FALSE)) %>% 
      add_annotations(
        x=3,
        y=2,
        xref = "x",
        yref = "y",
        text = "Information Not Available",
        xanchor = 'center',
        showarrow = F)}
  
}

########################## Normalized data #########################3
##########  normalized data cleaning
clean_norm_gender = function(data, var, total){
  data = data %>% 
    select(c(state_name, var, male, female,
             total_case, total_death, total_hosp)) %>% 
    ## separte total_case,total_death,total_hosp
    mutate(total_case = map(total_case, 
                            ~str_split(.x, ";")[[1]][1]),
           total_death = map(total_death, 
                             ~str_split(.x, ";")[[1]][1]),
           total_hosp = map(total_hosp, 
                            ~str_split(.x, ";")[[1]][1])) %>%
    unnest(c(total_case,total_death,total_hosp)) %>% 
    ## separate gender single charater to female:a, male:b... 
    mutate(split = map(.[[var]], ~str_split(.x, ";")[[1]])) %>%
    unnest(split) %>% 
    ## separate female and case number into two colmns
    separate(split, c("type", "case"), ":") %>% 
    ## remove leadning and tailing whitespace in string
    mutate_all(str_trim) %>% 
    select(-var) %>% 
    mutate(total_case = as.numeric(total_case),
           total_death = as.numeric(total_death),
           total_hosp = as.numeric(total_hosp)) 
  data %>% 
    mutate(type = str_to_lower(type)) %>% 
    mutate(type = str_replace(type,"not.*", "unkown"),
           type = str_replace(type,"miss.*", "unkown"),
           type = str_replace(type,"u.*", "unkown"),
           type = str_replace(type,"pending.*", "pending"),
           ## Colorado	male	0.01%	male to female-other
           type = str_replace(type,"male to female", "other"),
           type = str_replace(type,"neither", "other"),
           type = str_replace(type,"non-binary", "other"),
           type = str_replace(type,"f.*", "female"),
           type = str_replace(type,"m.*", "male")) %>% 
    mutate(type = factor(type, 
                         levels = c("female", "male", "unkown", "other", "pending"))) %>% 
    filter(type %in% c("female", "male")) %>% 
    pivot_longer(male:female,
                 names_to = "gender",
                 values_to = "pop") %>% 
    filter(gender == type) %>% 
    select(-gender) %>% 
    mutate(percent = str_detect(case, "%"), 
           case  =  parse_number(case),
           pop = as.numeric(pop)) %>% 
    filter(case != 0) %>% 
    arrange(type) %>%
    mutate(prop = ifelse(percent == FALSE, 
                         # normalized to 100,000 persons
                         case / pop * 100000,
                         # percentage, %*total_case/total_death/total_hosp
                         case*data[[total]]*1000/pop))
  
}


##########  raw gender pie visualization
norm_bar_gender = function(data,var,total){
  ## add color
  data = data %>% 
    mutate(color = case_when(type == "female" ~ cols[4],
                             type == "male" ~ cols[2],
                             type == "unkown" ~ cols[1],
                             type == "other" ~ cols[3],
                             type == "pending" ~ cols[5],
                             TRUE ~ as.character(type)))
  
  if(nrow(data)>0){data %>% 
      mutate(type = str_to_title(type)) %>% 
      plot_ly(
        x = ~type, y = ~prop, 
        type = 'bar', 
        name = ' ',
        text = ~round(prop), 
        textposition = 'auto',
        hovertemplate = ~paste0(
          str_to_title(type), " :\n",
          "Normalized cases: ", 
          round(prop),  "/100,000 persons \n",
          "Raw cases: ", ifelse(percent,
                                round(case*data[[total]]/100),
                                case)),
        marker = list(color = data$color,
                      line = list(color = '#FFFFFF', 
                                  width = 1))) %>% 
      layout(margin = list(l = 100, r = 20, t = 70, b = 70),
             title = "Normalized cases by Gender",
             yaxis = list(title = 'Cases/100,000 persons'),
             xaxis = list(title = ''),
             showlegend = F)
  }else{plot_ly() %>%
      layout(title = "Normalized cases by Gender",
             xaxis = list(showgrid = FALSE, 
                          zeroline = FALSE, 
                          showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, 
                          zeroline = FALSE, 
                          showticklabels = FALSE)) %>% 
      add_annotations(
        x=3,
        y=2,
        xref = "x",
        yref = "y",
        text = "Information Not Available",
        xanchor = 'center',
        showarrow = F
      )}
}

present_text = function(data){
  if(nrow(data)>0){
    data_present = data %>%
      select(c(state_name, type, prop)) %>% 
      mutate(type = fct_collapse(type, 
                                 present = c("female","male"),
                                 no_present = c("other", "unkown", "pending"))) %>% 
      group_by(type) %>% 
      mutate(sum_prop = sum(prop)) %>% 
      ungroup() %>% 
      select(-prop) %>% 
      distinct() %>% 
      pivot_wider(names_from = type,
                  values_from = sum_prop) %>% 
      mutate(no_present = ifelse(round(present,-1) == 100, 
                                 0, no_present))
    
    data_present %>% 
      plot_ly() %>%
      layout(showlegend = FALSE,
             xaxis = list(showgrid = FALSE, 
                          zeroline = FALSE, 
                          showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, 
                          zeroline = FALSE, 
                          showticklabels = FALSE)) %>% 
      add_annotations(
        x=3,
        y=2,
        xref = "x",
        yref = "y",
        text = ~paste0(
          ifelse(round(data_present$present)<1,
                 print("< 1"),
                 round(data_present$present)), '% ',
          "Data presented"),
        xanchor = 'center',
        showarrow = F
      )
    
  }else{plot_ly() %>%
      layout(showlegend = FALSE,
             xaxis = list(showgrid = FALSE, 
                          zeroline = FALSE, 
                          showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, 
                          zeroline = FALSE, 
                          showticklabels = FALSE))}
  
}


