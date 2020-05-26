###########################################################################
###########################################################################
###                                                                     ###
###                              MAIN CODE                              ###
###                                                                     ###
###########################################################################
###########################################################################

race_plot = function(type, state, unit) {
  race_df = df %>%
    filter(strata_type == "race" &
             data_type == type &
             state_name == state) %>%
    mutate(category = factor(category,
                             levels =c("WHITE", "NH WHITE",
                                       "BLACK", "NH BLACK",
                                       "ASIAN", "NH ASIAN", "ASIAN/PI", "NH ASIAN/PI",
                                       "AI/AN", "NH AI/AN", "AI",
                                       "NH/PI", "NH NH/PI", "NH", "PI",
                                       "AI/AN/NH/PI", 
                                       "MULTI", "NH MULTI", "MULTI/OTHERS",
                                       "OTHER", "NH OTHER", 
                                       "HISPANIC",
                                       "UNKNOWN", "PENDING")))
  
  if (unit == "raw") {
    if (race_df$metric[1] == "Percent") {
      race_df$count = race_df$count * 100
      ylabel = "Percent"
    }
    else {
      ylabel = "Count"
    }
    p = ggplot(race_df) +
      geom_bar(aes(x = category,
                   y = count), stat = "identity") +
      labs(title = state,
           x = "Category",
           y = ylabel) +
      theme_bw() +
      theme(
        text = element_text(size = 5),
        axis.text.x = element_text(angle = 30, hjust = 1)
      )
    print(ggplotly(p))
  }
  
  if (unit == "normalized") {
    p = ggplot(race_df[!is.na(race_df$pop_est), ]) +
      geom_bar(aes(x = category,
                   y = normalized), stat = "identity") +
      labs(title = state,
           x = "Category",
           y = "Per 100,000") +
      theme_bw() +
      theme(
        text = element_text(size = 5),
        axis.text.x = element_text(angle = 30, hjust = 1)
      )
    ggplotly(p)
  }
}

############################################################################
############################################################################
###                                                                      ###
###                                 DEMO                                 ###
###                                                                      ###
############################################################################
############################################################################

df = read.csv("../Data/processed_states/processed_state_data_20200524.csv")
# type: test, case, hosp, death
# state: state name
# unit : raw, normalized
race_plot("death", "New York", "normalized")
