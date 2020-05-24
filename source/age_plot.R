###########################################################################
###########################################################################
###                                                                     ###
###                              MAIN CODE                              ###
###                                                                     ###
###########################################################################
###########################################################################

age_plot = function(type, state, unit) {
  age_df = df %>%
    filter(strata_type == "age" &
             data_type == type &
             state_name == state) %>%
    mutate(category = factor(category,
                             levels = .$category[order(.$category %>%
                                                         str_extract("[0-9]{1,3}") %>%
                                                         as.numeric)] %>% unique))
  
  if (unit == "raw") {
    if (age_df$metric[1] == "Percent") {
      age_df$count = age_df$count * 100
      ylabel = "Percent"
    }
    else {
      ylabel = "Count"
    }
    print(
      ggplot(age_df) +
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
    )
  }
  
  if (unit == "normalized") {
    print(
      ggplot(age_df[!is.na(age_df$pop_est), ]) +
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
    )
  }
}

############################################################################
############################################################################
###                                                                      ###
###                                 DEMO                                 ###
###                                                                      ###
############################################################################
############################################################################

df = read.csv("../Data/processed_states/processed_state_data_20200522.csv")
# type: test, case, hosp, death
# state: state name
# unit : raw, normalized
age_plot("death", "Illinois", "normalized")
