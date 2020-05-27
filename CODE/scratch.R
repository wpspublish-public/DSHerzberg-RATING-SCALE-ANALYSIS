freq_demos_tables <-
  data_input_sim%>%
          select(var_order) %>% ) %>%
  pivot_longer(names_to = 'var', values_to = 'cat') %>%
  count(var, cat) %>%
  arrange(match(var, var_order), match(cat, cat_order)) %>%
  mutate(
    var = case_when(
      lag(var) == "age_range" & var == "age_range" ~ "",
      lag(var) == "gender" & var == "gender" ~ "",
      lag(var) == "educ" & var == "educ" ~ "",
      lag(var) == "ethnic" & var == "ethnic" ~ "",
      lag(var) == "region" & var == "region" ~ "",
      lag(var) == "clin_status" & var == "clin_status" ~ "",
      TRUE ~ var
    )
  )
)
