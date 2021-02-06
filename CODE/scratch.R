temp2 <- data_child_parent  %>%
  select(all_of(var_order)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "category") %>%
  group_by(variable, category) %>%
  count(variable, category) %>%
  arrange(match(variable, var_order), match(category, cat_order)) %>%
  ungroup() %>%
  mutate(across(
    variable,
    ~
      case_when(lag(.x) == .x ~ NA_character_,
                T ~ .x)
  ))

