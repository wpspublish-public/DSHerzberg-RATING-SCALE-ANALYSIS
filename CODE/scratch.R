match_dist_stand <- ASD_stand_match %>% 
  select(age_range, Gender, ParentHighestEducation, Ethnicity) %>% 
  gather("Variable", "Category")


match_dist_stand1 <- ASD_stand_match %>% 
  select(age_range, Gender, ParentHighestEducation, Ethnicity) %>% 
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Category") %>% 
  group_by(Variable, Category) %>%
  count(Variable, Category) %>%
  arrange(match(Variable, var_order), match(Category, cat_order)) %>% 
  ungroup() %>% 
  mutate(Variable = case_when(
    lag(Variable) == "age_range" & Variable == "age_range" ~ "",
    lag(Variable) == "Gender" & Variable == "Gender" ~ "",
    lag(Variable) == "ParentHighestEducation" & Variable == "ParentHighestEducation" ~ "",
    lag(Variable) == "Ethnicity" & Variable == "Ethnicity" ~ "",
    TRUE ~ Variable
  )) %>% 
  mutate(group = case_when(
    rownames(.) == "1" ~ 'Matched typical',
    T ~ NA_character_
  )) %>% 
  select(group, everything())

