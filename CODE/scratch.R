ASD_clin_stand_preMatch <- ASD_clin_stand_preMatch %>%
  replace_na(
    list(
      IDNumber = 999,
      AgeInMonths = 999,
      Age = 999,
      clin_dx = "999",
      Region = "999"
    )) %>% 
  mutate(HighestEducation = replace_na(as.character(HighestEducation), "999"))
