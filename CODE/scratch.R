ASD_matchStand_t_desc1 <-
  ASD_stand_match %>% 
  select(contains('_NT')) %>% 
  rename_with(~ str_sub(., 1, -4), everything()) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(sample = case_when(
    rownames(.) == "1" ~ 'Matched typical',
    TRUE ~ NA_character_
  ),
  var = sd^2) %>% 
  select(sample, scale, n, mean, sd, var) %>% 
  rename(n_typ = n,
         mean_typ = mean,
         sd_typ = sd,
         var_typ = var)
