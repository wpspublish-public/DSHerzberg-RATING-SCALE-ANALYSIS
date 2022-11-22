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



  mutate(ES = abs((mean_typ - mean_clin) / sqrt(((n_typ * var_typ) + (n_clin * var_clin)) / (n_typ + n_clin))),
         
         
         
 sqrt(((n_typ * var_typ) + (n_clin * var_clin)) / (n_typ + n_clin))
         

 
 ASD_match_t_desc <- left_join(ASD_matchStand_t_desc,
                               ASD_clin_t_desc, by = "scale") %>%
   mutate(ES = abs((mean_typ - mean_clin) / sqrt(((n_typ * var_typ) + (n_clin * var_clin)) / (n_typ + n_clin))),
          form_dx = case_when(
            row.names(.) == "1" ~ "Home-ASD"
          ),
          across(c(mean_typ, sd_typ, mean_clin, sd_clin, ES), ~
                   (round(., 2)))) %>%
   select(form_dx, everything(), -sample.x, -sample.y, -var_typ, -var_clin)
 
