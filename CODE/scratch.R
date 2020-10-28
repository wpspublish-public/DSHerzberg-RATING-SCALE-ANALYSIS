Child_512_Home_T_sd_512 <-
  Child_512_Home %>% 
  select(contains('_NT')) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname, sd_512 = sd)%>% 
  mutate_at(vars(scale), ~ str_sub(., 1, -4)) %>% 
  arrange(match(scale, scale_order)) %>% 
  select(scale, sd_512)

test7 <- map_df(
  lst(
    data_RS_sim_child_parent,
    data_RS_sim_child_teacher,
    data_RS_sim_teen_parent,
    data_RS_sim_teen_teacher
  ),
  ~
    .x %>%
    select(contains("raw")) %>%
    describe(fast = T) %>%
    rownames_to_column() %>%
    rename(scale_name = rowname) %>%
    mutate(
      form = str_sub(scale_name, 1, 2),
      scale = str_sub(scale_name, 3,-5)
    ) %>%
    select(form, scale, n, sd)
) 
