list(mget(str_c("data_RS_sim_", data_name_suffix)),
     item_cols,
     item_cats,
     data_name_suffix) %>%
  pmap(
    ~ ..1 %>%
      select(!!..2) %>%
      pivot_longer(everything(), names_to = 'item', values_to = 'value') %>%
      count(item, value) %>%
      pivot_wider(names_from = value, values_from = n) %>%
      arrange(match(item, !!..2)) %>%
      mutate(data = case_when(rownames(.) == "1" ~ ..4,
                              T ~ NA_character_)) %>%
      select(data, item, !!..3)
  )

temp1 <- data_RS_sim_child_parent %>%
  select(!!item_cols[[1]]) %>%
  pivot_longer(everything(), names_to = 'item', values_to = 'value') %>%
  count(item, value) %>%
  pivot_wider(names_from = value, values_from = n) %>%
  arrange(match(item, !!..2)) %>%
  mutate(data = case_when(rownames(.) == "1" ~ ..4,
                          T ~ NA_character_)) %>%
  select(data, item, !!..3)
