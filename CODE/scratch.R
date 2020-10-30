form_scale_cols <-
  crossing(str_to_upper(form_acronyms), scale_items_suffix) %>%
  set_names(c("form", "scale")) 

scale_item_data <- tibble(
  data = rep(input_recode_list,
             each = 6),
  item_names = scale_item_vectors) %>% 
  bind_cols(form_scale_cols) %>% 
  mutate(items = map2(data, item_names, ~ .x %>% select(all_of(.y))))
