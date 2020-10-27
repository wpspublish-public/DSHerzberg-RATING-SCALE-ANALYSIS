alpha_512 <- map_df(scale_order, ~
                      alpha(
                        cor(
                          eval(as.name(str_c(.x, '_item_scores_512')))
                        )
                      )[["total"]] %>%
                      mutate(scale = .x) %>% 
                      select(scale, raw_alpha) %>% 
                      rename(alpha_512 = raw_alpha) %>% 
                      assign(str_c(.x, '_alpha_512'), ., envir = .GlobalEnv)
)


test1 <- alpha(cor(scale_item_data$items[[1]]))[["total"]]
s
  

temp1 <- crossing(str_to_upper(form_acronyms), scale_items_suffix) %>% 
  set_names(c("pt1", "pt2")) %>% 
  mutate(scale_names = str_c(pt1, pt2, sep = "_")) %>%
  pull(scale_names)
