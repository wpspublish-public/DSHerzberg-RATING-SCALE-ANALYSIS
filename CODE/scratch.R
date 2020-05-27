freq_item_val_tables <- list(mget(str_c("data_input_", data_name_suffix)),
                             item_cols,
                             item_cats
                             ) %>%
  pmap(~ ..1 %>% 
         select(!! ..2) %>% 
         pivot_longer(everything(), names_to = 'var', values_to = 'value') %>% 
         count(var, value) %>% 
         pivot_wider(names_from = value, values_from = n) %>% 
         arrange(match(var, !!..2)) %>%
         select(var, !! ..3) 
  ) %>% 
  # set_names(c("freq_item_val_sim", "freq_item_val_bfi"))
set_names(str_c("freq_item_val_", data_name_suffix))

# trying to reduce my use of `assign()` to place data objecds intop the global
# environment. Let `pmap()` output a list of dfs `freq_item_val_tables`, then
# use `names()` to give each df the desired name
names(freq_item_val_tables) <- c("freq_item_val_sim", "freq_item_val_bfi")

# `list2env()` extracts the data frames from the list into the global environment
list2env(freq_item_val_tables, envir=.GlobalEnv)

setNames(mget(str_c('data_input_', data_name_suffix)), newnamesvector)



test <- 1:3 %>%
  purrr::map(~ rnorm(5, .x)) %>%
  setNames(paste0('V', 1:3))
