map_df(scale_order,
       ~
         Child_512_Home %>%
         select(eval(as.name(
           str_c(.x, '_items_Child_512_Home')
         ))) %>%
         assign(str_c(.x, '_item_scores_512'), ., envir = .GlobalEnv))

# The next code block puts the four input datasets into a `data` list-column,
# repeating each 6 times to set up a df with 24 rows. It then puts the 24
# vectors containing the scale item names into an `item_names` list-column. It
# then creates a third list column, `items`, to hold data frames containing only
# the item columns corresponding to each scale. It does this by mapping select()
# onto the `data` and 'item_names` cols.
test1 <- tibble(data = rep(
  lst(
    data_RS_sim_child_parent,
    data_RS_sim_child_teacher,
    data_RS_sim_teen_parent,
    data_RS_sim_teen_teacher
  ),
  each = 6
),
item_names = flatten(scale_item_vectors)) %>%
  mutate(items = map2(data, item_names, ~ .x %>% select(all_of(.y))))

