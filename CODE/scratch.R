map_df(scale_order,
       ~
         Child_512_Home %>%
         select(eval(as.name(
           str_c(.x, '_items_Child_512_Home')
         ))) %>%
         assign(str_c(.x, '_item_scores_512'), ., envir = .GlobalEnv))


temp_list1 <- lst(
  data_RS_sim_child_parent,
  data_RS_sim_child_teacher,
  data_RS_sim_teen_parent,
  data_RS_sim_teen_teacher
) %>% 
  map( ~
       map(scale_item_vectors, ~ .y %>% select(all_of(.x)), .y = .x)) 
  
temp_list2 <- map(scale_item_vectors[1:6],
                  ~
                    data_RS_sim_child_parent %>%
                    select(all_of(.x))
)
