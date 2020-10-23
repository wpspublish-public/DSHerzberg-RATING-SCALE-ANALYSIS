map_df(scale_order,
       ~
         Child_512_Home %>%
         select(eval(as.name(
           str_c(.x, '_items_Child_512_Home')
         ))) %>%
         assign(str_c(.x, '_item_scores_512'), ., envir = .GlobalEnv))


vec <- c("cpi01", "cpi02", "cpi03")
test3 <- tibble(
  data = list(data_RS_sim_child_parent),
  item_names = list(vec)
) 




test2 <- tibble(data = rep(
  lst(
    data_RS_sim_child_parent,
    data_RS_sim_child_teacher,
    data_RS_sim_teen_parent,
    data_RS_sim_teen_teacher
  ),
  each = 6
),
item_names = flatten(scale_item_vectors)) %>%
  mutate(items = select(data, (!!!rlang::sym(.$item_names))))



%>% 
  mutate(
    items = map2(data, item_names, ~ x. %>% select(!!.y))
  )




temp_list3 <- map2(
  lst(
  data_RS_sim_child_parent,
  data_RS_sim_child_teacher,
  data_RS_sim_teen_parent,
  data_RS_sim_teen_teacher
),
scale_
)

temp_list1 <- lst(
  data_RS_sim_child_parent,
  data_RS_sim_child_teacher,
  data_RS_sim_teen_parent,
  data_RS_sim_teen_teacher
) %>% 
  map( ~
       map(scale_item_vectors, ~ .y %>% select(all_of(eval(as.name(.x)))), .y = .x)) 
  



temp_list2 <- map(scale_item_vectors[1:6],
                  ~
                    data_RS_sim_child_parent %>%
                    select(all_of(.x))
)

temp3 <- tibble(var = map(
  lst(
    data_RS_sim_child_parent,
    data_RS_sim_child_teacher,
    data_RS_sim_teen_parent,
    data_RS_sim_teen_teacher
  ),
  ~
    rep(.x, each = 4)
))
