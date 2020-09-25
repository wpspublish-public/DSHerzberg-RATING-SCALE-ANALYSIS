hist_list <- lst(
  freq_demos_child_parent,
  freq_demos_child_teacher,
  freq_demos_teen_parent,
  freq_demos_teen_teacher
) %>%
  map( ~
         tibble(var = map(var_order, ~ .y %>% filter(var == .x), .y = .x))) %>%
  tibble(file = names(.), data1 = .) %>%
  unnest(cols = c(data1))


temp1 <- 
       tibble(var = map(var_order, ~ freq_demos_child_parent  %>% filter(var == .x)))
