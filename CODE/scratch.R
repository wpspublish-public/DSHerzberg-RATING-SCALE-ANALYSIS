miss_dup <- data_RS_sim_child_parent %>%
  mutate(across(ID,
                ~
                  case_when(
                    row_number() %in% c(10, 35, 579, 991) ~ lag(.x),
                    row_number() %in% c(11) ~ lag(lag(.x)),
                    row_number() %in% c(1, 2, 414, 793) ~ NA_real_,
                    T ~ .x
                  )))
