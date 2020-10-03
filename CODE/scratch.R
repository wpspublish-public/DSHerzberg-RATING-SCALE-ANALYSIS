temp1 <- data_RS_sim_child_parent %>%
      select(contains('raw')) %>%
      describe(fast = T) %>%
      rownames_to_column() %>%
      rename(scale = rowname) %>%
      mutate(
        data = case_when(rownames(.) == "1" ~ ..2,
                         T ~ NA_character_),
        # data = ..2,
        across(c(mean, sd),
               ~ round(., 2))
      ) %>%
      select(data, scale, n, mean, sd)
  ) %>%
  set_names(str_c("raw_desc_", data_name_suffix)) %>%
  list2env(envir = .GlobalEnv)
