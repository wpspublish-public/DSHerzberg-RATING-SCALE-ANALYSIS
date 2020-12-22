# Repeat above for subscale raw-to-T columns.
subscale_lookup <- map(
  # for this mapping we need all subscale suffixes except TOT. Because we set up
  # the scale_suffix vector with "TOT" in the last position, we can drop it from
  # the vector with the following code, where we get the last position in the
  # vector with base::length()
  scale_suffix[-length(scale_suffix)], 
  ~ get(str_c("data", age_range_name, form_name, "nt", sep = "_")) %>% 
    group_by(!!sym(str_c(scale_prefix, .x, "_raw"))
  ) %>% 
    summarise(
      !!sym(str_c(scale_prefix, .x, "_nt")) := min(!!sym(str_c(scale_prefix, .x, "_nt"))
      )) %>% 
    complete(
      !!sym(str_c(scale_prefix, .x, "_raw")) := all_raw_range
    ) %>% 
    fill(
      !!sym(str_c(scale_prefix, .x, "_nt")) 
    ) %>% 
    fill(
      !!sym(str_c(scale_prefix, .x, "_nt")), 
      .direction = "up"
    ) %>% 
    rename(
      raw = !!sym(str_c(scale_prefix, .x, "_raw"))
    ) %>% 
    mutate(
      across(!!sym(str_c(scale_prefix, .x, "_nt")), 
             ~ case_when(
        raw > subscale_raw_upper_bound ~ NA_integer_,
        TRUE ~ .x
      )
    )
)) %>% 
  reduce(
    left_join, 
    by = 'raw'
  )
