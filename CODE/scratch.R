# write raw score descriptives for all scales (using psych::describe)
assign(
  str_c("raw_score_desc", age_range_name, form_name, sep = "_"),
  data_child_parent %>%
    select(contains('raw')) %>%
    describe(fast = T) %>%
    rownames_to_column() %>%
    rename(scale = rowname) %>%
    select(scale, n, mean, sd) %>%
    mutate(across(c(mean, sd), ~ (round(
      ., 2
    ))))
)

# write T-scores per case table to .csv
write_csv(get(str_c(
  "raw_score_desc", age_range_name, form_name, sep = "_"
)),
here(str_c(
  "OUTPUT-FILES/TABLES/",
  str_c("raw-score-desc",
        age_range_name,
        form_name,
        sep = "-"),
  ".csv"
)),
na = '')
