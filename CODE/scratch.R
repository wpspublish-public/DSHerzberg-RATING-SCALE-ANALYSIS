set.seed(1234)
sample_60perc2 <- sample_full %>% 
  # group_by(age, gender, educ, ethnic, region) %>%
  slice_sample(prop = .6) %>% 
  ungroup()

# try splitstackshape::stratified()

library(splitstackshape)

set.seed(1234)
temp1 <- stratified(sample_full,
                    c("age", "gender", "educ", "ethnic", "region"),
                    size = .6)

demos_60_perc_temp1 <- map(
  c("gender", "educ", "ethnic", "region"),
  ~
    temp1 %>%
    group_by(age, !!sym(.x)) %>%
    summarize(n=n()) %>%
    pivot_wider(names_from = !!sym(.x), values_from = n)
) %>%
  reduce(left_join, by = "age") %>%
  ungroup() %>%
  mutate(
    sample = case_when(row_number() == 1 ~ "60_perc",
                       TRUE ~ NA_character_),
    n = case_when(
      row_number() == 1 ~ nrow(sample_60perc),
      TRUE ~ NA_integer_
    )
  ) %>%
  relocate(c(sample, n), .before = "age")
