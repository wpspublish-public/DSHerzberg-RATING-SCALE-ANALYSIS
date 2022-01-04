suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(splitstackshape))
suppressMessages(library(psych))

sample_full <- suppressMessages(read_csv(here("INPUT-FILES/data-RS-sim-child-parent.csv")))

set.seed(1234)
sample_60perc <- stratified(sample_full,
                    c("age", "gender", "educ", "ethnic", "region"),
                    size = .6)

write_csv(sample_60perc, here(
  "INPUT-FILES/data-RS-sim-child-parent-60perc.csv"
))

demos_full <- map(
  c("gender", "educ", "ethnic", "region"),
  ~
    sample_full %>%
    group_by(age,!!sym(.x)) %>%
    summarize(n = n()) %>%
    pivot_wider(names_from = !!sym(.x), values_from = n)
) %>%
  reduce(left_join, by = "age") %>%
  ungroup() %>%
  mutate(
    sample = case_when(row_number() == 1 ~ "full",
                       TRUE ~ NA_character_),
    n = case_when(row_number() == 1 ~ nrow(sample_full),
                  TRUE ~ NA_integer_)
  ) %>%
  relocate(c(sample, n), .before = "age")

demos_60_perc <- map(
  c("gender", "educ", "ethnic", "region"),
  ~
    sample_60perc %>%
    group_by(age,!!sym(.x)) %>%
    summarize(n = n()) %>%
    pivot_wider(names_from = !!sym(.x), values_from = n)
) %>%
  reduce(left_join, by = "age") %>%
  ungroup() %>%
  mutate(
    sample = case_when(row_number() == 1 ~ "60_perc",
                       TRUE ~ NA_character_),
    n = case_when(row_number() == 1 ~ nrow(sample_60perc),
                  TRUE ~ NA_integer_)
  ) %>%
  relocate(c(sample, n), .before = "age")

demos_comp <- bind_rows(demos_full,
                        demos_60_perc)

write_csv(
  demos_comp,
  here(
    "OUTPUT-FILES/TABLES/demos-full-60perc-comp-child-parent.csv"
  ),
  na = ""
)

raw_score_desc_full_sample <- sample_full %>%
  select(contains("raw")) %>%
  describe(fast = TRUE) %>%
  rownames_to_column(var = "scale") %>%
  select(scale, n, mean, sd)

raw_score_desc_60perc_sample <- sample_60perc %>% 
  select(contains("raw")) %>%
  describe(fast = TRUE) %>%
  rownames_to_column(var = "scale") %>%
  select(scale, n, mean, sd)

raw_score_desc_comp <- raw_score_desc_full_sample %>%
  left_join(
    raw_score_desc_60perc_sample,
    by = "scale",
    suffix = c("_full", "_60perc")
  ) %>%
  mutate(ES = abs((mean_full - mean_60perc) /
                    sqrt(((n_full*(sd_full^2)) + (n_60perc*(sd_60perc^2))) / (n_full + n_60perc)))) %>% 
  mutate(across(where(is.numeric), ~ round(., 3)))

write_csv(raw_score_desc_comp, here(
  "OUTPUT-FILES/TABLES/raw-desc-full-60perc-comp-child-parent.csv"
))


