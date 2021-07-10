suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(splitstackshape))
suppressMessages(library(psych))

sample_full <- suppressMessages(read_csv(here("INPUT-FILES/data-RS-sim-child-parent.csv")))

set.seed(1234)
sample_60perc <- stratified(sample_full,
                    c("age", "gender", "educ", "ethnic", "region"),
                    size = .6)

# write .csv of 60% sample for use in other procedures
write_csv(sample_60perc, here(
  "INPUT-FILES/data-RS-sim-child-parent-60perc.csv"
))

# Comp demos between full sample and 60% sample
# Prepare table of demographics counts for the full sample. We first call map()
# to return a list of four dfs, with the case counts by age and category for
# each of the four demographic variables. We map over a vector holding the names
# of these four demo vars. The input to this mapping procedure is the full
# standardization sample. We group this input by age and the .x argument to
# map(), that is, the currently iterated demo var. We can then call summarize()
# to get the case counts (n()) for each category of the demo var, within each
# ageyear. At this point the summary table is in nested format, where the
# categories of the demo vars are nested with in each value of age, resulting in
# a long table. We call pivot_wider() to transform the table into a more
# conventional demographic table format, in which there is one row for each age
# year, each category of the demographic variable has its own column, and the
# cells contain the person counts for each crossing of age X demographic
# category.
demos_full <- map(
  c("gender", "educ", "ethnic", "region"),
  ~
    sample_full %>%
    group_by(age, !!sym(.x)) %>%
    summarize(n=n()) %>%
    pivot_wider(names_from = !!sym(.x), values_from = n)
) %>%
  # At this point the data object is list of data frames, all of which have
  # identical values in the age column. We can use purrr:reduce() to iteratively
  # apply left_join(), joining the tables together by the age column. The result
  # is a single df with age column on the far left, and the columns of
  # categories of the four demo vars proceeding to the right, each holding the
  # person counts for each value of age.
  reduce(left_join, by = "age") %>%
  # We ungroup to facilitate a table structure that is more readable. The
  # mutate() call creates sample and n columns that are only filled in the top
  # cell. Note how we get the total sample size for the full standardization
  # sample by calling base::nrow() on that data file.
  ungroup() %>%
  mutate(
    sample = case_when(row_number() == 1 ~ "full",
                       TRUE ~ NA_character_),
    n = case_when(
      row_number() == 1 ~ nrow(sample_full),
      TRUE ~ NA_integer_
    )
  ) %>%
  # relocate provides the desired column sequence in the output table.
  relocate(c(sample, n), .before = "age")

# documentation for code block below is analogous to that for previous (creation
# of demos_full)
demos_60_perc <- map(
  c("gender", "educ", "ethnic", "region"),
  ~
    sample_60perc %>%
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

# Use bind_rows() to stack the tables from the full and 60_perc samples, and
# mutate() the existing sample column to keep it readable, by having the sample
# label only appear in the first row of the stacked table for each sample.
demos_comp <- bind_rows(demos_full,
                        demos_60_perc) %>%
  mutate(across(sample,
                ~ case_when(
                  lag(sample) == sample ~ NA_character_, 
                  TRUE ~ .x
                )))

# write .csv of demos comp
write_csv(
  demos_comp,
  here(
    "OUTPUT-FILES/TABLES/demos-full-60perc-comp-child-parent.csv"
  ),
  na = ""
)

# Comp raw-score descriptives between full sample and 60% sample
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

# write .csv of raw score desc comp
write_csv(raw_score_desc_comp, here(
  "OUTPUT-FILES/TABLES/raw-desc-full-60perc-comp-child-parent.csv"
))


