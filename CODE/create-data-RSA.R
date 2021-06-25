suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(psych))

# SIMULATED RATING SCALE DATA: CHILD 5-12 YO, PARENT REPORT

set.seed(123)

data_RS_sim_child_parent <-
  as_tibble(sim.poly.ideal(nvar = 50, n = 1000, cat = 4,)[["items"]]) %>%
  rename_with(~ str_c("cpi", str_pad(
    as.character(1:50), 2, side = "left", pad = "0"
  ))) %>%
  mutate((across(everything(),
                 ~
                   .x + 1))) %>%
  mutate(
    CPS1_raw = rowSums(.[str_c("cpi", str_pad(as.character(seq(1, 50, by = 5)), 2, side = "left", pad = "0"))]),
    CPS2_raw = rowSums(.[str_c("cpi", str_pad(as.character(seq(2, 50, by = 5)), 2, side = "left", pad = "0"))]),
    CPS3_raw = rowSums(.[str_c("cpi", str_pad(as.character(seq(3, 50, by = 5)), 2, side = "left", pad = "0"))]),
    CPS4_raw = rowSums(.[str_c("cpi", str_pad(as.character(seq(4, 50, by = 5)), 2, side = "left", pad = "0"))]),
    CPS5_raw = rowSums(.[str_c("cpi", str_pad(as.character(seq(5, 50, by = 5)), 2, side = "left", pad = "0"))]),
    CPTOT_raw = rowSums(.[str_c("cpi", str_pad(as.character(1:50), 2, side = "left", pad = "0"))]),
    across(
      contains("cpi"),
      ~ case_when(
        .x == 1 ~ "never",
        .x == 2 ~ "occasionally",
        .x == 3 ~ "frequently",
        .x == 4 ~ "always"
      )
    ),
    ID = 100001:101000,
    age = sample(c(5:12), 1000, replace = TRUE),
    age_range = case_when(age <= 8 ~ "5 to 8 yo",
                          T ~ "9 to 12 yo"),
    gender = sample(
      c("female", "male"),
      1000,
      replace = TRUE,
      prob = c(0.53, 0.47)
    ),
    educ = sample(
      c("no_HS", "HS_grad", "some_college", "BA_plus"),
      1000,
      replace = TRUE,
      prob = c(0.119, 0.263, 0.306, 0.311)
    ),
    ethnic = sample(
      c("hispanic", "asian", "black", "white", "other"),
      1000,
      replace = TRUE,
      prob = c(0.239, 0.048, 0.136, 0.521, .056)
    ),
    region = sample(
      c("northeast", "south", "midwest", "west"),
      1000,
      replace = TRUE,
      prob = c(0.166, 0.383, 0.212, 0.238)
    ),
    clin_status = sample(
      c("typ", "clin"),
      1000,
      replace = TRUE,
      prob = c(0.8, 0.2)
    )
  ) %>%
  select(ID:clin_status, CPS1_raw:CPTOT_raw, cpi01:cpi50)

write_csv(data_RS_sim_child_parent,
          here("INPUT-FILES/data-RS-sim-child-parent.csv"),
          na = "")

# SIMULATED RATING SCALE DATA: CHILD 5-12 YO, TEACHER REPORT

set.seed(456)

data_RS_sim_child_teacher <-
  as_tibble(sim.poly.ideal(nvar = 50, n = 1000, cat = 4,)[["items"]]) %>%
  rename_with(~ str_c("cti", str_pad(
    as.character(1:50), 2, side = "left", pad = "0"
  ))) %>%
  mutate((across(everything(),
                 ~
                   .x + 1))) %>%
  mutate(
    CTS1_raw = rowSums(.[str_c("cti", str_pad(as.character(seq(1, 50, by = 5)), 2, side = "left", pad = "0"))]),
    CTS2_raw = rowSums(.[str_c("cti", str_pad(as.character(seq(2, 50, by = 5)), 2, side = "left", pad = "0"))]),
    CTS3_raw = rowSums(.[str_c("cti", str_pad(as.character(seq(3, 50, by = 5)), 2, side = "left", pad = "0"))]),
    CTS4_raw = rowSums(.[str_c("cti", str_pad(as.character(seq(4, 50, by = 5)), 2, side = "left", pad = "0"))]),
    CTS5_raw = rowSums(.[str_c("cti", str_pad(as.character(seq(5, 50, by = 5)), 2, side = "left", pad = "0"))]),
    CTTOT_raw = rowSums(.[str_c("cti", str_pad(as.character(1:50), 2, side = "left", pad = "0"))]),
    across(
      contains("cti"),
      ~ case_when(
        .x == 1 ~ "never",
        .x == 2 ~ "occasionally",
        .x == 3 ~ "frequently",
        .x == 4 ~ "always"
      )
    ),
    ID = 100001:101000,
    age = sample(c(5:12), 1000, replace = TRUE),
    age_range = case_when(age <= 8 ~ "5 to 8 yo",
                          T ~ "9 to 12 yo"),
    gender = sample(
      c("female", "male"),
      1000,
      replace = TRUE,
      prob = c(0.53, 0.47)
    ),
    educ = sample(
      c("no_HS", "HS_grad", "some_college", "BA_plus"),
      1000,
      replace = TRUE,
      prob = c(0.119, 0.263, 0.306, 0.311)
    ),
    ethnic = sample(
      c("hispanic", "asian", "black", "white", "other"),
      1000,
      replace = TRUE,
      prob = c(0.239, 0.048, 0.136, 0.521, .056)
    ),
    region = sample(
      c("northeast", "south", "midwest", "west"),
      1000,
      replace = TRUE,
      prob = c(0.166, 0.383, 0.212, 0.238)
    ),
    clin_status = sample(
      c("typ", "clin"),
      1000,
      replace = TRUE,
      prob = c(0.8, 0.2)
    )
  ) %>%
  select(ID:clin_status, CTS1_raw:CTTOT_raw, cti01:cti50)

write_csv(data_RS_sim_child_teacher,
          here("INPUT-FILES/data-RS-sim-child-teacher.csv"),
          na = "")

# SIMULATED RATING SCALE DATA: TEEN 13-18 YO, PARENT REPORT

set.seed(789)

data_RS_sim_teen_parent <-
  as_tibble(sim.poly.ideal(nvar = 50, n = 1000, cat = 4,)[["items"]]) %>%
  rename_with(~ str_c("tpi", str_pad(
    as.character(1:50), 2, side = "left", pad = "0"
  ))) %>%
  mutate((across(everything(),
                 ~
                   .x + 1))) %>%
  mutate(
    TPS1_raw = rowSums(.[str_c("tpi", str_pad(as.character(seq(1, 50, by = 5)), 2, side = "left", pad = "0"))]),
    TPS2_raw = rowSums(.[str_c("tpi", str_pad(as.character(seq(2, 50, by = 5)), 2, side = "left", pad = "0"))]),
    TPS3_raw = rowSums(.[str_c("tpi", str_pad(as.character(seq(3, 50, by = 5)), 2, side = "left", pad = "0"))]),
    TPS4_raw = rowSums(.[str_c("tpi", str_pad(as.character(seq(4, 50, by = 5)), 2, side = "left", pad = "0"))]),
    TPS5_raw = rowSums(.[str_c("tpi", str_pad(as.character(seq(5, 50, by = 5)), 2, side = "left", pad = "0"))]),
    TPTOT_raw = rowSums(.[str_c("tpi", str_pad(as.character(1:50), 2, side = "left", pad = "0"))]),
    across(
      contains("tpi"),
      ~ case_when(
        .x == 1 ~ "never",
        .x == 2 ~ "occasionally",
        .x == 3 ~ "frequently",
        .x == 4 ~ "always"
      )
    ),
    ID = 150001:151000,
    age = sample(c(13:18), 1000, replace = TRUE),
    age_range = case_when(age <= 15 ~ "13 to 15 yo",
                          T ~ "16 to 18 yo"),
    gender = sample(
      c("female", "male"),
      1000,
      replace = TRUE,
      prob = c(0.53, 0.47)
    ),
    educ = sample(
      c("no_HS", "HS_grad", "some_college", "BA_plus"),
      1000,
      replace = TRUE,
      prob = c(0.119, 0.263, 0.306, 0.311)
    ),
    ethnic = sample(
      c("hispanic", "asian", "black", "white", "other"),
      1000,
      replace = TRUE,
      prob = c(0.239, 0.048, 0.136, 0.521, .056)
    ),
    region = sample(
      c("northeast", "south", "midwest", "west"),
      1000,
      replace = TRUE,
      prob = c(0.166, 0.383, 0.212, 0.238)
    ),
    clin_status = sample(
      c("typ", "clin"),
      1000,
      replace = TRUE,
      prob = c(0.8, 0.2)
    )
  ) %>%
  select(ID:clin_status, TPS1_raw:TPTOT_raw, tpi01:tpi50)

write_csv(data_RS_sim_teen_parent,
          here("INPUT-FILES/data-RS-sim-teen-parent.csv"),
          na = "")

# SIMULATED RATING SCALE DATA: TEEN 13-18 YO, TEACHER REPORT

set.seed(101112)

data_RS_sim_teen_teacher <-
  as_tibble(sim.poly.ideal(nvar = 50, n = 1000, cat = 4,)[["items"]]) %>%
  rename_with(~ str_c("tti", str_pad(
    as.character(1:50), 2, side = "left", pad = "0"
  ))) %>%
  mutate((across(everything(),
                 ~
                   .x + 1))) %>%
  mutate(
    TTS1_raw = rowSums(.[str_c("tti", str_pad(as.character(seq(1, 50, by = 5)), 2, side = "left", pad = "0"))]),
    TTS2_raw = rowSums(.[str_c("tti", str_pad(as.character(seq(2, 50, by = 5)), 2, side = "left", pad = "0"))]),
    TTS3_raw = rowSums(.[str_c("tti", str_pad(as.character(seq(3, 50, by = 5)), 2, side = "left", pad = "0"))]),
    TTS4_raw = rowSums(.[str_c("tti", str_pad(as.character(seq(4, 50, by = 5)), 2, side = "left", pad = "0"))]),
    TTS5_raw = rowSums(.[str_c("tti", str_pad(as.character(seq(5, 50, by = 5)), 2, side = "left", pad = "0"))]),
    TTTOT_raw = rowSums(.[str_c("tti", str_pad(as.character(1:50), 2, side = "left", pad = "0"))]),
    across(
      contains("tti"),
      ~ case_when(
        .x == 1 ~ "never",
        .x == 2 ~ "occasionally",
        .x == 3 ~ "frequently",
        .x == 4 ~ "always"
      )
    ),
    ID = 150001:151000,
    age = sample(c(13:18), 1000, replace = TRUE),
    age_range = case_when(age <= 15 ~ "13 to 15 yo",
                          T ~ "16 to 18 yo"),
    gender = sample(
      c("female", "male"),
      1000,
      replace = TRUE,
      prob = c(0.53, 0.47)
    ),
    educ = sample(
      c("no_HS", "HS_grad", "some_college", "BA_plus"),
      1000,
      replace = TRUE,
      prob = c(0.119, 0.263, 0.306, 0.311)
    ),
    ethnic = sample(
      c("hispanic", "asian", "black", "white", "other"),
      1000,
      replace = TRUE,
      prob = c(0.239, 0.048, 0.136, 0.521, .056)
    ),
    region = sample(
      c("northeast", "south", "midwest", "west"),
      1000,
      replace = TRUE,
      prob = c(0.166, 0.383, 0.212, 0.238)
    ),
    clin_status = sample(
      c("typ", "clin"),
      1000,
      replace = TRUE,
      prob = c(0.8, 0.2)
    )
  ) %>%
  select(ID:clin_status, TTS1_raw:TTTOT_raw, tti01:tti50)

write_csv(data_RS_sim_teen_teacher,
          here("INPUT-FILES/data-RS-sim-teen-teacher.csv"),
          na = "")

# REAL BFI DATA FROM PSYCH PACKAGE

set.seed(123)

data_input_bfi <- bfi %>%
  drop_na() %>%
  sample_n(1000) %>%
  mutate(
    AGR_raw = rowSums(.[str_c("A", 1:5)]),
    CON_raw = rowSums(.[str_c("C", 1:5)]),
    EXT_raw = rowSums(.[str_c("E", 1:5)]),
    NEU_raw = rowSums(.[str_c("N", 1:5)]),
    OPE_raw = rowSums(.[str_c("O", 1:5)]),
    across(
      c(A1:O5),
      ~
        case_when(
          .x == 1 ~ "very_inaccurate",
          .x == 2 ~ "moderately_inaccurate",
          .x == 3 ~ "slightly_inaccurate",
          .x == 4 ~ "slightly_accurate",
          .x == 5 ~ "moderately_accurate",
          .x == 6 ~ "very_accurate",
        )
    ),
    ID = 200001:201000,
    age_range = case_when(
      age <= 18 ~ "18 yo or younger",
      between(age, 19, 24) ~ "19 to 24 yo",
      between(age, 25, 39) ~ "25 to 39 yo",
      T ~ "40 yo or older"
    ),
    gender = case_when(gender == 1 ~ "male",
                       gender == 2 ~ "female"),
    educ = case_when(
      education == 1 ~ "no_HS",
      education == 2 ~ "HS_grad",
      education == 3 ~ "some_college",
      T ~ "BA_plus"
    ),
    ethnic = sample(
      c("hispanic", "asian", "black", "white", "other"),
      1000,
      replace = TRUE,
      prob = c(0.239, 0.048, 0.136, 0.521, .056)
    ),
    region = sample(
      c("northeast", "south", "midwest", "west"),
      1000,
      replace = TRUE,
      prob = c(0.166, 0.383, 0.212, 0.238)
    ),
    clin_status = sample(
      c("typ", "clin"),
      1000,
      replace = TRUE,
      prob = c(0.8, 0.2)
    )
  ) %>%
  select(ID, age, age_range, gender:clin_status, AGR_raw:OPE_raw, A1:O5)

write_csv(data_input_bfi,
          here("INPUT-FILES/data-input-bfi.csv"),
          na = "")

# METHOD TO CREATE A FRACTIONAL SAMPLE WITH SAME DISTRIBUTION OF DEMOGRAPHIC
# VARIABLES

# use dplyr::sample_frac() to get a 60% sample. By grouping on all demo vars, we
# roughly preserve demographic proportions in smaller sample.
set.seed(1234)
sample_60perc <- data_RS_sim_child_parent %>% 
  group_by(age, gender, educ, ethnic, region) %>%
  sample_frac(0.6)

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
    
    # START HERE ####################
    
    
    Child_512_Home_desamp %>%
    group_by(Age, !!sym(.x)) %>%
    summarize(n=n()) %>%
    pivot_wider(names_from = !!sym(.x), values_from = n)
) %>%
  # At this point the data object is list of data frames, all of which have
  # identical values in the age column. We can use purrr:reduce() to iteratively
  # apply left_join(), joining the tables together by the age column. The result
  # is a single df with age column on the far left, and the columns of
  # categories of the four demo vars proceeding to the right, each holding the
  # person counts for each value of age.
  reduce(left_join, by = "Age") %>%
  # We ungroup to facilitate a table structure that is more readdable. The
  # mutate() call creates sample and n columns that are only filled in the top
  # cell.
  ungroup() %>%
  mutate(
    sample = case_when(row_number() == 1 ~ "full",
                       TRUE ~ NA_character_),
    n = case_when(
      row_number() == 1 ~ nrow(Child_512_Home_desamp),
      TRUE ~ NA_integer_
    )
  ) %>%
  # relocate provides the desired column sequence in the output table.
  relocate(c(sample, n), .before = "Age")

# documentation for code block below is analogous to that for previous (creation
# of demos_full)
demos_60_perc <- map(
  c("Gender", "ParentHighestEducation", "Ethnicity", "Region"),
  ~
    sample_60perc %>%
    group_by(Age, !!sym(.x)) %>%
    summarize(n=n()) %>%
    pivot_wider(names_from = !!sym(.x), values_from = n)
) %>%
  reduce(left_join, by = "Age") %>%
  ungroup() %>%
  mutate(
    sample = case_when(row_number() == 1 ~ "60_perc",
                       TRUE ~ NA_character_),
    n = case_when(
      row_number() == 1 ~ nrow(sample_60perc),
      TRUE ~ NA_integer_
    )
  ) %>%
  relocate(c(sample, n), .before = "Age")

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
    "OUTPUT-FILES/CHILD/COMP-60PERC-SAMPLE/Child-512-Home-demos-full-60perc-comp.csv"
  ),
  na = ""
)

# Comp raw-score descriptives between full sample and 60% sample
T_per_case_full_sample <- read_csv(
  here(
    "OUTPUT-FILES/NORMS-OUTPUT-4080T/Child-512-Home-T-Scores-per-case-4080T.csv"
  )
)

# The file to be read in here is the t-scores per case for the 60perc sample
# that was written out above as "CHILD-512-Home-allData-desamp-60perc.csv". It
# needs to be generated from this latter file using the long script for creating
# raw-to-T lookup tables. A specialized version of this script, just for this
# purpose, has been saved as
# "Child-512-Home-Norms-Lookup-Tables-4080T-60perc-SPM2.R"
T_per_case_60perc_sample <- read_csv(
  here(
    "OUTPUT-FILES/NORMS-OUTPUT-4080T/Child-512-Home-T-Scores-per-case-4080T-60perc.csv"
  )
)

raw_score_desc_full_sample <- T_per_case_full_sample %>% 
  select(contains("raw")) %>%
  describe(fast = TRUE) %>%
  rownames_to_column(var = "scale") %>%
  select(scale, n, mean, sd)

raw_score_desc_60perc_sample <- T_per_case_60perc_sample %>% 
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
  "OUTPUT-FILES/CHILD/COMP-60PERC-SAMPLE/Child-512-Home-raw-desc-full-60perc-comp.csv"
))


