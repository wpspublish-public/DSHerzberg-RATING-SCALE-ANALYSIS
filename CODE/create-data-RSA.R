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
  mutate(
    CPS1_raw = rowSums(.[str_c("cpi", str_pad(as.character(seq(1, 50, by = 5)), 2, side = "left", pad = "0"))]),
    CPS2_raw = rowSums(.[str_c("cpi", str_pad(as.character(seq(2, 50, by = 5)), 2, side = "left", pad = "0"))]),
    CPS3_raw = rowSums(.[str_c("cpi", str_pad(as.character(seq(3, 50, by = 5)), 2, side = "left", pad = "0"))]),
    CPS4_raw = rowSums(.[str_c("cpi", str_pad(as.character(seq(4, 50, by = 5)), 2, side = "left", pad = "0"))]),
    CPS5_raw = rowSums(.[str_c("cpi", str_pad(as.character(seq(5, 50, by = 5)), 2, side = "left", pad = "0"))]),
    across(
      contains("cpi"),
      ~ case_when(
        .x == 0 ~ "never",
        .x == 1 ~ "occasionally",
        .x == 2 ~ "frequently",
        .x == 3 ~ "always"
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
  select(ID:clin_status, CPS1_raw:CPS5_raw, cpi01:cpi50)

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
  mutate(
    CTS1_raw = rowSums(.[str_c("cti", str_pad(as.character(seq(1, 50, by = 5)), 2, side = "left", pad = "0"))]),
    CTS2_raw = rowSums(.[str_c("cti", str_pad(as.character(seq(2, 50, by = 5)), 2, side = "left", pad = "0"))]),
    CTS3_raw = rowSums(.[str_c("cti", str_pad(as.character(seq(3, 50, by = 5)), 2, side = "left", pad = "0"))]),
    CTS4_raw = rowSums(.[str_c("cti", str_pad(as.character(seq(4, 50, by = 5)), 2, side = "left", pad = "0"))]),
    CTS5_raw = rowSums(.[str_c("cti", str_pad(as.character(seq(5, 50, by = 5)), 2, side = "left", pad = "0"))]),
    across(
      contains("cti"),
      ~ case_when(
        .x == 0 ~ "never",
        .x == 1 ~ "occasionally",
        .x == 2 ~ "frequently",
        .x == 3 ~ "always"
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
  select(ID:clin_status, CTS1_raw:CTS5_raw, cti01:cti50)

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
  mutate(
    TPS1_raw = rowSums(.[str_c("tpi", str_pad(as.character(seq(1, 50, by = 5)), 2, side = "left", pad = "0"))]),
    TPS2_raw = rowSums(.[str_c("tpi", str_pad(as.character(seq(2, 50, by = 5)), 2, side = "left", pad = "0"))]),
    TPS3_raw = rowSums(.[str_c("tpi", str_pad(as.character(seq(3, 50, by = 5)), 2, side = "left", pad = "0"))]),
    TPS4_raw = rowSums(.[str_c("tpi", str_pad(as.character(seq(4, 50, by = 5)), 2, side = "left", pad = "0"))]),
    TPS5_raw = rowSums(.[str_c("tpi", str_pad(as.character(seq(5, 50, by = 5)), 2, side = "left", pad = "0"))]),
    across(
      contains("tpi"),
      ~ case_when(
        .x == 0 ~ "never",
        .x == 1 ~ "occasionally",
        .x == 2 ~ "frequently",
        .x == 3 ~ "always"
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
  select(ID:clin_status, TPS1_raw:TPS5_raw, tpi01:tpi50)

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
  mutate(
    TTS1_raw = rowSums(.[str_c("tti", str_pad(as.character(seq(1, 50, by = 5)), 2, side = "left", pad = "0"))]),
    TTS2_raw = rowSums(.[str_c("tti", str_pad(as.character(seq(2, 50, by = 5)), 2, side = "left", pad = "0"))]),
    TTS3_raw = rowSums(.[str_c("tti", str_pad(as.character(seq(3, 50, by = 5)), 2, side = "left", pad = "0"))]),
    TTS4_raw = rowSums(.[str_c("tti", str_pad(as.character(seq(4, 50, by = 5)), 2, side = "left", pad = "0"))]),
    TTS5_raw = rowSums(.[str_c("tti", str_pad(as.character(seq(5, 50, by = 5)), 2, side = "left", pad = "0"))]),
    across(
      contains("tti"),
      ~ case_when(
        .x == 0 ~ "never",
        .x == 1 ~ "occasionally",
        .x == 2 ~ "frequently",
        .x == 3 ~ "always"
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
  select(ID:clin_status, TTS1_raw:TTS5_raw, tti01:tti50)

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
