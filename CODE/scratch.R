suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(psych))

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

