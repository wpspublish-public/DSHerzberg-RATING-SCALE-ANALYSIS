suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(psych))

set.seed(123)


data_input_sim <-
  as_tibble(sim.poly.ideal(nvar = 50, n = 1000, cat = 4, )[["items"]]) %>%
  mutate_all(
    ~ case_when(
      .x == 0 ~ "never",
      .x == 1 ~ "occasionally",
      .x == 2 ~ "frequently",
      .x == 3 ~ "always"
    )
  ) %>%
  rename_all( ~ str_c("i", str_pad(
    as.character(1:50), 2, side = "left", pad = "0"
  ))) %>%
  mutate(
    ID = 100001:101000,
    age = sample(c(5:12), 1000, replace = TRUE),
    age_range = case_when(
      age <=8 ~ "5 to 8 yo",
      T ~ "9 to 12 yo"
    ),
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
  select(ID:clin_status, i01:i50)

data_input_bfi <- bfi %>%
  drop_na() %>%
  sample_n(1000) %>%
  mutate(
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
  mutate_at(
    vars(A1:O5),
    ~
      case_when(
        .x == 1 ~ "very inacurate",
        .x == 2 ~ "moderately inacurate",
        .x == 3 ~ "slightly inacurate",
        .x == 4 ~ "slightly acurate",
        .x == 5 ~ "moderately acurate",
        .x == 6 ~ "very acurate",
      )
  ) %>% 
  select(ID, age:clin_status, A1:O5)

# get freqs of vals accross vars
var_order <- c("age", "age_range","gender", "p_educ", "ethnic", "region", "clin_status",
               str_c("i", str_pad(as.character(1:50), 2, side = "left", pad = "0")))

cat_order <- c(
  # age
  "5", "6", "7", "8", "9", "10", "11", "12",
  # age_range
  "5 to 8 yo", "9 to 12 yo", "18 yo or younger", 
  "19 to 24 yo", "25 to 39 yo", "40 yo or older",
  # gender
  "male", "female",
  # educ
  "no_HS","HS_grad", "some_college", "BA_plus", 
  # ethnic
  "hispanic","asian", "black", "white", "other", 
  # region
  "northeast","south", "midwest", "west",
  # clin_status
  "typ", "clin", 
  # items
  "never", "occasionally","frequently", "always"
)

freq_val_sim <- data_input_sim %>% 
  select(var_order) %>% 
  gather(var, value) %>% 
  group_by(var, value) %>% 
  count(var, value) %>% 
  ungroup() %>%
  spread(value, n) %>% 
  arrange(match(var, var_order)) %>%
  select(var, cat_order)
  
