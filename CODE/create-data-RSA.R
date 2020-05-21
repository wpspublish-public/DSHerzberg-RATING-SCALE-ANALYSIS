suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(psych))

set.seed(123)


data_input_sim <- as_tibble(sim.poly.ideal(nvar = 50, n = 1000, cat = 4,)[["items"]]) %>%
  mutate_all( ~ case_when(.x == 0 ~ "never",
                          .x == 1 ~ "occasionally",
                          .x == 2 ~ "frequently",
                          .x == 3 ~ "always")) %>% 
  rename_all(~ str_c("i", str_pad(as.character(1:50), 2, side = "left", pad = "0"))) %>% 
  mutate(
    ID = 100001:101000,
    age = sample(c(5:12), 1000, replace=TRUE),
    gender = sample(c("female","male"), 1000, replace=TRUE, prob = c(0.53, 0.47)),
    p_educ = sample(c("no_HS","HS_grad", "some_college", "BA_plus"), 
                    1000, replace=TRUE, prob = c(0.119, 0.263, 0.306, 0.311)),
    ethnic = sample(c("hispanic","asian", "black", "white", "other"), 
                    1000, replace=TRUE, prob = c(0.239, 0.048, 0.136, 0.521, .056)),
    region = sample(c("northeast","south", "midwest", "west"), 
                    1000, replace=TRUE, prob = c(0.166, 0.383, 0.212, 0.238)),
    clin_status = sample(c("typ","clin"), 1000, replace=TRUE, prob = c(0.8, 0.2))
  ) %>% 
  select(ID:clin_status, i01:i50)

data_input_bfi <- bfi %>% 
  drop_na() %>% 
  sample_n(1000)

# bfi coding
# gender
# Males = 1, Females =2
# 
# education
# 1 = HS, 2 = finished HS, 3 = some college, 4 = college graduate 5 = graduate degree
# 
# age
# age in years


# bfi item reponse key
# 1 Very Inaccurate 2 Moderately Inaccurate 3 Slightly Inaccurate 
# 4 Slightly Accurate 5 Moderately Accurate 6 Very Accurate

# get freqs of vals accross vars
var_order <- c("age", "gender", "p_educ", "ethnic", "region", "clin_status",
               str_c("i", str_pad(as.character(1:50), 2, side = "left", pad = "0")))

cat_order <- c(
  # age
  "5", "6", "7", "8", "9", "10", "11", "12",
  # gender
  "male", "female",
  # p_educ
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
  
