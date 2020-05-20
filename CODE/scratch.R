suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(psych))

test.sim.item <- as_tibble(
  sim.item(
    categorical = T
  ))

test.sim.poly <- as_tibble(sim.poly(nvar = 50, cat = 4,)[["items"]]) %>%
  mutate_all( ~ case_when(.x == 0 ~ 1,
                          .x == 1 ~ 2,
                          .x == 2 ~ 3,
                          .x == 3 ~ 4)) %>% 
  rename_all(~ str_c("i", str_pad(as.character(1:50), 2, side = "left", pad = "0"))) %>% 
  mutate(
    ID = 100001:100500,
    gender = sample(c("female","male"), 500, replace=TRUE, prob = c(0.53, 0.47)),
    p_educ = sample(c("no_HS","HS_grad", "some_college", "BA_plus"), 500, replace=TRUE, prob = c(0.119, 0.263, 0.306, 0.311)),
  )

# get freqs of vals accross vars
freq_val <- test.sim.poly %>% 
  gather(var, value) %>% 
  group_by(var, value) %>% 
  count() %>% 
  ungroup() %>% 
  spread(value, n)

cor <- cor(test.sim.poly)
