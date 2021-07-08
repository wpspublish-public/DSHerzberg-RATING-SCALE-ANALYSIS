set.seed(1234)
sample_60perc2 <- sample_full %>% 
  group_by(age, gender, educ, ethnic, region) %>%
  sample_frac(0.6) %>% 
  ungroup()
