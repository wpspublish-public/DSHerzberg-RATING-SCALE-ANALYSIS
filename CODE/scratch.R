alpha_output <- scale_item_data %>%
  mutate(alpha = map(items, ~ alpha(cor(.x))[["total"]])) %>%
  unnest(alpha) %>%
  select(form, scale, raw_alpha) %>%
  rename(alpha = raw_alpha) %>%
  left_join(scale_n_mean_sd, by = c("form", "scale")) %>%
  group_by(form) %>%
  mutate(
    SEM = sd * (sqrt(1 - alpha)),
    CV_90_UB = round(mean + 1.6449 * SEM),
    CV_90_LB = round(mean - 1.6449 * SEM),
    CV_95_UB = round(mean + 1.96 * SEM),
    CV_95_LB = round(mean - 1.96 * SEM),
    CI_90 = str_c(CV_90_LB, "--", CV_90_UB),
    CI_95 = str_c(CV_95_LB, "--", CV_95_UB),
    across(is.numeric, ~ round(., 2)),
    form = case_when(row_number() == 1 ~ form,
                     T ~ NA_character_),
    n = case_when(row_number() == 1 ~ n,
                  T ~ NA_real_)
  ) %>%
  select(form, n, scale, alpha, SEM, CI_90, CI_95) 

write_csv(alpha_output,
          here("OUTPUT-FILES/TABLES/alpha-summary-by-form.csv"),
          na = "")
