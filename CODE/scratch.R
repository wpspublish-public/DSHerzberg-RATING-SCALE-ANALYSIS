#Git test

demos_comp1 <- bind_rows(demos_full,
                        demos_60_perc) %>%
  mutate(across(sample,
                ~ case_when(
                  lag(sample) == sample ~ NA_character_,
                  TRUE ~ .x
                )))
