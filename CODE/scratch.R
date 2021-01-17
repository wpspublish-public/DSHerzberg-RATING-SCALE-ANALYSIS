ntScore_perCase <- nzScore_perCase %>%
  mutate(across(everything(),
                ~
                  round(. * 10) + 50),
         across(
           everything(),
           ~
             case_when(. < 40 ~ 40,
                       . > 80 ~ 80,
                       TRUE ~ .) %>%
             as.integer(.)
         )) %>%
  rename_with( ~ str_c(scale_prefix, str_replace_all(., "nz", "nt")))

ntScore_perCase1 <- nzScore_perCase %>%
  mutate(across(everything(),
                ~
                  (round(. * 10) + 50) %>% 
                     {case_when(. < 40 ~ 40,
                       . > 80 ~ 80,
                       TRUE ~ .)} %>%
             as.integer
         )) %>%
  rename_with( ~ str_c(scale_prefix, str_replace_all(., "nz", "nt")))

df3 <- df1 %>%
  mutate(across(everything(),
                ~
                  (round(. * 10) + 50) %>% 
                  {case_when(. < 45 ~ 45,
                             . > 55 ~ 55,
                             TRUE ~ .)} %>%
                  as.integer
  ))






ntScore_perCase2 <- nzScore_perCase %>%
  mutate(across(
                list(
                  # round(. * 10) + 50,
                case_when(. < 40 ~ 40,
                       . > 80 ~ 80,
                       TRUE ~ .),
                  as.integer(.)
         ))) %>%
  rename_with( ~ str_c(scale_prefix, str_replace_all(., "nz", "nt")))



# REPREX

set.seed(1234)
df1 <- tibble(
  S1 = rnorm(5), 
  S2 = rnorm(5), 
  S3 = rnorm(5), 
  S4 = rnorm(5), 
)

df2 <- df1 %>%
  mutate(across(everything(),
                ~
                  round(. * 10) + 50),
         across(
           everything(),
           ~
             case_when(. < 45 ~ 45,
                       . > 55 ~ 55,
                       TRUE ~ .) %>%
             as.integer(.)
         ))

df3 <- df1 %>%
  mutate(across(everything(),
                ~
                  (round(. * 10) + 50) %>% 
                    {case_when(. < 45 ~ 45,
                       . > 55 ~ 55,
                       TRUE ~ .)} %>%
             as.integer
         ))

df1 %>%
  mutate(across(everything(), ~
                  (round(. * 10) + 50) %>%
                  {case_when(. < 45 ~ 45, . > 55 ~ 55, TRUE ~ .)} %>% 
                  as.integer ))

