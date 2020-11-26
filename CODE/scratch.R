temp3 <- tibble(S1_nz = nzScore_perCase$S1_nz) %>% 
  mutate(S1_nt = map(S1_nz, ~ round(.x * 10) + 50))

temp4 <- nzScore_perCase %>% 
  mutate(S1_nt = map_dbl(S1_nz, ~ round(.x * 10) + 50))

temp5 <- nzScore_perCase %>% 
  mutate(S1_nt = round(S1_nz * 10) + 50)

temp6 <- map_dfc(scale_suffix,
             ~
               nzScore_perCase %>% 
               transmute(!!rlang::sym(str_c(.x, "_nt")) := round(!!rlang::sym(str_c(.x, "_nz")) * 10) + 50)
)


temp7 <- raw_score_cols_list %>%
  map( ~ get(chosen_transform)(.x)) %>%
  map( ~ tibble(pluck(.x, "x.t"), .name_repair = "universal")) %>%
  bind_cols() %>%
  set_names(str_c(scale_suffix, "_nz")) %>%
  map_dfc(scale_suffix,
          ~
            . %>%
            transmute(!!rlang::sym(str_c(.x, "_nt")) := round(!!rlang::sym(str_c(
              .x, "_nz"
            )) * 10) + 50))

temp8 <- nzScore_perCase %>% 
  map_dfc(scale_suffix,
                 ~
                   # . %>%
                   transmute(!!rlang::sym(str_c(.x, "_nt")) := round(!!rlang::sym(str_c(.x, "_nz")) * 10) + 50)
)
