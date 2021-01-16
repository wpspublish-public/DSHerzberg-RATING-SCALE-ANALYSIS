ntScore_perCase1 <- map_dfc(scale_suffix,
                           ~
                             nzScore_perCase %>%
                             transmute(!!sym(str_c(
                               scale_prefix, .x, "_nt"
                             )) := round(!!sym(str_c(
                               .x, "_nz"
                             )) * 10) + 50)) %>%
  mutate(across(
    everything(),
    ~
      case_when(. < 40 ~ 40,
                . > 80 ~ 80,
                TRUE ~ .) %>%
      as.integer(.)
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
s
