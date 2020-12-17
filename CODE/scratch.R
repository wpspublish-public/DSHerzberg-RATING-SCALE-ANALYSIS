# GENERATE RAW-TO-T LOOKUP TABLES -----------------------------------------

assign(
  "TOT_lookup", 
  get(str_c("data", age_range_name, form_name, "nt", sep = "_")) %>% 
    group_by(!!rlang::sym(str_c(scale_prefix, "TOT_raw"))
) %>% 
  summarise(
    !!rlang::sym(str_c(scale_prefix, "TOT_nt")) := min(!!rlang::sym(str_c(scale_prefix, "TOT_nt"))
  ))) 

%>% 
  complete(
    TOT_raw = 10:240
  ) %>% 
  fill(
    TOT_NT
  ) %>% 
  fill(
    TOT_NT,
    .direction = "up"
  ) %>% 
  rename(
    raw = TOT_raw
  ) %>% 
  mutate_at(
    vars(TOT_NT), ~ case_when(
      raw < 60 ~ NA_integer_,
      TRUE ~ .x
    )
  )
)

