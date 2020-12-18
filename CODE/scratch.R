# GENERATE RAW-TO-T LOOKUP TABLES -----------------------------------------

# Generate raw-to-T lookup columns. Handle TOT and subscale scores separately,
# because each type has different raw score range. Start wtih TOT. Input is
# stand sample with raw scores and normalized T scores for each case. Group
# cases by raw score, relationship between raw and T is many-to-one.
assign(
  "TOT_lookup", 
  get(str_c("data", age_range_name, form_name, "nt", sep = "_")) %>% 
    group_by(!!sym(str_c(scale_prefix, "TOT_raw"))
) %>% 
  # Because raw-to-T is many to one, all values of T are identical for each raw,
  # and summarizing by the min value of T per raw returns the ONLY value of T per
  # raw. But we need the raw column to contain all possible values of raw, and
  # not all possible values of raw are represented in the stand sample. Thus
  # current data object jumps possible raw values (e.g, raw = 62 and raw = 65
  # might be adjacent rows in this table)
  summarize(
    !!sym(str_c(scale_prefix, "TOT_nt")) := min(!!rlang::sym(str_c(scale_prefix, "TOT_nt"))
  )) %>% 
  # complete() expands the table vertically, filling in missing values of raw
  # within the range given. This leaves NA cells for T for those rows that
  # didn't have raw values in the input object.
  complete(
    !!sym(str_c(scale_prefix, "TOT_raw")) := all_raw_range
  ) %>% 
  # fill() replaces NA in T going down the table, with values from the last
  # preceding (lagging) cell that was not NA.
  fill(
    !!sym(str_c(scale_prefix, "TOT_nt")) 
  ) %>% 
  # A second call of fill() is needed to handle inputs where the first cell(s) of
  # T are NA. 2nd fill call() is uses direction up to fill those first NA cells
  # with the value from the first subsequent (leading) cell that is not NA.
  fill(
    !!sym(str_c(scale_prefix, "TOT_nt")),
    .direction = "up"
  ) %>% 
  # next code is because TOT lookup table will be bound to subscales lookup
  # table, and the two have different raw score ranges. The joined table will
  # represent raw scores that are not possible for TOT, therefore these rawscore
  # rows need to be recoded to NA in the NT col
   mutate(
    across(!!sym(str_c(scale_prefix, "TOT_nt")), 
    ~ case_when(
      !!sym(str_c(scale_prefix, "TOT_raw")) < TOT_raw_lower_bound ~ NA_integer_,
      TRUE ~ .x
    )
  )
))



