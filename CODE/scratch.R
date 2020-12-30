# generate print pub format raw-to-T table
all_lookup_print <- all_lookup %>% 
  # pivot_longer() collapses wide table into three-column tall table. First
  # argument (-raw) specifies that raw score column will exclude from pivot, and
  # its rows will be expanded as needed to accomdate pivot of remaining cols,
  # Second argument (names_to = "scale") names the col that will hold in its
  # rows the col names from the input object. Third argument (values_to = "T")
  # names the col that will hold the cell values from the input object.
  # pivot_longer returns a 3-col df in which the key-value pairs of scale-Tscore
  # are nested within each value of raw score.
  
  # df %>% gather("key", "value", x, y, z) is equivalent to df %>%
  # pivot_longer(c(x, y, z), names_to = "key", values_to = "value")

  pivot_longer(contains("nt"), names_to = "scale", values_to = "NT") %>% 
  arrange(scale) %>% 
  group_by(scale) %>%
  # expand the table vertically, adding new rows, so there's a row for every possible T value
  complete(NT = 40:80) %>% 
  ungroup() %>%
  # regroup table by two levels
  group_by(scale, NT) %>%
  # filter step retains all 1-row groups, and the first and last rows of any
  # multi-row groups. n() == 1 returns 1-row groups; n() > 1 & row_number() %in%
  # c(1, n()) returns rows of multi-row groups with the row number of either 1
  # (first row), or n() which is the number of rows and also the number of the
  # last row. The first and last rows hold the min and max values of raw for
  # that value of T (the grouping variable). dplyr::n() works within summarize()
  # and returns the number of rows in the current group.
  filter(n() == 1 | n() > 1 & row_number()  %in% c(1, n())) %>%
  # SummariZe creates a table with one row per group (one row per
  # possible value of T). For the 1-row groups, str_c simply passes the
  # value of raw as a string; for the multi-row groups, str_c joins the min
  # and max values of raw with the '--' separator.
  summarize(raw = str_c(raw, collapse = '--')) %>%
  # recode missing values of raw to '-'
  mutate(across(raw, ~ case_when(is.na(.x) ~ '-', TRUE ~ .x))) %>%
  # sort on two cols
  arrange(scale, desc(NT)) %>% 
  # spread table back to wide, all values of T (one row for each), scale
  # columns filled with values of rawscore
  pivot_wider(names_from = scale,
              values_from = raw) %>% 
  
  # START HERE - DATA OBJECT GOOD UP TO THIS POINT
  # sort descending on T
  arrange(desc(NT)) %>% 
  # rename with desired final column names
  rename_at(vars(ends_with('_NT')), ~ gsub("_NT", "_raw", .)) %>% 
  # order columns left-to-right
  select(T, all_of(all_lookup_col_names)) %>% 
  # drop row where T == NA
  filter(!is.na(T))
