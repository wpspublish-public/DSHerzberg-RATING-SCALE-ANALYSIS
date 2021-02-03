# Load packages, read data

suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(psych))
suppressMessages(library(bestNormalize))

urlRemote_path  <- "https://raw.githubusercontent.com/"
github_path <- "DSHerzberg/RATING-SCALE-ANALYSIS/master/INPUT-FILES/"
input_name <- "data-RS-sim-child-parent.csv"

item_prefix <- "cp"
scale_prefix <- "CP"
scale_suffix <- c("S1", "S2", "S3", "S4", "S5", "TOT")
age_range_name <- "child"
form_name <- "parent"
all_raw_range <- 10:200
TOT_raw_lower_bound <- 50
subscale_raw_upper_bound <- 40
t_score_lower_bound <- 40
t_score_upper_bound <- 80

assign(
  str_c("data", age_range_name, form_name, sep = "_"),
  suppressMessages(read_csv(url(
    str_c(urlRemote_path, github_path, input_name)
  ))) %>%
    mutate(across(
      contains(str_c(item_prefix, "i")),
      ~ case_when(
        .x == "never" ~ 1,
        .x == "occasionally" ~ 2,
        .x == "frequently" ~ 3,
        .x == "always" ~ 4
      )
    ))
)

# DETERMINE BEST NORMALIZATION MODEL, CALC NORMALIZED T-SCORES PER CASE  ------------------------

assign(str_c("data", age_range_name, form_name, "TOT", sep = "_"),
       suppressMessages(read_csv(url(
         str_c(urlRemote_path, github_path, input_name)
       ))) %>% 
         select(!!sym(str_c(scale_prefix, "TOT_raw"))) %>% 
         as_vector() %>% 
         set_names(NULL)
)

set.seed(12345)
TOT_nz_obj <- bestNormalize(data_child_parent_TOT)
TOT_nz_obj$chosen_transform
chosen_transform <- class(TOT_nz_obj$chosen_transform)

raw_score_vecs_list <-
  map(str_c(scale_prefix, scale_suffix),
      ~ get(str_c("data", age_range_name, form_name, sep = "_")) %>%
        pull(!!sym(str_c(.x, "_raw")))) %>% 
  set_names(str_c(scale_prefix, scale_suffix, "_raw"))

nzScore_perCase <- raw_score_vecs_list %>% 
  map(~ get(chosen_transform)(.x)) %>% 
  map(~ tibble(pluck(.x, "x.t"))) %>% 
  bind_cols() %>%
  set_names(str_c(scale_suffix, "_nz")) 

ntScore_perCase <- nzScore_perCase %>%
  mutate(across(everything(),
                ~
                  (round(. * 10) + 50) %>%
                  {
                    case_when(
                      . < t_score_lower_bound ~ t_score_lower_bound,
                      . > t_score_upper_bound ~ t_score_upper_bound,
                      TRUE ~ .
                    )
                  } %>%
                  as.integer)) %>%
  rename_with( ~ str_c(scale_prefix, str_replace_all(., "nz", "nt")))

assign(
  str_c("data", age_range_name, form_name, "nt", sep = "_"),
  get(str_c("data", age_range_name, form_name, sep = "_")) %>% bind_cols(ntScore_perCase) %>%
    mutate(clin_status = 'typ',
           clin_dx = NA) %>%
    select(
      ID:region,
      clin_status,
      clin_dx,
      contains("raw"),
      contains("nt"),
      everything()
    )
)

write_csv(get(str_c(
  "data", age_range_name, form_name, "nt", sep = "_"
)),
here(str_c(
  "OUTPUT-FILES/TABLES/",
  str_c("nt-Scores-per-case",
        age_range_name,
        form_name,
        sep = "-"),
  ".csv"
)),
na = '')

get(str_c("data", age_range_name, form_name, "nt", sep = "_")) %>%
  select(contains("TOT_nt")) %>%
  as_vector() %>%
  MASS::truehist(.,
                 h = 1,
                 prob = FALSE,
                 xlab = "TOT_nt")

# GENERATE BASIC FORMAT RAW-TO-T LOOKUP TABLE -----------------------------------------

all_lookup_basic <- map(
  scale_suffix,
  ~ get(str_c(
    "data", age_range_name, form_name, "nt", sep = "_"
  )) %>%
    group_by(!!sym(str_c(
      scale_prefix, .x, "_raw"
    ))) %>%
    summarize(!!sym(str_c(
      scale_prefix, .x, "_nt"
    )) := min(!!sym(
      str_c(scale_prefix, .x, "_nt")
    ))) %>%
    complete(!!sym(str_c(
      scale_prefix, .x, "_raw"
    )) := all_raw_range) %>%
    fill(!!sym(str_c(
      scale_prefix, .x, "_nt"
    )),
    .direction = "downup") %>%
    rename(raw = !!sym(str_c(
      scale_prefix, .x, "_raw"
    ))) 
) %>%
  reduce(left_join,
         by = 'raw') %>% 
mutate(across(
  contains(scale_suffix[-length(scale_suffix)]),
  ~ case_when(raw > subscale_raw_upper_bound ~ NA_integer_,
              TRUE ~ .x)
),
across(
  contains(scale_suffix[length(scale_suffix)]),
  ~ case_when(raw < TOT_raw_lower_bound ~ NA_integer_,
              TRUE ~ .x)
))

write_csv(all_lookup_basic,
here(str_c(
  "OUTPUT-FILES/TABLES/",
  str_c("raw-T-lookup",
        age_range_name,
        form_name,
        sep = "-"),
  ".csv"
)),
na = '')

# GENERATE PRINT FORMAT RAW-TO-T LOOKUP TABLE -----------------------------------------

all_lookup_print <- all_lookup_basic %>% 
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
  # sort descending on T
  arrange(desc(NT)) %>% 
  # rename with desired final column names
  rename_with(~ str_replace_all(., "_nt", "_raw")) %>%
  rename(T_score = NT) %>% 
  # drop row where T == NA
  filter(!is.na(T_score))

# write print format lookup table to .csv
write_csv(all_lookup_print,
          here(str_c(
            "OUTPUT-FILES/TABLES/",
            str_c("raw-T-lookup-print",
                  age_range_name,
                  form_name,
                  sep = "-"),
            ".csv"
          )),
          na = '')

# raw score descriptives for all scales (using psych::describe)
assign(
  str_c("raw_score_desc", age_range_name, form_name, sep = "_"),
  data_child_parent %>%
    select(contains('raw')) %>%
    describe(fast = T) %>%
    rownames_to_column() %>%
    rename(scale = rowname) %>%
    select(scale, n, mean, sd) %>%
    mutate(across(c(mean, sd), ~ (round(
      ., 2
    ))))
)

# write raw score descriptives table to .csv
write_csv(get(str_c(
  "raw_score_desc", age_range_name, form_name, sep = "_"
)),
here(str_c(
  "OUTPUT-FILES/TABLES/",
  str_c("raw-score-desc",
        age_range_name,
        form_name,
        sep = "-"),
  ".csv"
)),
na = '')

# table of demographic counts (make vector code robust wherever possible)

var_order <- c("age_range", "gender", "educ", "ethnic", "region")

cat_order <- c(
  # age_range
  str_sort(unique(get(str_c("data", age_range_name, form_name, sep = "_"))$age_range)),
  # gender
  str_sort(unique(get(str_c("data", age_range_name, form_name, sep = "_"))$gender), 
           decreasing = T),
  # educ
  "no_HS", "HS_grad", "some_college", "BA_plus" , 
  # ethnic
  "hispanic", "asian", "black", "white", "other",
  # Region
  "northeast", "midwest", "south", "west")

assign(
  str_c("demo_counts", age_range_name, form_name, sep = "_"),
  get(str_c("data", age_range_name, form_name, sep = "_")) %>%
    select(all_of(var_order)) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "category") %>%
    group_by(variable, category) %>%
    count(variable, category) %>%
    arrange(match(variable, var_order), match(category, cat_order)) %>%
    ungroup() %>%
    mutate(across(
      variable,
      ~
        case_when(lag(.x) == .x ~ NA_character_,
                  T ~ .x)
    ))
)

# write demo counts table to .csv
write_csv(get(str_c(
  "demo_counts", age_range_name, form_name, sep = "_"
)),
here(str_c(
  "OUTPUT-FILES/TABLES/",
  str_c("demo-counts",
        age_range_name,
        form_name,
        sep = "-"),
  ".csv"
)),
na = '')
