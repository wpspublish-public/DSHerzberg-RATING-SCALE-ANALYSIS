# Load packages, read data

suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(psych))
suppressMessages(library(bestNormalize))

urlRemote_path  <- "https://raw.github.com/"
github_path <- "wpspublish/DSHerzberg-RATING-SCALE-ANALYSIS/master/INPUT-FILES/"
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
pivot_longer(contains("nt"), names_to = "scale", values_to = "NT") %>% 
  arrange(scale) %>% 
  group_by(scale) %>%
  complete(NT = 40:80) %>% 
  group_by(scale, NT) %>%
  filter(n() == 1 | n() > 1 & row_number()  %in% c(1, n())) %>%
  summarize(raw = str_c(raw, collapse = '--')) %>%
  mutate(across(raw, ~ case_when(is.na(.x) ~ '-', TRUE ~ .x))) %>%
  arrange(scale, desc(NT)) %>% 
  pivot_wider(names_from = scale,
              values_from = raw) %>% 
  rename_with(~ str_replace_all(., "_nt", "_raw")) %>%
  rename(T_score = NT) %>% 
  filter(!is.na(T_score))

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

# RAW SCORE DESCRIPTIVES AND DEMOGRAPHIC COUNTS -----------------------------------------

assign(
  str_c("raw_score_desc", age_range_name, form_name, sep = "_"),
  get(str_c("data", age_range_name, form_name, sep = "_")) %>%
    select(contains("raw")) %>%
    describe(fast = TRUE) %>%
    rownames_to_column(var = "scale") %>%
    select(scale, n, mean, sd) %>%
    mutate(across(c(mean, sd), ~ (round(
      ., 2
    ))))
)

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

var_order <- c("age_range", "gender", "educ", "ethnic", "region")

cat_order <- c(
  # age_range
  str_sort(unique(get(str_c("data", age_range_name, form_name, sep = "_"))$age_range)),
  # gender
  str_sort(unique(get(str_c("data", age_range_name, form_name, sep = "_"))$gender), 
           decreasing = TRUE),
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
                  TRUE ~ .x)
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
