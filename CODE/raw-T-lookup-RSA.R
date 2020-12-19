# Load packages, read data

suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
library(bestNormalize)
suppressMessages(library(psych))
suppressMessages(library(data.table))

urlRemote_path  <- "https://raw.githubusercontent.com/"
github_path <- "DSHerzberg/RATING-SCALE-ANALYSIS/master/INPUT-FILES/"

item_prefix <- "cp"
scale_prefix <- "CP"
scale_suffix <- c("S1", "S2", "S3", "S4", "S5", "TOT")
age_range_name <- "child"
form_name <- "parent"
all_raw_range <- 10:200
TOT_raw_lower_bound <- 50
subscale_raw_upper_bound <- 40

# base::assign() can be used to name objects which require names that are
# concatenations of strings and character vectors. The first argument is the
# desired name as a string. Recode item responses to numeric (still within
# assign()).
assign(
  str_c("data", age_range_name, form_name, sep = "_"),
  suppressMessages(read_csv(url(
    str_c(urlRemote_path, github_path, "data-RS-sim-child-parent.csv")
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

# Here we extract a single column as an argument for the bestNormalize()
# function. To get the col into the right format, we use select() to isolate the
# col (which is now a single col data frame), purrr::as_vector() to convert the
# df into a named numeric vector, and purrr::set_names() to get rid of the the
# name on the vector
 assign(str_c("data", age_range_name, form_name, "TOT", sep = "_"),
       suppressMessages(read_csv(url(
         str_c(urlRemote_path, github_path, "data-RS-sim-child-parent.csv")
       ))) %>% 
         select(!!sym(str_c(scale_prefix, "TOT_raw"))) %>% 
         as_vector() %>% 
         set_names(NULL)
)

# DETERMINE BEST NORMALIZATION MODEL --------------------------------------

# (NOTE: THIS SECTION SHOULD BE TOGGLED OFF AFTER SELECTION OF NORMALIZATION
# MODEL)

# # # create a bestNormalize object to lock down the normalizing function that
# will be used on repeated runs of the norms. This should be done on TOT score
# or analogous score. We use set.seed() to control R's random number generator,
# ensuring that the same normalization model is selected every time we run the
# script.
set.seed(12345)
TOT_nz_obj <- bestNormalize(data_child_parent_TOT)

# # print transformation (to show normalizing function in console)
TOT_nz_obj$chosen_transform

# # Extract transformation type. class() gets the class atributes of a list
# element. bestNormalize puts the name of the normalizing function as a class
# attribute of the chosen_transform element, it names the function that is
# called to implement the normalization model when estimating normalized z-scores
# for each case in the input data set.
chosen_transform <- class(TOT_nz_obj$chosen_transform)[1]

# pull 6 raw score columns into a named list. In the pipeline within the map()
# call, we start with the input data file. Because the name of that file is a
# concatenated string (to facilitate robust code), we have to wrap the string in
# get(), which returns the object (the input data file) named by the
# concatenated string.
raw_score_cols_list <-
  map(str_c(scale_prefix, scale_suffix),
      ~ get(str_c("data", age_range_name, form_name, sep = "_")) %>%
        pull(!!rlang::sym(str_c(.x, "_raw")))) %>% 
  set_names(str_c(scale_prefix, scale_suffix, "_raw"))


# This next pipeline starts with the list of 6 raw score cols. First call of
# map() iterates over the list of raw score cols, and applies the chosen
# normalizing function to each raw score col iteratively. To call the
# normalizing function, we use base::get(), which takes a string as input and
# returns the function named by that string. The list, which now contains
# normalization objects, is piped into a second map() call, which extracts
# columns of normalized z-scores per case. These scores are held in the "x.t."
# element of the normalization object for each scale. purrr::pluck() extracts an
# element from a list, and wrapping pluck in tibble() gets it into the desired
# data frame structure. Piping that list through dplyr::bind_cols() binds the 6
# dfs into a single df, set_names() names the 6 cols within the single df.
nzScore_perCase <- raw_score_cols_list %>% 
  map(~ get(chosen_transform)(.x)) %>% 
  map(~ tibble(pluck(.x, "x.t"), .name_repair = "universal")) %>% 
  bind_cols() %>%
  set_names(str_c(scale_suffix, "_nz")) 

# we now apply an arithmetic transformation to convert each normalized Z score
# (over the 6 scales) into a normalized t-score. Here we map over scale_suffix,
# because we need to refer to the scale identifiers, and we use map_dfc() to
# return a data frame. Within the mapping function, we pipe the six col df
# containing the normalized z-scores (nzScore_perCase) into dplyr::transmute(),
# which is similar to mutate except that it drops the input cols from the
# output. We use !!rlang::sym() to convert a character string col name to
# unquoted symbol, so it can be passed to transmute(). We also need to use the
# NSE := operator instead of a conventional equals sign. We return a six col df
# consisting only of the 6 rounded normalize t-scores. We then use
# mutate(across(everything())) to transform all six columns, applying a function
# that first truncates the score distribution in each col to 40-80 range, and
# then coerces all numbers to integer
ntScore_perCase <- map_dfc(scale_suffix,
                           ~
                             nzScore_perCase %>%
                             transmute(!!rlang::sym(str_c(
                               scale_prefix, .x, "_nt"
                             )) := round(!!rlang::sym(str_c(
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

# Bind the normalized T-score columns to the table containing raw scores for
# each case.
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

# write T-scores per case table to .csv
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


# Quick visual check of normality with MASS::truehist() plot. Use
# purrr::as_vector() to convert df col to numeric vector, which truehist() needs
# as input.
get(str_c("data", age_range_name, form_name, "nt", sep = "_")) %>%
  select(contains("TOT_nt")) %>%
  as_vector() %>%
  MASS::truehist(.,
                 h = 1,
                 prob = F,
                 xlab = "TOT_nt")

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
    # we need to drop the subscale identifier from the "raw" col, because this col
    # will be used as the index to join the TOT lookup table to the subscale
    # lookup tables
    rename(
      raw = !!sym(str_c(scale_prefix, "TOT_raw"))
    ) %>% 
    # next code is because TOT lookup table will be bound to subscales lookup
    # table, and the two have different raw score ranges. The joined table will
    # represent raw scores that are not possible for TOT, therefore these rawscore
    # rows need to be recoded to NA in the NT col
    mutate(
      across(!!sym(str_c(scale_prefix, "TOT_nt")), 
             ~ case_when(
               raw < TOT_raw_lower_bound ~ NA_integer_,
               TRUE ~ .x
             )
      )
    ))


# NEXT: CONTINUE WORKING ON RAW-T LOOKUPS, ADAPTING SPM-2 CODE, SUBSTITUTING
# ROBUST OBJECT NAMES

