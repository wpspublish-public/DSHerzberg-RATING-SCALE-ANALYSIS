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

# Generate raw-to-T lookup columns. Input is stand sample with raw scores and
# normalized T scores for each case. map() generates a list of two-col lookup
# tables, one for each scale. Each 2-col lookup has raw col and nt (t-score)
# col. Group cases by raw score, relationship between raw and T is many-to-one.
#
# map() iterates over scale_suffix, the char vec containing the six strings that
# identify (differentiate) the six scale names. The names themselves are
# concatenated with a static prefic. the .x argument of map(), and a static
# score type suffix (e.g., "_raw"). !!sym() is used to unquote the names (which
# are strings) so they can be arguments in dplyr functions.
all_lookup <- map(
  scale_suffix,
  ~ get(str_c(
    "data", age_range_name, form_name, "nt", sep = "_"
  )) %>%
    group_by(!!sym(str_c(
      scale_prefix, .x, "_raw"
    ))) %>%
    # Because raw-to-T is many to one, each value of raw has one and only one
    # value of T, and thus summarizing by the min value of T per raw returns the
    # ONLY value of T per raw. But we need the raw column to contain all
    # possible values of raw, and not all possible values of raw are represented
    # in the stand sample. Thus current data object jumps possible raw values
    # (e.g, raw = 62 and raw = 65 might be adjacent rows in this table)
    summarise(!!sym(str_c(
      scale_prefix, .x, "_nt"
    )) := min(!!sym(
      str_c(scale_prefix, .x, "_nt")
      # complete() expands the table vertically, filling in missing values of raw
      # within the range given. This leaves NA cells for T for those rows that
      # didn't have raw values in the input object.
    ))) %>%
    complete(!!sym(str_c(
      scale_prefix, .x, "_raw"
    )) := all_raw_range) %>%
    # fill() replaces NA in T going down the table, with values from the last
    # preceding (lagging) cell that was not NA.
    fill(!!sym(str_c(
      scale_prefix, .x, "_nt"
    ))) %>%
    # A second call of fill() is needed to handle inputs where the first cell(s) of
    # T are NA. 2nd fill call() is uses direction up to fill those first NA cells
    # with the value from the first subsequent (leading) cell that is not NA.
    fill(!!sym(str_c(
      scale_prefix, .x, "_nt"
    )),
    .direction = "up") %>%
    # Recall that the data object at this point is a list of 2-col dfs. We need
    # to drop the subscale identifier from the "raw" col, because this col will
    # be used as the index to join the separate dfs into a single lookup table.
    rename(raw = !!sym(str_c(
      scale_prefix, .x, "_raw"
    ))) 
) %>%
  # purrr::reduce() is used to reduce the six element list to a single df.
  # reduce() applies left_join() iteratively over the input list, such that it
  # joins the first two 2-col dfs by "raw", returning a 3-col df, which it then
  # joins to the next list element (2-col df), returning a 4-col df, and so on,
  # until the final returned object is a 7-col df, with the "raw" col on the far
  # left, and the remaining 6 cols are the t-score lookup cols for each scale.
  reduce(left_join,
         by = 'raw') %>% 
  # The combined look up table has rows that represent impossible raw scores for
  # the subscales (too high) and total score (too low). The next call of
  # mutate() recodes the t-score lookup cols to NA for impossible raw scores. It
  # accomplishes this with two separate calls of across(), isolating first the
  # subscale cols for conditional recoding with case_when(), and then the TOT_nt
  # col for same. We can extract elements of the vector of scale suffixes with
  # the [] subsetting brackets. Because by design, the suffix for TOT is the
  # last element in this vector, we can isolate it with length(scale_suffix),
  # which returns the numerical value of the last position in the vector. To
  # retain all BUT the last element, we simple prepend the expression with a
  # minus (-) sign.
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

# write combined raw-to-T lookup table to .csv
write_csv(all_lookup,
here(str_c(
  "OUTPUT-FILES/TABLES/",
  str_c("raw-T-lookup",
        age_range_name,
        form_name,
        sep = "-"),
  ".csv"
)),
na = '')

# GENERATE RAW-TO-T LOOKUP TABLES -----------------------------------------

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
  # sort descending on T
  arrange(desc(NT)) %>% 
  # rename with desired final column names
  rename_with(~ str_replace_all(., "_nt", "_raw")) %>%
  rename(T_score = NT) %>% 
  # drop row where T == NA
  filter(!is.na(T_score))



# CONTINUE WORKING ON RAW-T LOOKUPS, ADAPTING SPM-2 CODE, SUBSTITUTING
# ROBUST OBJECT NAMES

# NEXT: WRITE PRINT FORMAT LOOKUP TO CSV.

