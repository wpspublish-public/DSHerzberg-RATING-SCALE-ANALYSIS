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

# base::assign() can be used to name objects which require names that are
# concatentations of strings and character vectors. The first argument is the
# desired name as a string.
assign(str_c("data", age_range_name, form_name, sep = "_"),
       suppressMessages(read_csv(url(
         str_c(urlRemote_path, github_path, "data-RS-sim-child-parent.csv")
       ))))

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

# pull 6 raw score columns into a named list
raw_score_cols_list <-
  map(str_c("CP", scale_suffix),
      ~ data_RS_sim_child_parent %>%
        pull(!!rlang::sym(str_c(.x, "_raw")))) %>% 
  set_names(str_c("CP", scale_suffix, "_raw"))

# create a named list containing the normalizations for each raw score
# distribution. map() iterates over the list of raw score cols, and applies the
# chosen normalizing function to each raw score col iteratively. To call the
# normalizing function, we use base::get(), which takes a string as input and
# returns the function named by that string
raw_score_nz_transform_list <- raw_score_cols_list %>% 
  map(~ get(chosen_transform)(.x)) %>% 
  set_names(str_c(scale_suffix, '_nz_transform'))

# map() over the list of . These scores are held in the "x.t." element
# of the normalization object for each scale. purrr::pluck() extracts an element
# from a list, and wrapping pluck in data.frame() gets it into the desired df
# structure. Piping that list through dplyr::bind_cols() binds the 6 dfs into a
# single df, set_names() names the 6 cols within the single df.
nzScore_perCase <- raw_score_nz_transform_list %>% 
  map(~ data.frame(pluck(.x, "x.t"))) %>% 
  bind_cols() %>% 
  set_names(str_c(scale_suffix, "_nz"))

# This next pipeline starts with the list of 6 raw score cols. First call of
# map() map() iterates over the list of raw score cols, and applies the chosen
# normalizing function to each raw score col iteratively. To call the
# normalizing function, we use base::get(), which takes a string as input and
# returns the function named by that string. The list, which now contains
# normalization objects, is piped into a second map() call, which extracts
# columns of normalized z-scores per case. These scores are held in the "x.t." element
# of the normalization object for each scale. purrr::pluck() extracts an element
# from a list, and wrapping pluck in tibble() gets it into the desired data frame
# structure. Piping that list through dplyr::bind_cols() binds the 6 dfs into a
# single df, set_names() names the 6 cols within the single df.
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
                             transmute(!!rlang::sym(str_c(.x, "_nt")) := round(!!rlang::sym(str_c(
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
data_RS_sim_child_parent_nt <- data_RS_sim_child_parent %>% bind_cols(ntScore_perCase) %>% 
  mutate(clin_status = 'typ',
         clin_dx = NA) %>% 
  select(IDNumber, Age, age_range, Gender:Region, data, clin_status, clin_dx, everything())


# NEXT CONTINUE TO SUBSTITUTE ROBUST OBJECT NAMES THROUGHOUT SCRIPT

