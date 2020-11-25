# Load packages, read data

suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
library(bestNormalize)
suppressMessages(library(psych))
suppressMessages(library(data.table))

urlRemote_path  <- "https://raw.githubusercontent.com/"
github_path <- "DSHerzberg/RATING-SCALE-ANALYSIS/master/INPUT-FILES/"

data_RS_sim_child_parent <- suppressMessages(read_csv(url(
  str_c(urlRemote_path, github_path, "data-RS-sim-child-parent.csv")
)))

scale_suffix <- c("S1", "S2", "S3", "S4", "S5", "TOT")


# DETERMINE BEST NORMALIZATION MODEL --------------------------------------

# (NOTE: THIS SECTION SHOULD BE TOGGLED OFF AFTER SELECTION OF NORMALIZATION
# MODEL)

# # # create a bestNormalize object to lock down the normalizing function that
# will be used on repeated runs of the norms. This should be done on TOT score
# or analogous score
TOT_nz_obj <- bestNormalize(data_RS_sim_child_parent$CPTOT_raw)

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

# START HERE: the next snippet successfully collapses the previous two code
# blocks. We are left with the single 6 col df containing the six normalized
# z-scores per case. The next step is to mutate 6 new columns containing the
# normalized T-scores per case. The question for automation is: should that
# operation be performed on the list of 6 single col dfs, leading to a list of 6
# two-cols dfs that can then be bound, or should it be done on the single df
# with 6 cols?
nzScore_perCase <- raw_score_cols_list %>% 
  map(~ get(chosen_transform)(.x)) %>% 
  map(~ data.frame(pluck(.x, "x.t"))) %>% 
  bind_cols() %>% 
  set_names(str_c(scale_suffix, "_nz")) 


