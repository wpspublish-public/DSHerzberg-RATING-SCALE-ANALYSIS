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

# # apply the chosen method to create normalized z-scores for each case.
# `bass::get()` forces R to evaluate chosen_transform, a char vector, as the
# name of a function
TOT_nz_transform <- get(chosen_transform)(data_RS_sim_child_parent$CPTOT_raw)

# create char vec with names for the nine score transformations
nz_transform_names <- str_c(scale_suffix, '_nz_transform')

# pull 6 raw score columns into a list
raw_score_cols_list <-
  map(str_c("CP", scale_suffix),
      ~ data_RS_sim_child_parent %>%
        pull(!!rlang::sym(str_c(.x, "_raw")))) %>% 
  set_names(str_c("CP", scale_suffix, "_raw"))




