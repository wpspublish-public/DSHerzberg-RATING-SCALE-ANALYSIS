# Load packages, read data, specify input parameters

suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(psych))

urlRemote_path  <- "https://raw.githubusercontent.com/"
github_path <- "DSHerzberg/RATING-SCALE-ANALYSIS/master/INPUT-FILES/"

data_RS_sim_child_parent <- suppressMessages(read_csv(url(
  str_c(urlRemote_path, github_path, "data-RS-sim-child-parent.csv")
)))
data_RS_sim_child_teacher <- suppressMessages(read_csv(url(
  str_c(urlRemote_path, github_path, "data-RS-sim-child-teacher.csv")
)))
data_RS_sim_teen_parent <- suppressMessages(read_csv(url(
  str_c(urlRemote_path, github_path, "data-RS-sim-teen-parent.csv")
)))
data_RS_sim_teen_teacher <- suppressMessages(read_csv(url(
  str_c(urlRemote_path, github_path, "data-RS-sim-teen-teacher.csv")
)))

# Initialize containers for form, scale abbreviations
form_acronyms <- c("cp", "ct", "tp", "tt")

scale_items_suffix <- c("S1", "S2", "S3", "S4", "S5", "TOT")

form_scale_cols <-
  crossing(str_to_upper(form_acronyms), scale_items_suffix) %>%
  set_names(c("form", "scale")) 

# recode the input data to numerical coding for the item responses, and hold the
# resulting data frames in a list.

input_recode_list <- map(
  lst(
    data_RS_sim_child_parent,
    data_RS_sim_child_teacher,
    data_RS_sim_teen_parent,
    data_RS_sim_teen_teacher
  ),
  ~
    .x %>%
    mutate(
      across(
        contains("cpi") |
          contains("cti") | contains("tpi") | contains("tti"),
        ~ case_when(
          .x == "never" ~ 1,
          .x == "occasionally" ~ 2,
          .x == "frequently" ~ 3,
          .x == "always" ~ 4
        )
      )
    )
)

# Create a list containing the four TOT scale item name vectors.
TOT_item_names_list <- map(form_acronyms,
                           ~
                             str_c(str_c(.x, "i"), str_pad(
                               as.character(1:50), 2, side = "left", pad = "0"
                             )))

# define a function to subset a vector by numerical positions.
nth_element <- function(vector, starting_position, interval) { 
  vector[seq(starting_position, length(vector), interval)] 
}

# Apply nth_element() within a nested mapping structure to obtain a list with 24
# vectors containing item column names needed for TOT and subscale alphas.
scale_item_vectors <- map(TOT_item_names_list,
                          ~ splice(map(1:5, ~ nth_element(.y, .x, 5), .y = .x), .x)) %>%
  flatten()

# Create data frame with a list column including the subsetted item columns
# required for internal consistency anlysis of each scale.
scale_item_data <- tibble(
  data = rep(input_recode_list,
             each = 6),
  item_names = scale_item_vectors) %>% 
  bind_cols(form_scale_cols) %>% 
  mutate(items = map2(data, item_names, ~ .x %>% select(all_of(.y))))

# for output table, extract n's, scale, mean, SDs from input data sets.
scale_n_mean_sd <- map_df(
  lst(
    data_RS_sim_child_parent,
    data_RS_sim_child_teacher,
    data_RS_sim_teen_parent,
    data_RS_sim_teen_teacher
  ),
  ~
    .x %>%
    select(contains("raw")) %>%
    describe(fast = T) %>%
    rownames_to_column() %>%
    rename(scale_name = rowname) %>%
    mutate(
      form = str_sub(scale_name, 1, 2),
      scale = str_sub(scale_name, 3,-5)
    ) %>%
    select(form, scale, n, mean, sd)
) 

# Create alpha output table with SEM, CVs, using `psych::alpha()`. Unnest the
# alpha column to get those data frames flattened out into columns.
alpha_output <- scale_item_data %>%
  mutate(alpha = map(items, ~ alpha(cor(.x))[["total"]])) %>%
  unnest(alpha) %>%
  select(form, scale, raw_alpha) %>%
  rename(alpha = raw_alpha) %>%
  left_join(scale_n_mean_sd, by = c("form", "scale")) %>%
  group_by(form) %>%
  mutate(
    SEM = sd * (sqrt(1 - alpha)),
    CV_90_UB = round(mean + 1.6449 * SEM),
    CV_90_LB = round(mean - 1.6449 * SEM),
    CV_95_UB = round(mean + 1.96 * SEM),
    CV_95_LB = round(mean - 1.96 * SEM),
    CI_90 = str_c(CV_90_LB, "--", CV_90_UB),
    CI_95 = str_c(CV_95_LB, "--", CV_95_UB),
    across(is.numeric, ~ round(., 2)),
    form = case_when(row_number() == 1 ~ form,
                     T ~ NA_character_),
    n = case_when(row_number() == 1 ~ n,
                  T ~ NA_real_)
  ) %>%
  select(form, n, scale, alpha, SEM, CI_90, CI_95) 

write_csv(alpha_output,
          here("OUTPUT-FILES/TABLES/alpha-summary-by-form.csv"),
          na = "")
