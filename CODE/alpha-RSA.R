# Load packages, read data, specify input parameters

suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(data.table))
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

# Create a list containing four sublists: the scale item vectors for each form
# (needed for subscale alphas)

form_acronyms <- c("cp", "ct", "tp", "tt")

scale_items_suffix <- c("S1", "S2", "S3", "S4", "S5", "TOT")

S1_item_names <- function(x) {
  str_c(str_c(x, "i"), str_pad(as.character(seq(1, 50, by = 5)), 2, side = "left", pad = "0"))
}
S2_item_names <- function(x) {
  str_c(str_c(x, "i"), str_pad(as.character(seq(2, 50, by = 5)), 2, side = "left", pad = "0"))
}
S3_item_names <- function(x) {
  str_c(str_c(x, "i"), str_pad(as.character(seq(3, 50, by = 5)), 2, side = "left", pad = "0"))
}
S4_item_names <- function(x) {
  str_c(str_c(x, "i"), str_pad(as.character(seq(4, 50, by = 5)), 2, side = "left", pad = "0"))
}
S5_item_names <- function(x) {
  str_c(str_c(x, "i"), str_pad(as.character(seq(5, 50, by = 5)), 2, side = "left", pad = "0"))
}
TOT_item_names <- function(x) {
  str_c(str_c(x, "i"), str_pad(as.character(1:50), 2, side = "left", pad = "0")) 
}

list_item_names <- list(S1_item_names, S2_item_names, S3_item_names, 
                        S4_item_names, S5_item_names, TOT_item_names)

scale_item_vectors <- crossing(form_acronyms, scale_items_suffix) %>% 
  mutate(scale_items = str_c(str_to_upper(form_acronyms), scale_items_suffix, "_items"),
         fun_item_names = rep(list_item_names, 4), 
         item_names = invoke_map(fun_item_names, form_acronyms)) %>% 
  select(form_acronyms, scale_items, item_names) %>% 
  group_by(form_acronyms) %>% 
  group_split(.keep = F) %>% 
  map( ~ .x %>% pull(item_names, scale_items)) %>% 
  set_names(form_acronyms)

# Create list of dfs containing subscale item scores for all persons

# The next code block puts the four input datasets into a `data` list-column,
# repeating each 6 times to set up a df with 24 rows. It then puts the 24
# vectors containing the scale item names into an `item_names` list-column. It
# then creates a third list column, `items`, to hold data frames containing only
# the item columns corresponding to each scale. It does this by mapping select()
# onto the `data` and 'item_names` cols.
scale_item_data <- tibble(data = rep(
  lst(
    data_RS_sim_child_parent,
    data_RS_sim_child_teacher,
    data_RS_sim_teen_parent,
    data_RS_sim_teen_teacher
  ),
  each = 6
),
item_names = flatten(scale_item_vectors)) %>%
  mutate(items = map2(data, item_names, ~ .x %>% select(all_of(.y))))


