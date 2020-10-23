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

# Create a list containing scale item vectors for each form (needed for subscale alphas)

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
  pull(item_names, scale_items) 

# Create list of dfs containing subscale item scores for all persons
