suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(psych))

# read data from remote URL
urlRemote_path  <- "https://raw.githubusercontent.com/"
github_path <- "DSHerzberg/RATING-SCALE-ANALYSIS/master/INPUT-FILES/"

data_input_sim <- suppressMessages(read_csv(url(
  str_c(urlRemote_path, github_path, "data-input-sim.csv")
)))
data_input_bfi <- suppressMessages(read_csv(url(
  str_c(urlRemote_path, github_path, "data-input-bfi.csv")
)))


# extract elements unique to each data set: name suffix, item cols, item cats
data_name_suffix <- c("sim", "bfi")

sim_item_cols <- str_c("i", str_pad(as.character(1:50), 2, side = "left", pad = "0"))
bfi_item_cols <- cross(list(c("A", "C", "E", "N", "O"), seq(1:5))) %>%
  map_chr(str_c, collapse = "") %>% 
  sort()

sim_item_cats <- c("never", "occasionally","frequently", "always")
bfi_item_cats <- c("very_inaccurate", "moderately_inaccurate", "slightly_inaccurate",
                   "slightly_accurate", "moderately_accurate", "very_accurate")

# make list containing item cols, item cats for each data set
item_cols <- list(sim_item_cols, bfi_item_cols)
item_cats <- list(sim_item_cats, bfi_item_cats)

# item val freq tables

# akrun solution mget() returns the two data objects whose string names are
# contained within str_c(). These objects are put into a list, to be one of the
# inputs that pmap() iterates over. Now all three inputs to pmap() are lists
# containing named objects
list(mget(str_c("data_input_", data_name_suffix)),
     item_cols,
     item_cats,
     data_name_suffix) %>%
  # ..1 etc is how to refer to multiple inputs in pmap(), Here `..1` refers to
  # the list containing the input data frames for which we are getting freq
  # counts.
  pmap(
    ~ ..1 %>%
      # `..2` refers to `item_cols`, the list containing the char vecs that
      # name the item columns in each input data set. Because these names are
      # coming into a dplyr function as quoted names, they need to be
      # unquoted with `!!`
      select(!!..2) %>%
      # `pivot_longer` is an updated version of `gather`
      pivot_longer(everything(), names_to = 'var', values_to = 'value') %>%
      count(var, value) %>%
      # `pivot_wider` is an updated version of `spread`
      pivot_wider(names_from = value, values_from = n) %>%
      arrange(match(var,!!..2)) %>%
      # `..3` refers to `item_cats`, the list containing the char vecs that
      # name the item response categories for each input data set.
      mutate(data = case_when(rownames(.) == "1" ~ ..4,
                              T ~ NA_character_)) %>%
      select(data, var,!!..3)
  ) %>%
  # trying to reduce my use of `assign()` to place data objecds intop the global
  # environment. Let `pmap()` output a list of dfs `freq_item_val_tables`, then
  # use `set_names()` to give each df the desired name
  set_names(str_c("freq_item_val_", data_name_suffix)) %>%
  
  # `list2env()` extracts the data frames from the list into the global environment
  list2env(envir = .GlobalEnv)

# demo tables
var_order <- c("age_range", "gender", "educ", "ethnic", "region", "clin_status")

cat_order <- c(
  # age_range
  "5 to 8 yo", "9 to 12 yo", "18 yo or younger", 
  "19 to 24 yo", "25 to 39 yo", "40 yo or older",
  # gender
  "male", "female",
  # educ
  "no_HS","HS_grad", "some_college", "BA_plus", 
  # ethnic
  "hispanic","asian", "black", "white", "other", 
  # region
  "northeast","south", "midwest", "west",
  # clin_status
  "typ", "clin", 
  # items
  "never", "occasionally","frequently", "always"
)

list(mget(str_c("data_input_", data_name_suffix)),
     data_name_suffix) %>%
  pmap(
    ~ ..1 %>%
      select(all_of(var_order)) %>%
      pivot_longer(everything(), names_to = 'var', values_to = 'cat') %>%
      count(var, cat) %>%
      arrange(match(var, var_order), match(cat, cat_order)) %>%
      mutate(
        var = case_when(
          lag(var) == "age_range" & var == "age_range" ~ "",
          lag(var) == "gender" & var == "gender" ~ "",
          lag(var) == "educ" & var == "educ" ~ "",
          lag(var) == "ethnic" & var == "ethnic" ~ "",
          lag(var) == "region" & var == "region" ~ "",
          lag(var) == "clin_status" & var == "clin_status" ~ "",
          TRUE ~ var
        ),
        data = case_when(rownames(.) == "1" ~ ..2,
                         T ~ NA_character_)
      ) %>%
      select(data, var, cat, n)
  ) %>%
  set_names(str_c("freq_demos_", data_name_suffix)) %>%
  list2env(envir = .GlobalEnv)

# raw score descriptives tables
list(mget(str_c("data_input_", data_name_suffix)),
     data_name_suffix) %>%
  pmap(
    ~ ..1 %>%
      select(contains('raw')) %>%
      describe(fast = T) %>%
      rownames_to_column() %>%
      rename(scale = rowname) %>%
      mutate(
        data = case_when(rownames(.) == "1" ~ ..2,
                         T ~ NA_character_),
        across(c(mean, sd),
               ~ round(., 2))
      ) %>%
      select(data, scale, n, mean, sd)
  ) %>%
  set_names(str_c("raw_desc_", data_name_suffix)) %>%
  list2env(envir = .GlobalEnv)


rm(list = setdiff(ls(), ls(pattern = "_bfi|_sim")))
