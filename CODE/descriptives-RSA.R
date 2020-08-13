suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(psych))
library(ggrepel)

# read data from remote URL
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


# extract elements unique to each data set: name suffix, item cols, item cats
data_name_suffix <- c("child_parent", "child_teacher", "teen_parent", "teen_teacher")
item_col_prefix <- c("cp", "ct", "tp", "tt")
scale_col_prefix <- toupper(item_col_prefix)

item_cols <- item_col_prefix %>% 
  map(
    ~ str_c(.x, "i", str_pad(as.character(1:50), 2, side = "left", pad = "0"))
  ) %>% 
  set_names(str_c("item_cols_", data_name_suffix))
    
item_cats <- replicate(
  4, 
  c("never", "occasionally","frequently", "always"), 
  simplify =  F
  )  %>% 
  set_names(str_c("item_cats_", data_name_suffix))

####### START HERE

# akrun solution mget() returns the two data objects whose string names are
# contained within str_c(). These objects are put into a list, to be one of the
# inputs that pmap() iterates over. Now all three inputs to pmap() are lists
# containing named objects
list(mget(str_c("data_RS_sim_", data_name_suffix)),
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
  "5 to 8 yo", "9 to 12 yo", "13 to 15 yo", 
  "16 to 18 yo",
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

list(mget(str_c("data_RS_sim_", data_name_suffix)),
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

# START HERE: ADD HISTOGRAMS FOR DEMO TABLES

# raw score descriptives tables
list(mget(str_c("data_RS_sim_", data_name_suffix)),
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

mean_plot <- raw_desc_child_parent %>% 
  ggplot(aes(scale, mean)) +
  geom_point(
    col = "blue",
    fill = "blue",
    alpha = .5,
    size = 3,
    shape = 23
  ) +
  geom_label_repel(aes(label = mean), hjust = .7, vjust = -1, label.padding = unit(0.1, "lines"), size = 4, col = "blue") +
  scale_y_continuous(breaks = seq(0, 15, by = 1), limits = c(0,15)) +
  labs(title = "Raw Score Means (with SDs)", x = "Scale", y = "Raw Score Mean") +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    col = "red",
    size = 0.2,
    width = 0.2
  ) 
print(mean_plot)
