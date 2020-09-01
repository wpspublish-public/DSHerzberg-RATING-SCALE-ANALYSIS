suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(psych))
library(ggrepel)
library(ggpubr)

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

data_name_suffix <- c("child_parent", "child_teacher", "teen_parent", "teen_teacher")
item_col_prefix <- c("cp", "ct", "tp", "tt")
scale_col_prefix <- toupper(item_col_prefix)
var_order <- c("age_range", "gender", "educ", "ethnic", "region", "clin_status")
cat_order <- c(
  "5 to 8 yo", "9 to 12 yo", "13 to 15 yo", 
  "16 to 18 yo",
  "male", "female",
  "no_HS","HS_grad", "some_college", "BA_plus", 
  "hispanic","asian", "black", "white", "other", 
  "northeast","south", "midwest", "west",
  "typ", "clin", 
  "never", "occasionally","frequently", "always"
)

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
      pivot_longer(everything(), names_to = 'item', values_to = 'value') %>%
      count(item, value) %>%
      # `pivot_wider` is an updated version of `spread`
      pivot_wider(names_from = value, values_from = n) %>%
      arrange(match(item,!!..2)) %>%
      # `..3` refers to `item_cats`, the list containing the char vecs that
      # name the item response categories for each input data set.
      mutate(data = case_when(rownames(.) == "1" ~ ..4,
                              T ~ NA_character_)) %>%
      select(data, item,!!..3)
  ) %>%
  # trying to reduce my use of `assign()` to place data objecds intop the global
  # environment. Let `pmap()` output a list of dfs `freq_item_val_tables`, then
  # use `set_names()` to give each df the desired name
  set_names(str_c("freq_item_val_", data_name_suffix)) %>%
  
  # `list2env()` extracts the data frames from the list into the global environment
  list2env(envir = .GlobalEnv)

# use iwalk() to iterate over a list of four data frames and save them to csv.
# "i" so we can map a function to both the data frames (.x) and their names(.y).
# "walk" because we don't need the mapping operation to print the data frames to
# the console. What we care about is the "side effect" of the data frames being
# saved as .csv.
iwalk(mget(str_c("freq_item_val_", data_name_suffix)),
      ~ write_csv(.x, here(
        str_c("OUTPUT-FILES/TABLES/",
              str_replace_all(.y, "_", "-"),
              ".csv")
      ),
      na = ""))


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
          lag(var) == "age_range" & var == "age_range" ~ NA_character_,
          lag(var) == "gender" & var == "gender" ~ NA_character_,
          lag(var) == "educ" & var == "educ" ~ NA_character_,
          lag(var) == "ethnic" & var == "ethnic" ~ NA_character_,
          lag(var) == "region" & var == "region" ~ NA_character_,
          lag(var) == "clin_status" & var == "clin_status" ~ NA_character_,
          TRUE ~ var
        ),
        data = case_when(rownames(.) == "1" ~ ..2,
                         T ~ NA_character_)
        # data = ..2
      ) %>%
      select(data, var, cat, n)
  ) %>%
  set_names(str_c("freq_demos_", data_name_suffix)) %>%
  list2env(envir = .GlobalEnv)

iwalk(mget(str_c("freq_demos_", data_name_suffix)),
      ~ write_csv(.x, here(
        str_c("OUTPUT-FILES/TABLES/",
              str_replace_all(.y, "_", "-"),
              ".csv")
      ),
      na = ""))

# fill NA values of $data with file name, for creation of histograms in next snippet
map(mget(str_c("freq_demos_", data_name_suffix)), ~ .x %>% 
   fill(c(data, var))) %>%
    list2env(envir = .GlobalEnv)


# This snippet creates a data frame with three columns: file: name of the input
# demographic table, by form and reporter var: list-column containing 24 demo
# tables by var x cat plots: list-column containing 24 ggplots (histograms), one
# for each demo table. It prints these 24 histograms to the RStudio plots pane.
hist_list <- lst(
  freq_demos_child_parent,
  freq_demos_child_teacher,
  freq_demos_teen_parent,
  freq_demos_teen_teacher
) %>%
  map( ~
         tibble(var = map(var_order, ~ .y %>% filter(var == .x), .y = .x))) %>%
  tibble(file = names(.), data1 = .) %>%
  unnest(cols = c(data1)) %>%
  mutate(plots = map2(
    var,
    file,
    ~
      print(
        ggplot(data = .x, aes(cat, n)) +
          geom_col(col = "red",
                   fill = "blue",
                   alpha = .2,
                   width = .3) +
          scale_y_continuous(breaks = seq(0, max(.x$n), 50)) +
          labs(subtitle = str_c(
            "Demo Counts - ",
            str_replace(str_sub(.y
                                , 12), "_", " form, "),
            " report"
          )) +
          scale_x_discrete(limits = .x %>% pull(cat)) +
          theme(panel.grid.minor = element_blank(),
                axis.title.x = element_blank()) +
          facet_wrap(vars(var))
      )
  ))

# hist_plot_prep is a list, in which .x argument to the outer map() call is a list
# containing four data frames, hist_list divided into separate dfs for the four
# age_range x rater combos. This list is supplied as the .x argument to the
# inner map() call, which applies ggarrange() to put the plots of plots of the
# .x list into muliple plot per page format suitable for saving as .pdf
hist_plot_prep <- map(
  map(unique(hist_list$file), ~ hist_list %>% filter(file == .x)), 
  ~ ggpubr::ggarrange(plotlist = .x$plots, ncol = 2, nrow = 2))

# this snippet exports and saves the .pdfs.
walk2(hist_plot_prep,
      unique(hist_list$file),
      ~ ggpubr::ggexport(.x, filename = here(
        str_c("OUTPUT-FILES/PLOTS/", str_replace_all(.y, "_", "-"),
              ".pdf")
      )))

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
        # data = ..2,
        across(c(mean, sd),
               ~ round(., 2))
      ) %>%
      select(data, scale, n, mean, sd)
  ) %>%
  set_names(str_c("raw_desc_", data_name_suffix)) %>%
  list2env(envir = .GlobalEnv)

iwalk(mget(str_c("raw_desc_", data_name_suffix)),
      ~ write_csv(.x, here(
        str_c("OUTPUT-FILES/TABLES/",
              str_replace_all(.y, "_", "-"),
              ".csv")
      ),
      na = ""))

map(mget(str_c("raw_desc_", data_name_suffix)), ~ .x %>% 
      fill(data)) %>%
  list2env(envir = .GlobalEnv)

# print and save plots of raw score means, SDs by scale
imap(
  .x = lst(
    raw_desc_child_parent,
    raw_desc_child_teacher,
    raw_desc_teen_parent,
    raw_desc_teen_teacher
  ),
  ~ print(
    ggplot(.x, aes(scale, mean)) +
      geom_point(
        col = "blue",
        fill = "blue",
        alpha = .5,
        size = 3,
        shape = 23
      ) +
      geom_label_repel(
        aes(label = mean),
        hjust = .7,
        vjust = -1,
        label.padding = unit(0.1, "lines"),
        size = 4,
        col = "blue"
      ) +
      scale_y_continuous(breaks = seq(0, 15, by = 1), limits = c(0, 15)) +
      labs(
        title = str_c(
          "Raw Score Means (with SDs) - ",
          str_replace(str_sub(.y, 10), "_", " form, "),
          " report"
        ),
        x = "Scale",
        y = "Raw Score Mean"
      ) +
      geom_errorbar(
        aes(ymin = mean - sd, ymax = mean + sd),
        col = "red",
        size = 0.2,
        width = 0.2
      )
  )
) %>%
  iwalk(~ ggsave(plot = .x, file = here(
    str_c("OUTPUT-FILES/PLOTS/",
          str_replace_all(.y, "_", "-"), ".pdf")
  )))
