# Load packages, read data, specify input parameters

suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(psych))
library(ggrepel)
library(ggpubr)

urlRemote_path  <- "https://raw.github.com/"
github_path <- "wpspublish/DSHerzberg-RATING-SCALE-ANALYSIS/master/INPUT-FILES/"

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

# Item response frequencies: Tables

list(mget(str_c("data_RS_sim_", data_name_suffix)),
     item_cols,
     item_cats,
     data_name_suffix) %>%
  pmap(
    ~ ..1 %>%
      select(!!..2) %>%
      pivot_longer(everything(), names_to = 'item', values_to = 'value') %>%
      count(item, value) %>%
      pivot_wider(names_from = value, values_from = n) %>%
      arrange(match(item, !!..2)) %>%
      mutate(data = case_when(rownames(.) == "1" ~ ..4,
                              T ~ NA_character_)) %>%
      select(data, item, !!..3)
  ) %>%
  set_names(str_c("freq_item_val_", data_name_suffix)) %>%
  iwalk(~ write_csv(.x, here(
    str_c("OUTPUT-FILES/TABLES/",
          str_replace_all(.y, "_", "-"),
          ".csv")
  ),
  na = "")) %>%
  list2env(envir = .GlobalEnv)

# Demographic Frequency Counts: Tables

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
          lag(var) == "clin_status" &
            var == "clin_status" ~ NA_character_,
          TRUE ~ var
        ),
        data = case_when(rownames(.) == "1" ~ ..2,
                         T ~ NA_character_)
      ) %>%
      select(data, var, cat, n)
  ) %>%
  set_names(str_c("freq_demos_", data_name_suffix)) %>%
  iwalk(~ write_csv(.x, here(
    str_c("OUTPUT-FILES/TABLES/",
          str_replace_all(.y, "_", "-"),
          ".csv")
  ),
  na = "")) %>%
  list2env(envir = .GlobalEnv)

# Demographic Frequency Counts: Histograms

map(mget(str_c("freq_demos_", data_name_suffix)), ~ .x %>% 
   fill(c(data, var))) %>%
    list2env(envir = .GlobalEnv)

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
          scale_y_continuous(breaks = seq(0, max(.x$n), round(max(.x$n) / 10, -1))) +
          labs(subtitle = str_c(
            "Demo Counts - ",
            str_replace(str_sub(.y
                                , 12), "_", " form, "),
            " report"
          )) +
          scale_x_discrete(limits = .x$cat) +
          theme(panel.grid.minor = element_blank(),
                axis.title.x = element_blank())
      )
  ))

hist_plot_prep <- map(
  map(unique(hist_list$file), ~ hist_list %>% filter(file == .x)), 
  ~ ggpubr::ggarrange(plotlist = .x$plots, ncol = 2, nrow = 2))

walk2(hist_plot_prep,
      unique(hist_list$file),
      ~ ggpubr::ggexport(.x, filename = here(
        str_c("OUTPUT-FILES/PLOTS/", str_replace_all(.y, "_", "-"),
              ".pdf")
      )))

# raw score descriptives: tables

list(mget(str_c("data_RS_sim_", data_name_suffix)),
     data_name_suffix) %>%
  pmap(
    ~ ..1 %>%
      select(contains('raw')) %>%
      describe(fast = T) %>%
      rownames_to_column(var = "scale") %>%
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

iwalk(mget(str_c("raw_desc_", data_name_suffix)),
      ~ write_csv(.x, here(
        str_c("OUTPUT-FILES/TABLES/",
              str_replace_all(.y, "_", "-"),
              ".csv")
      ),
      na = ""))

# raw score descriptives: plots

map(mget(str_c("raw_desc_", data_name_suffix)), ~ .x %>% 
      fill(data)) %>%
  list2env(envir = .GlobalEnv)

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
      scale_y_continuous(breaks = seq(0, round(max(.x$mean) + (2 * max(.x$sd)), -1), 
                                      by = round(max(.x$sd), -1) / 2), 
                         limits = c(0, round(max(.x$mean) + (2 * max(.x$sd))), -1)) +
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
