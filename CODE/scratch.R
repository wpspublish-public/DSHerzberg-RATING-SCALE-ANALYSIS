suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(psych))

# This code creates a data frame with three columns:
# file: name of the input demographic table, by form and reporter
# var: list-column containing 24 demo tables by var x cat
# plots: list-column containing 24 ggplots (histograms), one for each demo table
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
map2(hist_plot_prep,
     unique(hist_list$file),
     ~ ggpubr::ggexport(.x, filename = here(str_c("PLOTS/", .y, ".pdf"))))
