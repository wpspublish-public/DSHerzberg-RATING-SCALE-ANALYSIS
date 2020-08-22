suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(psych))

demo_table_input <- lst(freq_demos_child_parent, freq_demos_child_teacher)

# next code block implements list-column workflow to get desired histograms for
# single freq_demos table, next challenge is to interate this over list of
# freq_demo tables
temp3 <-
  tibble(file = deparse(substitute(freq_demos_child_parent)),
         var = map(var_order, ~ freq_demos_child_parent %>% filter(var == .x))) %>%
  mutate(plots = map(
    var,
    ~ ggplot(data = .x, aes(cat, n)) +
      geom_col(col = "red",
               fill = "blue",
               alpha = .2) +
      scale_y_continuous(breaks = seq(0, 1000, 50)) +
      labs(title = str_c(
        "Demo Counts - ",
        str_replace(str_sub(
          file
          , 12), "_", " form, "),
        " report"
      )) +
      scale_x_discrete(limits = .x %>% pull(cat)) +
      theme(panel.grid.minor = element_blank(),
            axis.title.x = element_blank()) +
      facet_wrap(vars(var))
  ))


temp1 <- tibble(file = names(demo_table_input), data1 = demo_table_input) %>% 
  mutate(plots = map(data1, 
          ~ ggplot(data = .x, aes(cat, n)) +
            geom_col(col = "red",
                     fill = "blue",
                     alpha = .2) +
            scale_y_continuous(breaks = seq(0, 1000, 50)) +
            # labs(title = str_c(
            #   "Demo Counts - ",
            #   str_replace(str_sub(
            #     names(.x)
            #     , 12), "_", " form, "),
            #   " report"
            # )) +
            # scale_x_discrete(limits = .x %>% pull(cat))) +
            theme(panel.grid.minor = element_blank(),
                  axis.title.x = element_blank()) +
            facet_wrap(vars(var))#,
          # .y = .x
      )
)

demo_hist <- map(
  demo_table_input,
  ~
    var_order %>%
    map(
      ~ ggplot(data = (.y %>% filter(var == .x)), aes(cat, n)) +
        geom_col(col = "red",
                 fill = "blue",
                 alpha = .2) +
        scale_y_continuous(breaks = seq(0, 1000, 50)) +
        labs(title = str_c(
          "Demo Counts - ",
          str_replace(str_sub(
            names(.y)
          , 12), "_", " form, "),
          " report"
        )) +
        scale_x_discrete(limits = (.y %>% filter(var == .x) %>% pull(cat))) +
        theme(panel.grid.minor = element_blank(),
              axis.title.x = element_blank()) +
        facet_wrap(vars(var)),
      .y = .x
    )
)
demo_hist

# demo_hist <- var_order %>%
#   map(
#     ~ ggplot(data = (
#       freq_demos_child_parent %>% filter(var == .x)
#     ), aes(cat, n)) +
#       geom_col(col = "red",
#                fill = "blue",
#                alpha = .2) +
#       scale_y_continuous(breaks = seq(0, 1000, 50)) +
#       labs(
#         title = str_c(
#           "Demo Counts - ",
#           str_replace(str_sub(deparse(quote(freq_demos_child_parent)), 12), "_", " form, "),
#           " report"
#         )) +
#       scale_x_discrete(limits = (
#         freq_demos_child_parent %>% filter(var == .x) %>% pull(cat)
#       )) +
#       theme(panel.grid.minor = element_blank(),
#             axis.title.x = element_blank()) +
#       facet_wrap(vars(var))
#   )
# demo_hist

imap(
  .x = lst(
    freq_demos_child_parent,
    freq_demos_child_teacher,
    freq_demos_teen_parent,
    freq_demos_teen_teacher
  ),
  ~ print(
    ggplot(.x, aes(cat, n)) +
      geom_col(
        col = "red",
        fill = "blue",
        alpha = .2
      ) +
      scale_y_continuous(breaks = seq(0, 1000, 50)) +
      labs(title = "Frequency Distribution") + 
      theme(panel.grid.minor=element_blank(),
            axis.title.x=element_blank()) +
      facet_wrap(vars(var))
  )
) %>%
  imap( ~ ggsave(plot = .x, file = here(str_c("PLOTS/", .y, ".pdf"))))





