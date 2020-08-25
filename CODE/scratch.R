suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(psych))

temp1 <- lst(
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
      # print(
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
      # )
  ))

# NOTE: temp3 and temp4 can be combined into a single function, they'll fail the
# identical test because ggplot generate different environments, but plot output
# is identical (test for this)
temp3 <- map(unique(temp1$file), ~ temp1 %>% filter(file == .x))
temp4 <- map(temp3, ~ ggpubr::ggarrange(plotlist = .x$plots, ncol = 2, nrow = 2))

temp5 <- map(
  temp3, 
  ~ ggpubr::ggarrange(plotlist = .x$plots, ncol = 2, nrow = 2))


map2(temp5, unique(temp1$file), ~ ggpubr::ggexport(.x, filename = here(str_c("PLOTS/", .y, ".pdf"))))
