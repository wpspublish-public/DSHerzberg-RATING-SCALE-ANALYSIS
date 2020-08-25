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
          scale_y_continuous(breaks = seq(0, 1000, 50)) +
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

plots <- temp1$plots

temp2 <- ggpubr::ggarrange(plotlist = plots, ncol = 2, nrow = 2)

ggpubr::ggexport(temp2, filename = here("PLOTS/temp2.pdf"))

vec <- unique(temp1$file)

temp3 <- map(unique(temp1$file), ~ temp1 %>% filter(file == .x))

temp4 <- map(temp3, ~ ggpubr::ggarrange(plotlist = plots, ncol = 2, nrow = 2))

# NEXT: figure out why next line is yielding four separate pdfs, each containing
# all the graphs, instead of only the 6 that should go in that pdf

map2(temp4, vec, ~ ggpubr::ggexport(.x, filename = here(str_c("PLOTS/", .y, ".pdf"))))
