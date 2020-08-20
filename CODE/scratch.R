suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(psych))

ggplot(data = (freq_demos_child_parent %>% filter(var == "gender")), aes(cat, n)) +
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





