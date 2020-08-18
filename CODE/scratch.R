suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(psych))

test <- bind_rows(
  # note use of tibble::lst() to create a list with elements named with the
  # names of the input objects
  lst(
    raw_desc_child_parent,
    raw_desc_child_teacher,
    raw_desc_teen_parent,
    raw_desc_teen_teacher
    ),
  .id = "source"
)

test_list <-   
  lst(
  raw_desc_child_parent,
  raw_desc_child_teacher,
  raw_desc_teen_parent,
  raw_desc_teen_teacher
)


mean_plot <- test_list %>%
  map(~
  ggplot(., aes(scale, mean)) +
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
  ) +
  facet_grid(vars(data))
  )
mean_plot[[1]]


# mean_plot <- test %>%
#   ggplot(aes(scale, mean)) +
#   geom_point(
#     col = "blue",
#     fill = "blue",
#     alpha = .5,
#     size = 3,
#     shape = 23
#   ) +
#   geom_label_repel(aes(label = mean), hjust = .7, vjust = -1, label.padding = unit(0.1, "lines"), size = 4, col = "blue") +
#   scale_y_continuous(breaks = seq(0, 15, by = 1), limits = c(0,15)) +
#   labs(title = "Raw Score Means (with SDs)", x = "Scale", y = "Raw Score Mean") +
#   geom_errorbar(
#     aes(ymin = mean - sd, ymax = mean + sd),
#     col = "red",
#     size = 0.2,
#     width = 0.2
#   ) +
#   facet_grid(vars(source))
# print(mean_plot)
