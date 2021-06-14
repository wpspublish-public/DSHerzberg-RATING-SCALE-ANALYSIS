suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))

urlRemote_path  <- "https://raw.github.com/"
github_path <- "wpspublish/DSHerzberg-RATING-SCALE-ANALYSIS/master/INPUT-FILES/"

data_RS_sim_child_parent <- suppressMessages(read_csv(url(
  str_c(urlRemote_path, github_path, "data-RS-sim-child-parent.csv")
)))

anyDuplicated(miss_dup$ID)
any(is.na(miss_dup$ID))

data_RS_sim_child_parent_dupMissIDs <- miss_dup %>%
  arrange(ID) %>% 
  mutate(dup = duplicated(ID)) %>%
  filter(dup == T | lead(dup) == T | is.na(ID)) %>%
  select(-dup)

write_csv(data_RS_sim_child_parent_dupMissIDs,
  here(
    str_c(
      'OUTPUT-FILES/DUP-IDS/data-RS-sim-child-parent-dupIDs-missingIDs-',
      format(Sys.Date(), "%Y-%m-%d"),
      '.csv'
    )
  ), 
  na = 'missing'
)

