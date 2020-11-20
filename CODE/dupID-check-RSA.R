# Load packages, read data

suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))

urlRemote_path  <- "https://raw.githubusercontent.com/"
github_path <- "DSHerzberg/RATING-SCALE-ANALYSIS/master/INPUT-FILES/"

data_RS_sim_child_parent <- suppressMessages(read_csv(url(
  str_c(urlRemote_path, github_path, "data-RS-sim-child-parent.csv")
)))

# recode four cases to have missing ID numbers, four DUP IDS
miss_dup <- data_RS_sim_child_parent %>%
  mutate(across(ID,
                ~
                  case_when(
                    row_number() %in% c(10, 35, 579, 991) ~ lag(.x),
                    row_number() %in% c(1, 208, 414, 793) ~ NA_real_,
                    T ~ .x
                  )))

# returns row number of FIRST dup ID encountered, or returns 0 if no dups
anyDuplicated(miss_dup$ID)

# Check for any NAs on IDNumber, returns TRUE if NA exist
any(is.na(miss_dup$ID))

# extract cases with Dup ID numbers or NA on IDNumber, write out for investigation
data_RS_sim_child_parent_dupMissIDs <- miss_dup %>% 
  mutate(dup = duplicated(ID)) %>% 
  filter(dup == TRUE | is.na(ID)) %>% 
  select(-dup) 

# NEXT DO YOU WANT SEPARATE REPORTS FOR DUP AND MISSING, DO YOU WANT THE PAIR OF
# CASES FOR EACH DUP ID?
