# load packages, read data
suppressMessages(library(here)) 
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(psych))
suppressMessages(library(MatchIt))

# use spm2 data, not RSA sim data

stand_preMatch <- 
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/data-RS-spm2-stand-all-T-scores-per-case.csv")
  )))

clin_ASD_preMatch <-
  suppressMessages(as_tibble(read_csv(
    here("INPUT-FILES/data-RS-spm2-clin-all-T-scores-per-case.csv")
  ))) %>% 
  filter(clin_dx == "ASD")

# EXTRACT MATCHED TYPICAL SAMPLE ------------------------------------------

# This step encodes a logical var (Group), needed by matchit, that captures clin
# vs typ status
ASD_clin_stand_preMatch <- bind_rows(
  clin_ASD_preMatch,
  stand_preMatch 
) %>% 
  mutate(Group = case_when(
    clin_status == 'clin' ~ TRUE,
    TRUE ~ FALSE
  ))

# matchit cannot process NA. First get sum of NA for data. If that is 0,
# proceed. If sum NA is positive, recode all NA to 999
sum(is.na(ASD_clin_stand_preMatch))

# identify cols with NA
na_cols <- ASD_clin_stand_preMatch %>% select_if(~ any(is.na(.)))

# in NA cols, replace NA with 999. Note that we need a separate call of mutate
# () to replace_na on HighestEducation, because that var is logical on the input
# file, and replace_na can't convert variable types on the fly, so we have to
# coerce to as.character() to replace NA with the string "999"
ASD_clin_stand_preMatch <- ASD_clin_stand_preMatch %>%
  replace_na(
    list(
      IDNumber = 999,
      AgeInMonths = 999,
      Age = 999,
      clin_dx = "999",
      Region = "999"
    )) %>% 
  mutate(HighestEducation = replace_na(as.character(HighestEducation), "999"))

# run matchit to get 1:1 matching
set.seed(12345)
match <- matchit(
  Group ~ age_range + Gender + ParentHighestEducation + Ethnicity, 
  data = ASD_clin_stand_preMatch, 
  method = "nearest", 
  ratio = 1)
match_summ <- summary(match)

# save matched samples into new df; split by clin_status
ASD_clin_stand_match <- match.data(match) %>% 
  select(-Group, -distance, -weights) #%>% 
ASD_stand_match <- ASD_clin_stand_match %>% 
  filter(clin_status == 'typ')
ASD_clin_match <- ASD_clin_stand_match %>% 
  filter(clin_status == 'clin')

# demo counts: setting up labels and values for the output tables.
var_order <- c("age_range", "Age", "AgeInMonths", "Gender", "ParentHighestEducation", "HighestEducation", 
               "Ethnicity", "Region")
cat_order <- c(
  # AgeInMonths
  NA, '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', 
  '23', '24', '25', '26', '27', '28', '29', '30', '31', '32', '33', '34', '35', '36',
  # age_range
  NA, "03.5 to 6 mo", "03.5 to 10 mo", "4 to 10 mo", "07 to 10.5 mo", "09.5 to 20 mo",  
  "11 to 20 mo", "11 to 31.5 mo", "21 to 30 mo",
  "21 to 31.5 mo", "2 to 4 years", "5 years", "5 to 8 years", "9 to 12 years", "12 to 13 years", 
  "14 to 15 years", "16 to 17 years", "18 to 21 years", "21.00 to 30.99 years", "31.00 to 40.99 years", 
  "41.00 to 50.99 years", "51.00 to 64.99 years", "65.00 to 99.99 years",
  # Age
  "2", "3", "4", "5",
  # Gender
  NA, "Male", "Female",
  # ParentHighestEducation & HighestEducation
  NA, "Did not complete high school (no diploma)", "High school graduate (including GED)", 
  "Some college or associate degree", "Bachelor's degree or higher",
  # Ethnicity
  NA, "Hispanic", "Asian", "Black", "White", "AmericanIndAlaskanNat", 
  "NativeHawPacIsl", "MultiRacial", "Other",
  # Region
  NA, "northeast", "midwest", "south", "west")

match_dist_stand <- ASD_stand_match %>% 
  select(age_range, Gender, ParentHighestEducation, Ethnicity) %>% 
  gather("Variable", "Category") %>% 
  group_by(Variable, Category) %>%
  count(Variable, Category) %>%
  arrange(match(Variable, var_order), match(Category, cat_order)) %>% 
  ungroup() %>% 
  mutate(Variable = case_when(
    lag(Variable) == "age_range" & Variable == "age_range" ~ "",
    lag(Variable) == "Gender" & Variable == "Gender" ~ "",
    lag(Variable) == "ParentHighestEducation" & Variable == "ParentHighestEducation" ~ "",
    lag(Variable) == "Ethnicity" & Variable == "Ethnicity" ~ "",
    TRUE ~ Variable
  )) %>% 
  mutate(group = case_when(
    rownames(.) == "1" ~ 'Matched typical',
    T ~ NA_character_
  )) %>% 
  select(group, everything())

match_dist_clin <- ASD_clin_match %>% 
  select(age_range, Gender, ParentHighestEducation, Ethnicity) %>% 
  gather("Variable", "Category") %>% 
  group_by(Variable, Category) %>%
  count(Variable, Category) %>%
  arrange(match(Variable, var_order), match(Category, cat_order)) %>% 
  ungroup() %>% 
  mutate(Variable = case_when(
    lag(Variable) == "age_range" & Variable == "age_range" ~ "",
    lag(Variable) == "Gender" & Variable == "Gender" ~ "",
    lag(Variable) == "ParentHighestEducation" & Variable == "ParentHighestEducation" ~ "",
    lag(Variable) == "Ethnicity" & Variable == "Ethnicity" ~ "",
    TRUE ~ Variable
  )) %>% 
  mutate(group = case_when(
    rownames(.) == "1" ~ 'ASD',
    T ~ NA_character_
  )) %>% 
  select(group, everything())

# write table of combined matched typical, clinical demo counts.
match_dist <- bind_rows(
  match_dist_clin,
  match_dist_stand
)

# COMPARE MATCHED SAMPLES ON T-SCORES -------------------------------------

# Extract match cases from stand sample
ASD_matchStand <- stand_preMatch %>% 
  semi_join(ASD_stand_match, by ='IDNumber')

scale_order <- c("SOC", "VIS", "HEA", "TOU", 
                 "TS", "BOD", "BAL", "PLA", "TOT")

# Write matched typical t-score descriptives
ASD_matchStand_t_desc <-
  ASD_matchStand %>% 
  select(contains('_NT')) %>% 
  rename_with(~ str_sub(., 1, -4), everything()) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(sample = case_when(
    rownames(.) == "1" ~ 'Matched typical',
    T ~ NA_character_
  ),
  var = sd^2) %>% 
  select(sample, scale, n, mean, sd, var) %>% 
  rename(n_typ = n,
         mean_typ = mean,
         sd_typ = sd,
         var_typ = var)

# Write clinical t-score descriptives
ASD_clin_t_desc <-
  clin_ASD_preMatch %>% 
  select(contains('_NT')) %>% 
  rename_with(~ str_sub(., 1, -4), everything()) %>% 
  describe(fast = T) %>%
  rownames_to_column() %>% 
  rename(scale = rowname)%>% 
  arrange(match(scale, scale_order)) %>% 
  mutate(sample = case_when(
    rownames(.) == "1" ~ 'ASD',
    T ~ NA_character_
  ),
  var = sd^2) %>% 
  select(sample, scale, n, mean, sd, var) %>% 
  rename(n_clin = n,
         mean_clin = mean,
         sd_clin = sd, 
         var_clin = var)

# Combine stand, clin columns, add ES column based on correct formula for Cohen's d

ASD_match_t_desc <- left_join(ASD_matchStand_t_desc,
                              ASD_clin_t_desc, by = "scale") %>%
  mutate(ES = abs((mean_typ - mean_clin) / sqrt(((n_typ * var_typ) + (n_clin * var_clin)) / (n_typ + n_clin))),
         across(c(mean_typ, sd_typ, mean_clin, sd_clin, ES), ~
                  (round(., 2)))) %>%
  select(everything(), -sample.x, -sample.y, -var_typ, -var_clin)

###### WRITE MANUAL TABLE OUTPUT -----------------------------------------------------

write_csv(match_dist,
          here(
              "OUTPUT-FILES/TABLES/demos-ASD-matchTyp.csv"
          ),
          na = '')

# write table comping t-score descriptives with ES
write_csv(ASD_match_t_desc,
          here(
            "OUTPUT-FILES/TABLES/raw-desc-ES-ASD-matchTyp.csv"
          ),
          na = '')

