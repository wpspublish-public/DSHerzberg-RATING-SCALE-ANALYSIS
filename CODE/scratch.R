# write table of demographic counts (make vector code robust wherever possible)

var_order <- c("age_range", "gender", "educ", "ethnic", "region")

cat_order <- c(
  # age_range
  str_sort(unique(get(str_c("data", age_range_name, form_name, sep = "_"))$age_range)),
  # gender
  str_sort(unique(get(str_c("data", age_range_name, form_name, sep = "_"))$gender), 
           decreasing = T),
  # educ
  "no_HS", "HS_grad", "some_college", "BA_plus" , 
  # ethnic
  "hispanic", "asian", "black", "white", "other",
  # Region
  "northeast", "midwest", "south", "west")

assign(
  str_c("demo_counts", age_range_name, form_name, sep = "_"),
  get(str_c("data", age_range_name, form_name, sep = "_")) %>%
    select(all_of(var_order)) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "category") %>%
    group_by(variable, category) %>%
    count(variable, category) %>%
    arrange(match(variable, var_order), match(category, cat_order)) %>%
    ungroup() %>%
    mutate(across(
      variable,
      ~
        case_when(lag(.x) == .x ~ NA_character_,
                  T ~ .x)
    ))
)

write_csv(Child_512_Home_demo_counts, here(
  'OUTPUT-FILES/CHILD/DESCRIPTIVES/Child-512-Home-demo-counts.csv'
  # paste0(
  #   'OUTPUT-FILES/CHILD/DESCRIPTIVES/Child-512-Home-demo-counts-',
  #   format(Sys.Date(), "%Y-%m-%d"),
  #   '.csv'
  # )
), 
na = '(missing)'
)

######################


# write raw score descriptives for all scales (using psych::describe)
assign(
  str_c("raw_score_desc", age_range_name, form_name, sep = "_"),
  data_child_parent %>%
    select(contains('raw')) %>%
    describe(fast = T) %>%
    rownames_to_column() %>%
    rename(scale = rowname) %>%
    select(scale, n, mean, sd) %>%
    mutate(across(c(mean, sd), ~ (round(
      ., 2
    ))))
)




 
test2 <-  
  get(str_c("data", age_range_name, form_name, sep = "_")) %>%
    select(all_of(var_order)) %>% 
    pivot_longer(everything(), names_to = "variable", values_to = "category") %>% 
    group_by(variable, category) %>%
    count(variable, category) %>%
    arrange(match(variable, var_order), match(category, cat_order)) %>% 
    ungroup() %>% 
  mutate(across(variable, 
                ~
                  case_when(
                    lag(.x) == .x ~ NA_character_,
                    T ~ .x
                  )))
  
  
  
 
