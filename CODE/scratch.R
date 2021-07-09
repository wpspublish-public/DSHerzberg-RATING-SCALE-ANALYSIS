set.seed(1234)
sample_60perc2 <- sample_full %>% 
  # group_by(age, gender, educ, ethnic, region) %>%
  slice_sample(prop = .6) %>% 
  ungroup()

# try splitstackshape::stratified()

library(splitstackshape)

set.seed(1234)
temp1 <- stratified(sample_full,
                    c("age", "gender", "educ", "ethnic", "region"),
                    size = .6)
