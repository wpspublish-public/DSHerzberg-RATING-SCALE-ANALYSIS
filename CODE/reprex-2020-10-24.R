# Akrun solution
library(tidyverse)

set.seed(2346)
df <- tibble(
  col1 = sample(c(0,1), replace=T, size=10),
  col2 = sample(c(0,1), replace=T, size=10),
  col3 = sample(c(0,1), replace=T, size=10),
  col4 = sample(c(0,1), replace=T, size=10)
)

cols <- c("col1", "col3")

df_list_col <- tibble(
  data = list(df), 
  cols = list(cols)
)

akrun <- df_list_col %>% 
  mutate(subset = map2(data, cols, ~ .x %>% select(all_of(.y))))


I have a list of data frames and I want to subset each one, retaining only certain columns
The names of the columns I want are contained in character vectors unique to each dataframe.
One way I thought of doing this was with a list-column workflow. I would create a data frame
with a data list-column holding the data frames, and a cols list-column holding the character
vectors.

The real application of this will include a list of 24 large datasets. Here is 
minimal example to illustrate my dilemma.

set.seed(2346)
df <- tibble(
  col1 = sample(c(0,1), replace=T, size=10),
  col2 = sample(c(0,1), replace=T, size=10),
  col3 = sample(c(0,1), replace=T, size=10),
  col4 = sample(c(0,1), replace=T, size=10)
)

cols <- c("col1", "col3")

df_list_col <- tibble(
  data = list(df), 
  cols = list(cols)
)

`df_list_col` has the list-column structure, but only in a single row.

With `dplyr`, it seems that this would be a matter of creating a third list-column to hold
the subsetted data frame. Thus:
  
df_output <- df_list_col %>% 
  mutate(subset = select(.$data, !!.$cols))

df_output <- df_list_col %>% 
  mutate(subset = map(.$data, ~ select(.x, !!.$cols)))
           

But this returns an error:
  
#   Error: Problem with `mutate()` input `subset`.
# x `select()` doesn't handle lists.
# ℹ Input `subset` is `select(.$data, list(c("col1", "col3")))`.
  
  I cannot figure out how to get select to see the contents of `cols` as a character vector, instead of a list.dirs(
    
    Thanks in advance for any help!
  )
