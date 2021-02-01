temp1 <- data_child_parent_nt %>% 
  group_by(CPS1_raw) %>% 
  select (CPS1_raw, CPS1_nt) %>% 
  arrange(CPS1_raw) %>% 
  summarize(CPS1_nt = min(CPS1_nt)) %>% 
  complete(CPS1_raw =  all_raw_range) %>% 
  fill(CPS1_nt, .direction = "downup") %>%
  rename(raw = CPS1_raw
  )


