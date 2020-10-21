form_acronyms <- c("cp", "ct", "tp", "tt")

scale_items_suffix <- c("S1", "S2", "S3", "S4", "S5", "TOT")


S1_item_names <- function(x) {
  str_c(str_c(x, "i"), str_pad(as.character(seq(1, 50, by = 5)), 2, side = "left", pad = "0"))
}
S2_item_names <- function(x) {
  str_c(str_c(x, "i"), str_pad(as.character(seq(2, 50, by = 5)), 2, side = "left", pad = "0"))
}
S3_item_names <- function(x) {
  str_c(str_c(x, "i"), str_pad(as.character(seq(3, 50, by = 5)), 2, side = "left", pad = "0"))
}
S4_item_names <- function(x) {
  str_c(str_c(x, "i"), str_pad(as.character(seq(4, 50, by = 5)), 2, side = "left", pad = "0"))
}
S5_item_names <- function(x) {
  str_c(str_c(x, "i"), str_pad(as.character(seq(5, 50, by = 5)), 2, side = "left", pad = "0"))
}
TOT_item_names <- function(x) {
  str_c(str_c(x, "i"), str_pad(as.character(1:50), 2, side = "left", pad = "0")) 
}

list_item_names <- list(S1_item_names, S2_item_names, S3_item_names, 
                        S4_item_names, S5_item_names, TOT_item_names)


temp1 <- crossing(form_acronyms, scale_items_suffix) %>% 
  mutate(scale_items = str_c(str_to_upper(form_acronyms), scale_items_suffix, "_items")) %>% 
  mutate(fun_item_names = rep(list_item_names, 4)) %>% 
  mutate(item_names = map(form_acronyms, fun_item_names))

map( ~
       tibble(var = map(var_order, ~ .y %>% filter(var == .x), .y = .x))) 
