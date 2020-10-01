temp1 <- data_RS_sim_child_parent %>% 
mutate(
  CPTOT_raw = rowSums(.[str_c("cpi", str_pad(as.character(1:50), 2, side = "left", pad = "0"))])
)

identical(sum(temp1[str_c("cpi", str_pad(as.character(1:50), 2, side = "left", pad = "0"))]), temp1$CPTOT_raw)
mean(temp1$CPTOT_raw)
str(temp1$CPTOT_raw)


temp2 <- temp1 %>% filter(rownames(.) == "200")
identical(sum(temp2[str_c("cpi", str_pad(as.character(1:50), 2, side = "left", pad = "0"))]), temp2$CPTOT_raw)
