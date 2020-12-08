assign(str_c("data", age_range_name, form_name, "TOT", sep = "_"),
       suppressMessages(read_csv(url(
         str_c(urlRemote_path, github_path, "data-RS-sim-child-parent.csv")
       ))) %>% 
         select(!!rlang::sym(str_c(scale_prefix, "TOT_raw")))
       )
