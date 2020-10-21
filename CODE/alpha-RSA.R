# Load packages, read data, specify input parameters

suppressPackageStartupMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(data.table))
suppressMessages(library(psych))

urlRemote_path  <- "https://raw.githubusercontent.com/"
github_path <- "DSHerzberg/RATING-SCALE-ANALYSIS/master/INPUT-FILES/"

data_RS_sim_child_parent <- suppressMessages(read_csv(url(
  str_c(urlRemote_path, github_path, "data-RS-sim-child-parent.csv")
)))
data_RS_sim_child_teacher <- suppressMessages(read_csv(url(
  str_c(urlRemote_path, github_path, "data-RS-sim-child-teacher.csv")
)))
data_RS_sim_teen_parent <- suppressMessages(read_csv(url(
  str_c(urlRemote_path, github_path, "data-RS-sim-teen-parent.csv")
)))
data_RS_sim_teen_teacher <- suppressMessages(read_csv(url(
  str_c(urlRemote_path, github_path, "data-RS-sim-teen-teacher.csv")
)))

form_acronyms <- c("cp", "ct", "tp", "tt")


CPS1_items <- str_c("cpi", str_pad(as.character(seq(1, 50, by = 5)), 2, side = "left", pad = "0"))
CPS2_items <- str_c("cpi", str_pad(as.character(seq(2, 50, by = 5)), 2, side = "left", pad = "0"))
CPS3_items <- str_c("cpi", str_pad(as.character(seq(3, 50, by = 5)), 2, side = "left", pad = "0"))
CPS4_items <- str_c("cpi", str_pad(as.character(seq(4, 50, by = 5)), 2, side = "left", pad = "0"))
CPS5_items <- str_c("cpi", str_pad(as.character(seq(5, 50, by = 5)), 2, side = "left", pad = "0"))
CPTOT_items <- str_c("cpi", str_pad(as.character(1:50), 2, side = "left", pad = "0"))
