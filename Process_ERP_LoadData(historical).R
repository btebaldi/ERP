library(readxl)
library(dplyr)

# Data load ---------------------------------------------------------------

print("Loading data")

# data_ref <- "2024-09-01"
# T10_Bond <- 3.9020/100
# 0.086639 

data_ref <- "2024-08-01"
T10_Bond <- 3.9110/100


dir_to_load <- file.path("./Historico", sprintf("ref %s", data_ref))
file_to_load <- list.files(path = dir_to_load, pattern = ".xlsx", full.names = TRUE)

base_ERP_full <- read_excel(file_to_load)

base_ERP_full <- base_ERP_full %>% filter(!is.na(Ticker))

rm(list = c("file_to_load", "dir_to_load"))











