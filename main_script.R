library(tidyverse)
library(lubridate)
library(readxl)

data_day <- str_replace_all(today(), "-","_")

data_path <- paste0("U:/CDU/CduImt/CDB led/2019-nCoV 2020/EPI/Case Line List - NOCS Reporting/data", 
                    "/", 
                    data_day,
                    "/",
                    "line_list_current_NOCS-R_",
                    data_day,
                    ".xlsx"
                    )
data_path

NOCS_data <- read_xlsx("data/line_list_current_NOCS-R_2022_05_07.xlsx", sheet = 4)

nocs <- NOCS_data
glimpse()