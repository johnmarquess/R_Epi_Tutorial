library(tidyverse)
library(lubridate)
library(readxl)
library(kableExtra)
library(janitor)

data_day <- str_replace_all(today(), "-", "_")

data_path <- paste0(
  "U:/CDU/CduImt/CDB led/2019-nCoV 2020/EPI/Case Line List - NOCS Reporting/data",
  "/",
  data_day,
  "/",
  "line_list_current_NOCS-R_",
  data_day,
  ".xlsx"
)
data_path

NOCS_data <- read_xlsx("data/line_list_current_NOCS-R_2022_05_07.xlsx", sheet = 4)
glimpse(NOCS_data)

# NOCS_data %>% group_by(HOSPITALISED) %>% tally()

nocs <- NOCS_data %>% filter(COLLECTDATE >= dmy("01012022"), COLLECTDATE < today()) %>% 
    select(NOTF_ID, COLLECTDATE, INDIG_STATUS, BIRTHDATE,SEX, STATUS, AGE_AT_ONSET, 
           HHS, PHU,HOSPITALISED, Death, VACCINATION_STATUS, RES_LAT, RES_LONG, RESIDENCE_SA2) %>% 
    mutate(week_date = floor_date(COLLECTDATE, "week", week_start = 6))

glimpse(nocs)


# make labels for age groups
age_labels <- c("0 to 19", "20 to 39", "40 to 59", "60 to 79", "80 plus")


nocs <- nocs %>% mutate(AGE_GROUP = cut(AGE_AT_ONSET,
  breaks = c(-Inf, 20, 40, 60, 80, Inf),
  labels = age_labels,
  right = FALSE
))


# nocs %>% select(AGE_AT_ONSET, AGE_GROUPS) %>% View()

this_week <- week(today())
n_weeks_ago <- week(today())-6

case_HHS_summary_n_wks <- nocs %>%
    mutate(week_num = week(week_date)) %>% 
    group_by(HHS, week_date, week_num) %>% 
    tally() %>% 
    filter(between(week_num, n_weeks_ago, this_week)) %>% 
    ungroup() %>% 
    select(-week_date) %>%
    mutate(HHS = str_to_title(HHS)) %>% 
    filter(!is.na(HHS)) %>% 
    pivot_wider(names_from = week_num, values_from = n, values_fill = 0) %>% 
    filter(!HHS == "Unknown")

case_HHS_summary_n_wks

kable(case_HHS_summary_n_wks, format = "simple", align = "lrrrrrr",
      caption = "COVID-19 case numbers per HHS over previous 6 weeks")

kable(case_HHS_summary_n_wks,
      caption = "COVID-19 case numbers per HHS over previous 6 weeks") %>% 
    kable_styling(full_width = F) %>% 
    add_header_above(c(" " = 1, "Week" = 6))




age_rate <- nocs %>%
  select(week_date, AGE_AT_ONSET, AGE_GROUP) %>%
  filter(!is.na(AGE_AT_ONSET)) %>%
  group_by(week_date, AGE_GROUP) %>%
  tally() %>%
  pivot_wider(names_from = AGE_GROUP,
              values_from = n,
              values_fill = 0) %>%
  pivot_longer(cols = 2:6, names_to = "AgeGroup", values_to = "Notifications") %>%
    ungroup() %>% 
    filter(year(week_date) == 2022)

age_rate %>% 
    ggplot(. , aes(x = week_date, y = Notifications, fill=AgeGroup)) +
    geom_col() + theme_minimal()
