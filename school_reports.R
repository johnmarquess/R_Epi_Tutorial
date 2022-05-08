library(tidyverse)
library(lubridate)
library(readxl)
library(ggthemes)
library(RColorBrewer)
library(tidyquant)


# Loading the data
rat <- read_xlsx("data/School_Cohort_PCR_RAT_Plots_Preschool.xlsx", sheet = 7) %>% 
    select(1:4) 
pcr <- read_xlsx("data/School_Cohort_PCR_RAT_Plots_Preschool.xlsx", sheet = 5) %>% 
    select(c(1,2,3,5))

# Data frames


# Using vectors
colnames(pcr) <- c("DATE", "NOCS_PRIMARY", "NOCS_SECONDARY", "NOCS_PRESCHOOL")
colnames(rat) <- c("DATE", "RAT_PRIMARY", "RAT_SECONDARY", "RAT_PRESCHOOL")

all_results <- full_join(rat, pcr, by = "DATE") %>% 
    filter(year(DATE) > 2021)

all_results_long <- all_results %>% 
    pivot_longer(cols = 2:7, names_to = "Cohort", values_to = "Notifications") %>% 
    mutate(type = as_factor(Cohort)) %>% 
    mutate(groups = recode(Cohort, 
                           NOCS_PRESCHOOL = "Children < 5 (NoCS)",
                           NOCS_PRIMARY = "Children 5 to 11 (NoCS)",
                           NOCS_SECONDARY = "Children 12 to 17 (NoCS)",
                           RAT_PRESCHOOL = "Children < 5 (RATs)",
                           RAT_PRIMARY = "Children 5 to 11 (RATs)",
                           RAT_SECONDARY = "Children 12 to 17 (RATs)"))


all_results_long %>% 
    filter(DATE > dmy("01032022")) %>% 
    select(-Cohort) %>% 
    mutate(Cohort = groups) %>% 
    ggplot(., aes(x = DATE, y = Notifications, color = Cohort)) + 
    geom_point(alpha = 0.4, size = 2) +
    # stat_smooth(se = FALSE) +
    theme_fivethirtyeight() +
    scale_color_brewer(palette = "Paired") +
    geom_ma(ma_fun = SMA, n = 7, size = 1, linetype = "solid", alpha = 0.6) + 
    ggtitle("PCT and RAT cases in children", subtitle = "Solid lines are 7-day moving average")


rats_and_nocs <- all_results %>% 
    mutate(all_under_5 = RAT_PRESCHOOL + NOCS_PRESCHOOL,
           all_5_to_11 = RAT_PRIMARY + NOCS_PRIMARY,
           all_12_to_17 = RAT_SECONDARY + NOCS_SECONDARY) %>% 
    select(1, 8,9,10) 

rats_and_nocs_long <- rats_and_nocs %>% 
    pivot_longer(cols = 2:4, names_to = "Cohort", values_to = "Notifications") %>% 
    mutate(Cohort = recode(Cohort, 
                           all_under_5 = "Children < 5",
                           all_5_to_11 = "Children 5 to 11",
                           all_12_to_17 = "Children 12 to 17"))


rats_and_nocs_long %>% 
    ggplot(., aes(x = DATE, y = Notifications, color = Cohort)) + 
    theme_fivethirtyeight() +
    scale_color_brewer(palette = "Set2") +
    geom_point(alpha = 0.3, size = 2, stroke=1) +
    geom_ma(ma_fun = SMA, n = 7, size = 1.2, linetype = "solid") + 
    ggtitle("COVID-19 cases in children (NoCS and RATS combined)", subtitle = "Solid lines are 7-day moving average")
