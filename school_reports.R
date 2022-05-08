library(tidyverse)
library(lubridate)
library(readxl)
library(ggthemes)
library(RColorBrewer)
library(tidyquant)


# Load the data
rat <- read_xlsx("data/School_Cohort_PCR_RAT_Plots_Preschool.xlsx", sheet = 7) %>% 
    select(1:4) 
pcr <- read_xlsx("data/School_Cohort_PCR_RAT_Plots_Preschool.xlsx", sheet = 5) %>% 
    select(c(1,2,3,5))

colnames(pcr) <- c("DATE", "NOCS_PRIMARY", "NOCS_SECONDARY", "NOCS_PRESCHOOL")
colnames(rat) <- c("DATE", "RAT_PRIMARY", "RAT_SECONDARY", "RAT_PRESCHOOL")

all_results <- full_join(rat, pcr, by = "DATE") %>% 
    filter(year(DATE) > 2021)

all_results_long <- all_results %>% 
    pivot_longer(cols = 2:7, names_to = "Cohort", values_to = "Notifications")


all_results_long %>% 
    filter(DATE > dmy("01032022")) %>% 
    ggplot(., aes(x = DATE, y = Notifications, color = Cohort)) + 
    # geom_line() +
    geom_point(alpha = 0.5) +
    # stat_smooth(se = FALSE) +
    theme_fivethirtyeight() +
    scale_color_brewer(palette = "Paired") +
    geom_ma(ma_fun = SMA, n = 7, size = 1, linetype = "solid", alpha = 0.6) + 
    ggtitle("PCT and RAT cases in children", subtitle = "Solid lines are 7-day moving average")

