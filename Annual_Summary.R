library(dplyr)
library(tidyr)
library(gt)

setwd("~/GitHub/LA_Tower/Data")

list.files(,pattern="csv")

la2<-read.csv("Daily_Gapfilled_LA2.csv")
head(la2)
summary(la2 %>% select(FC_filled, FCH4_filled, LE_filled, Salinity))

la3<-read.csv("Daily_Gapfilled_LA3.csv")
head(la3)
summary(la3 %>% select(FC_filled, FCH4_filled, LE_filled, Salinity))
str(la3)

###### Annual Summary #####

# Average by Year with standard deviation
# US-LA3
la3_annual_summary <- la3 %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarise(
    CO2_mean = mean(FC_filled, na.rm = TRUE),
    CO2_sd   = sd(FC_filled, na.rm = TRUE),
    
    CH4_mean = mean(FCH4_filled, na.rm = TRUE),
    CH4_sd   = sd(FCH4_filled, na.rm = TRUE),
    
    LE_mean  = mean(LE_filled, na.rm = TRUE),
    LE_sd    = sd(LE_filled, na.rm = TRUE),
    
    Days = n()
  )

la3_annual_summary

la2_annual_summary <- la2 %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarise(
    CO2_mean = mean(FC_filled, na.rm = TRUE),
    CO2_sd   = sd(FC_filled, na.rm = TRUE),
    
    CH4_mean = mean(FCH4_filled, na.rm = TRUE),
    CH4_sd   = sd(FCH4_filled, na.rm = TRUE),
    
    LE_mean  = mean(LE_filled, na.rm = TRUE),
    LE_sd    = sd(LE_filled, na.rm = TRUE),
    
    Days = n()
  )

la2_annual_summary

# Cumulative Summary by Year 
la3_annual_cumulative <- la3 %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarise(
    CO2_cumulative = sum(FC_filled, na.rm = TRUE),
    CH4_cumulative = sum(FCH4_filled, na.rm = TRUE),
    LE_cumulative  = sum(LE_filled, na.rm = TRUE),
    Days = n()
  )

la3_annual_cumulative

la2_annual_cumulative <- la2 %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarise(
    CO2_cumulative = sum(FC_filled, na.rm = TRUE),
    CH4_cumulative = sum(FCH4_filled, na.rm = TRUE),
    LE_cumulative  = sum(LE_filled, na.rm = TRUE),
    Days = n()
  )

la2_annual_cumulative
