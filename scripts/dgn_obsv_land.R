#libraries
library(tidyverse)
library(here)
library(parzer)
library(slider)

#To-Do
#1) Because response (swordfish catch) is relative within a 10-day window, should we also average the HSI predictions for a 10 day window?
#2) Look into Boyce Index
#3) consider FTID? captains sold to multiple dealers resulting in multiple landings with the same date
#4) incorporate gear code as random effect? 


#load and clean data 
obsv <- read.csv("data/obsv_land/obsv.csv") %>%
  filter(SPECIES_COMMON_NAME == "Swordfish, Broadbill") %>%
  mutate(date_obsv = as.Date(HAUL_DATE, format = "%d-%b-%y"), 
         lat = parse_lat(ACTIVITY_LATITUDE), 
         lon = parse_lon(ACTIVITY_LONGITUDE)) %>%
  select(-c("CATCH_SPECIES_CODE", 'TOTAL_KEPT_COUNT', "RETURNED_ALIVE_COUNT", "RETURNED_DEAD_COUNT", "RETURNED_UNKNOWN_COUNT", "HAUL_DATE", "ACTIVITY_LATITUDE", "ACTIVITY_LONGITUDE"))

land <- read.csv("data/obsv_land/land.csv") %>%
  filter(SPECIES_CODE_NAME == "SWORDFISH") %>%
  mutate(date_land = as.Date(LANDING_DATE, format = "%d-%b-%y")) %>%
  select(-c("LANDING_DATE", "PORT_CODE", "SPECIES_CODE", "ROUND_WEIGHT_LBS", "EXVESSEL_REVENUE")) %>%
  filter(date_land > as.Date("1990-08-03", format = "%Y-%m-%d")) #filter out landings data before observer data starts

#calculate rolling 10 day window from landings data
land <- land %>%
  mutate(roll_mean_10 = slide_index_dbl(
    NUM_OF_FISH, 
    .i = date_land, 
    .f = mean, 
    .before = days(10)
  ))

land_roll <- land %>%
  select(c("VESSEL_NUM", "date_land", "roll_mean_10"))

#assign rolling 10 day window mean from landings data to observer data by date in order to get the relative catch conditions that day (response var in model)
obsv_rel <- left_join(
  obsv, 
  land_roll, 
  by = join_by(VESSEL_NUM, closest(date_obsv <= date_land))
)

obsv_rel <- obsv_rel %>%
  drop_na(roll_mean_10) %>%
  mutate(rel_catch = TOTAL_CATCH_COUNT/roll_mean_10) #relative to the catch within a 10 day period
