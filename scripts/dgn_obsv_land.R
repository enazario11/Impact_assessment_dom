#libraries
library(tidyverse)
library(here)
library(parzer)
library(slider)
library(terra)
library(ecospat)

#To-Do
#1) Because response (swordfish catch) is relative within a 10-day window, should we also average the HSI predictions for a 10 day window?
#2) Look into Boyce Index
#3) consider FTID? captains sold to multiple dealers resulting in multiple landings with the same date
#4) incorporate gear code as random effect if we do any kind of regression?


### load and clean data ####
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

### calculating relative catch from landings data ####
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


### Boyce index ####
eco_rast <- rast(here("data/ecocast/ecocast_2018_2026.nc"))

#create month-year bins for observer data and ecocast raster predictions
obsv <- obsv %>%
  mutate(mo_yr = format(as.Date(date_obsv), "%Y-%m"))

mo_yr <- format(seq(as.Date("2018-05-09"), as.Date("2026-07-08"), by = "month"), "%Y-%m")
eco_rast_m <- tapp(eco_rast, index = "yearmonth", fun = "mean")

#caclulate boyce index per month-year group
boyce_df <- data.frame(mo_yr =  character(length(mo_yr)), 
                       boyce_index = numeric(length(mo_yr)), 
                       n = numeric(length(mo_yr)))
for(i in 1:length(mo_yr)){
  curr_mo_yr <- mo_yr[i]
  curr_obsv <- obsv %>% filter(mo_yr == curr_mo_yr) 
  curr_eco <- try(eco_rast_m[[names(eco_rast_m) == paste0("ym_", year(curr_obsv$date_obsv), format(curr_obsv$date_obsv, "%m"))]], silent = TRUE)
  curr_obsv <- curr_obsv %>% select(c("lon", "lat"))

  if(nrow(curr_obsv) >= 1 & class(curr_eco) == "SpatRaster"){
    curr_boyce <- ecospat.boyce(fit = curr_eco, obs = curr_obsv, method = 'spearman')

    boyce_df$mo_yr[i] <- curr_mo_yr
    boyce_df$boyce_index[i] <- curr_boyce$cor
    boyce_df$n[i] <- nrow(curr_obsv)

  } else {
    boyce_df$mo_yr[i] <- NA
    boyce_df$boyce_index[i] <- NA
    boyce_df$n[i] <- NA
  }
}

boyce_df %>%
  mutate(mo_yr2 = format(ym(mo_yr), "%Y-%m")) %>%
  drop_na() %>%
  ggplot() +
    geom_point(aes(x = mo_yr2, y = boyce_index), size = 4) +
    geom_line(aes(x = mo_yr2, y = boyce_index, group = 1), alpha = 0.5) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 2) +
    geom_text(aes(x = mo_yr2, y = boyce_index, label = n), nudge_y = 0.05, vjust = 0, size = 6)+
    xlab("") +
    theme_bw() + 
    theme(axis.text = element_text(size = 14, hjust = 1, angle = 45))
