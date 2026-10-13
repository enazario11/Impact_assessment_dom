#libraries
library(tidyverse)
library(here)
library(parzer)
library(slider)
library(terra)
library(ecospat)
library(tidysdm)
library(sf)

### load and clean data ####
obsv <- read.csv("data/obsv_land/obsv.csv") %>%
  filter(SPECIES_COMMON_NAME == "Swordfish, Broadbill") %>%
  mutate(date_obsv = as.Date(HAUL_DATE, format = "%d-%b-%y"), 
         lat = parse_lat(ACTIVITY_LATITUDE), 
         lon = parse_lon(ACTIVITY_LONGITUDE)) %>%
  select(-c("CATCH_SPECIES_CODE", 'TOTAL_KEPT_COUNT', "RETURNED_ALIVE_COUNT", "RETURNED_DEAD_COUNT", "RETURNED_UNKNOWN_COUNT", "HAUL_DATE", "ACTIVITY_LATITUDE", "ACTIVITY_LONGITUDE"))

### Boyce index ####
#### Ecospat ####
eco_rast <- rast(here("data/ecocast/ecocast_2018_2026.nc"))

#create month-year bins for observer data and ecocast raster predictions
obsv <- obsv %>%
  mutate(yr = format(as.Date(date_obsv), "%Y"))

yr <- format(seq(as.Date("2018-05-09"), as.Date("2026-07-08"), by = "year"), "%Y")
eco_rast_yr <- tapp(eco_rast, index = "years", fun = "mean")

#caclulate boyce index per month-year group
boyce_df <- data.frame(yr =  character(length(yr)), 
                       boyce_index = numeric(length(yr)), 
                       n = numeric(length(yr)))
for(i in 1:length(yr)){
  curr_yr <- yr[i]
  curr_obsv <- obsv %>% filter(yr == curr_yr) 
  curr_eco <- try(eco_rast_m[[names(eco_rast_m) == paste0("ym_", year(curr_obsv$date_obsv), format(curr_obsv$date_obsv, "%m"))]], silent = TRUE)
  curr_obsv <- curr_obsv %>% select(c("lon", "lat"))

  if(nrow(curr_obsv) >= 1 & class(curr_eco) == "SpatRaster"){
    curr_boyce <- ecospat.boyce(fit = curr_eco, obs = curr_obsv, method = 'spearman', window.w = 1/10)

    boyce_df$yr[i] <- curr_yr
    boyce_df$boyce_index[i] <- curr_boyce$cor
    boyce_df$n[i] <- nrow(curr_obsv)

  } else {
    boyce_df$yr[i] <- NA
    boyce_df$boyce_index[i] <- NA
    boyce_df$n[i] <- NA
  }
}

boyce_df %>%
  drop_na() %>%
  ggplot() +
    geom_boxplot(aes(y = boyce_index)) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 2) +
    #geom_text(aes(x = mo_yr2, y = boyce_index, label = n), nudge_y = 0.05, vjust = 0, size = 6)+
    theme_bw() + 
    theme(axis.text = element_text(size = 14, hjust = 1, angle = 45), 
          axis.title = element_text(size = 16))

ecospat_boyce <- boyce_df %>% drop_na() %>% summarise(mean_boyce = mean(boyce_index)) #value is 0.3 when grouped by year and then averaged

#tested for average raster and got 0.505
test_rast <- mean(eco_rast)
test_obs <- obsv %>% filter(date_obsv >= as.Date("2018-05-09")) %>% select(lon, lat)
test <- ecospat.boyce(fit = test_rast, obs = test_obs, method = "spearman", window.w = 1/10)

#### TidySDM ####
coords_dat <- obsv %>% drop_na(c(lat, lon)) %>% select(c(lon, lat, date_obsv)) %>% filter(date_obsv >= as.Date("2018-05-09"))

  #create background samples
coords_sf <- coords_dat %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)
pres_abs_dat <- sample_background(
  data = coords_sf,          
  raster = eco_rast,       
  n = nrow(coords_sf),                  
  method = "random"       
) 

pres_abs_coords <- st_coordinates(pres_abs_dat)

pres_abs_dat <- pres_abs_dat %>%
  mutate(lon = pres_abs_coords[, "X"],
         lat = pres_abs_coords[, "Y"], 
         eco_pred = NA) %>%
  st_drop_geometry()

coords_date <- rep(coords_dat$date_obsv, 2)
pres_abs_dat$date <- coords_date

for(i in 1:nrow(pres_abs_dat)){
  curr_row <- pres_abs_dat[i,]
  curr_vec <- curr_row %>% select(c(lon, lat)) %>% vect(crs = "+proj=longlat +datum=WGS84")
  curr_eco <- eco_rast[[time(eco_rast) == as.POSIXct(paste0(as.character(curr_row$date) , " 12:00:00"), tz = "UTC")]]

  curr_extract <- extract(curr_eco, curr_vec, ID = FALSE)

  pres_abs_dat$eco_pred[i] <- curr_extract[1,1]

}

tidy_boyce <- boyce_cont(data = pres_abs_dat, truth = class, eco_pred) #boyce index is 0.94

#### JC function ####
tmp <- pres_abs_dat %>% mutate(bin = cut(eco_pred, breaks = 10, labels = FALSE), bin = factor(bin))
colnames(tmp) <- c("is_pres", "lon", "lat", "val", "date", "bin")

# Expected frequency (Landscape)
expected <- tmp |>
  count(bin) |>
  mutate(freq_expected = n / sum(n)) |>
  select(bin, freq_expected)


# Predicted frequency per bin (Presences)
boyce_res <- tmp |>
  filter(is_pres == "presence") |>
  count(bin, .drop = FALSE) |>
  mutate(freq_pred = n / sum(n)) |>
  left_join(expected, by = "bin") |>
  mutate(PE_ratio = freq_pred / freq_expected) |>
  # Replace NAs/Inf from empty bins with 0 to keep correlation stable
  mutate(PE_ratio = ifelse(is.infinite(PE_ratio) | is.na(PE_ratio), 0, PE_ratio))

# Calc Boyce Index (as Spearman corr)
boyce_corr <- summarize(boyce_res,
                        index = cor(as.integer(bin), PE_ratio, method = "spearman", use = "complete.obs")) #value is 0.96
