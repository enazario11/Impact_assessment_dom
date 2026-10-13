
#libraries
library(terra)
library(tidyverse)
library(tidyterra)
library(here)
library(rnaturalearth)
library(sf)

#get ecocast predictions
eco_rast <- rast(here("data/ecocast/ecocast_2018_2026.nc"))
eco_layer <- eco_rast[[time(eco_rast) == as.POSIXct("2026-07-25 12:00:00", tz = "UTC")]]

#land shape
land <- ne_countries(scale = "large", returnclass = "sf") %>% st_transform(land, crs = 4326)

#plot
ggplot() + 
    geom_spatraster(data = eco_layer) +
    geom_sf(data = land, fill = "grey85", color = "grey30", linewidth = 0.2) +
    coord_sf(xlim = c(-131, -115),
      ylim = c(29.5, 47),
      expand = FALSE) +
    theme_bw() +
    scale_fill_grass_c(palette = "bcyr", direction = -1)
