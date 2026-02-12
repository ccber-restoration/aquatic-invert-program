library(tidyverse)
library(sf)
library(mapview)

sampling_sites <- read_csv("data/ncos_aquatic_invertebrate_sampling_locations.csv")

sites_sf <-  st_as_sf(sampling_sites, coords = c("x", "y"), crs = 4326)

#display interactive map
mapview(sites_sf, map.types = "Esri.WorldImagery")

#extract coordinates from sf object
coords_matrix <- st_coordinates(sites_sf)

#bind columns
sampling_sites_coordinates <- bind_cols(sites_sf, lon = coords_matrix[,1], lat = coords_matrix[,2])

#not sure what the units are... aren't decimal degrees

