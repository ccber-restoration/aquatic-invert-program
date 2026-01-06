library(dplyr)
library(sf)
library(mapview)

sampling_sites <- st_read("data/sampling_site_shapefile/Aquatic_Invert_Sampling_Sites.shp")

#display interactive map
mapview(sampling_sites)

#extract coordinates from sf object
coords_matrix <- st_coordinates(sampling_sites)


#bind columns
sampling_sites_coordinates <- bind_cols(sampling_sites, lon = coords_matrix[,1], lat = coords_matrix[,2])

#not sure what the units are... aren't decimal degrees

