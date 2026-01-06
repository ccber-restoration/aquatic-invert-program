library(sf)
library(mapview)

sampling_sites <- st_read("data/sampling_site_shapefile/Aquatic_Invert_Sampling_Sites.shp")

mapview(sampling_sites)