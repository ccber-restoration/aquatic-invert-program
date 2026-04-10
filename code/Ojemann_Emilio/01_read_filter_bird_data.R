#note: this script relies on objects loaded by get_zones_FJ.R

# goal: read in bird data from csv, then filter based on zone polygons

#read in ncos bird data for 203-2025
ncos_aquatic_birds_sf <- read_csv(file = "data/for_Emilio/aquatic_bird_observations_2023_2025.csv") %>% 
  st_as_sf(coords = c("x", "y")) %>% 
  #assign WGS84 (not projected)
  st_set_crs(4326)

#check coordinate reference system
st_crs(ncos_aquatic_birds_sf)

# view bird data
mapview(ncos_aquatic_birds_sf)

#check coordinate reference system of VernalPools polygon
st_crs(VernalPools)

#EPSG is 3857This is pseudo-mercator, primarily used for web-mapping

#create new version with CRS = 4326
vernal_pools_sf <- VernalPools %>% 
  st_transform(crs = 4326)

#filter the bird point data to those contained within the vernal_pools_sf geometry
# see documentation on subsetting using brackets: https://r-spatial.github.io/sf/articles/sf4.html#subsetting-feature-sets

vp_birds <- ncos_aquatic_birds_sf[vernal_pools_sf,]

# view on map
mapview(vp_birds, map.type = "Esri.WorldImagery")

#now summarize the total count for each bird species
vp_bird_summary <- vp_birds %>% 
  group_by(species) %>% 
  summarize(total_count = sum(count))

# East and West branches, with incoming southern branch

st_crs(E_W_Main_Zones)
e_w_s_zones_sf <- E_W_Main_Zones |> 
  st_transform(crs = 4326)

ews_birds <- ncos_aquatic_birds_sf[e_w_s_zones_sf,]
mapview(ews_birds, map.type = "Esri.WorldImagery")

# TODO summarize counts

