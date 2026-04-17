#note: this script relies on objects loaded by get_zones_FJ.R

# Load data ----
source("code/Ojemann_Emilio/00_get_zones_FJ.R")

# GOAL: read in bird data from csv, then filter based on zone polygons

#read in ncos bird data for 2023-2025 
ncos_aquatic_birds_sf <- read_csv(file = "data/for_Emilio/aquatic_bird_observations_2023_2025.csv") %>% 
  st_as_sf(coords = c("x", "y")) %>% 
  #assign WGS84 (not projected)
  st_set_crs(4326)

#check coordinate reference system
st_crs(ncos_aquatic_birds_sf)

# view bird data
mapview(ncos_aquatic_birds_sf,
        col.regions = "red"
)

# filter bird observations to those from any aquatic sampling zone

# All Zones ----
# filter birds to ALL aquatic zones 

# Combine all zone geometries into one and filter corresponding bird data ---

zone_union <- st_union(VernalPools, Phelps) %>% 
  st_union(Ponds) %>% 
  st_union(whole_slough)

#map zones
mapview(zone_union)

# check coordinate system of zones
st_crs(zone_union)

# create new object with reprojected zones
zone_union_sf <- zone_union %>% 
  st_transform(crs = 4326)

# filter bird data spatially (i.e. only keep observations (rows) within the aquatic sampling zones)
aquatic_zone_birds <- ncos_aquatic_birds_sf[zone_union_sf,]

# view
mapview(aquatic_zone_birds)

sp_all <- unique(aquatic_zone_birds$species)


#now summarize the total count for each bird species
all_zones_bird_summary <- aquatic_zone_birds %>% 
  group_by(species) %>% 
  summarize(total_count = sum(count))


# Vernal Pools ----

#check coordinate reference system of VernalPools polygon
st_crs(VernalPools)

#EPSG is 3857This is pseudo-mercator, primarily used for web-mapping

#create new version with CRS = 4326
vernal_pools_sf <- VernalPools %>% 
  st_transform(crs = 4326)

#filter the bird point data to those contained within the vernal_pools_sf geometry
# see documentation on subsetting using brackets: https://r-spatial.github.io/sf/articles/sf4.html#subsetting-feature-sets

vp_birds <- ncos_aquatic_birds_sf[vernal_pools_sf,]
vp_birds

# view on map
mapview(vp_birds, map.type = "Esri.WorldImagery")

#now summarize the total count for each bird species
vp_bird_summary <- vp_birds %>% 
  group_by(species) %>% 
  summarize(total_count = sum(count))

vp_bird_vector <- vp_bird_summary %>% 
  pull(species)

# Entire Slough: East and West branches, with incoming Southern branch ----

st_crs(E_W_Main_Zones)
e_w_s_zones_sf <- E_W_Main_Zones |> 
  st_transform(crs = 4326)

# view
ews_birds <- ncos_aquatic_birds_sf[e_w_s_zones_sf,]
mapview(ews_birds, map.type = "Esri.WorldImagery")

# summarize

ews_bird_summary <- ews_birds |> 
  group_by(species) |> 
  summarize(total_count = sum(count))

# Phelps Creek junction ---- 

st_crs(Phelps)
phelps_sf <- Phelps |> 
  st_transform(crs = 4326)

# view
phelps_birds <- ncos_aquatic_birds_sf[phelps_sf,]
mapview(phelps_birds, map.type = "Esri.WorldImagery")

# summarize

phelps_bird_summary <- phelps_birds |> 
  group_by(species) |> 
  summarize(total_count = sum(count))

# Lower Dev. Slough ---- 
st_crs(Lower_Slough)
lower_slough_sf <- Lower_Slough |> 
  st_transform(crs = 4326)

# view
lower_slough_birds <- ncos_aquatic_birds_sf[lower_slough_sf,]
mapview(lower_slough_birds, map.type = "Esri.WorldImagery")

# summarize

lower_slough_bird_summary <- lower_slough_birds |> 
  group_by(species) |> 
  summarize(total_count = sum(count))

# NCOS ponds ---- 

st_crs(Ponds)
ponds_sf <- Ponds |> 
  st_transform(crs = 4326)

# view
ponds_birds <- ncos_aquatic_birds_sf[ponds_sf,]
mapview(ponds_birds, map.type = "Esri.WorldImagery")

# summarize

ponds_bird_summary <- ponds_birds |> 
  group_by(species) |> 
  summarize(total_count = sum(count))

# East and West Branches only ---- 

st_crs(Branches_Main_Zones)
e_w_branches_sf <- Branches_Main_Zones |> 
  st_transform(crs = 4326)

# view
e_w_branches_birds <- ncos_aquatic_birds_sf[e_w_branches_sf,]
mapview(e_w_branches_birds, map.type = "Esri.WorldImagery")

# summarize

e_w_branches_bird_summary <- e_w_branches_birds |> 
  group_by(species) |> 
  summarize(total_count = sum(count))

ncos_aquatic_birds_sf


