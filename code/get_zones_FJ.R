# 0. purpose of script: ----
# (1) read in geodatabase with zones for bird/invert analysis
# (2) save in a more user-friendly format for work outside of esri ecosystem

# see https://rpubs.com/dhainje/importingdata#:~:text=%23%23%20Multiple%20layers%20are%20present,containing%20more%20%23%23%20than%20one.

library(tidyverse)

library(sf) # simple features, go-to R package for working with vector data

library(mapview)

# 1. read in gdb ----

path_gbd <- "data/for_Emilio/CCBER_BirdSurveys.gdb"

zone_layers <- st_layers(dsn = path_gbd)

# now read in individual zones

# 2.  whole slough ----
whole_slough  <- st_read(path_gbd, "WholeSlough")

plot(whole_slough)

# write to file
st_write(whole_slough, "data/for_Emilio/WholeSlough.gpkg")

# 3 .slough in three sections (east, west, and main) ----
E_W_Main_Zones <- st_read(path_gbd, "E_W_Main_Zones") %>% 
  #drop z dimension because mapview doesn't like it
  st_zm(drop = TRUE)

plot(E_W_Main_Zones)

mapview(E_W_Main_Zones, map.types = "Esri.WorldImagery")

st_write(E_W_Main_Zones, "data/for_Emilio/E_W_Main_Zones.gpkg")

# 4.  upper east and west branches ----
Branches_Main_Zones <- st_read(path_gbd, "Branches_Main_Zones")

plot(Branches_Main_Zones)

st_write(Branches_Main_Zones, "data/for_Emilio/Branches_Main_Zones.gpkg")

# 5. Lower slough  ----
Lower_Slough <- st_read(path_gbd, "Lower_Slough")

plot(Lower_Slough)

st_write(Lower_Slough, "data/for_Emilio/Lower_Slough.gpkg")

# 6. vernal pools ----
VernalPools <- st_read(path_gbd, "VernalPools")

plot(VernalPools)

st_write(VernalPools, "data/for_Emilio/VernalPools.gpkg")

# 7. Ponds ----
Ponds <- st_read(path_gbd, "Ponds") %>% 
  #drop z dimension because mapview doesn't like it
  st_zm(drop = TRUE)

plot(Ponds)

mapview(Ponds) +
  mapview(E_W_Main_Zones)

st_write(Ponds, "data/for_Emilio/Ponds.gpkg")

# 8. Phelps ----
Phelps <-  st_read(path_gbd, "Phelps")

plot(Phelps)

st_write(Phelps, "data/for_Emilio/Phelps.gpkg")


