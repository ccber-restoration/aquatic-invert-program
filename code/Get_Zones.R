# Purpose: 
#1 Read in geodatabase containing zones for birds/invert analysis
#2 Save in user-friendly format for use outside of Esri
# See: https://rpubs.com/dhainje/importingdata 

library(tidyverse)
library(sf)

# Read in .gdb

path_gdb <- "data/for_Emilio/CCBER_BirdSurveys.gdb"
zone_layers <- st_layers(dsn = path_gdb)

whole_slough <- st_read(path_gdb, "WholeSlough")
plot(whole_slough)
st_write(whole_slough, "data/for_Emilio/whole_slough.gpkg")

E_W_Main_Zones <- st_read(path_gdb, "E_W_Main_Zones")
plot(E_W_Main_Zones)
st_write(E_W_Main_Zones, "data/for_Emilio/E_W_Main_Zones.gpkg")


Branches_Main_Zones <- st_read(path_gdb, "Branches_Main_Zones")
plot(Branches_Main_Zones)
st_write(Branches_Main_Zones, "data/for_Emilio/Branches_Main_Zones.gpkg")

VernalPools <- st_read(path_gdb, "VernalPools")
plot(VernalPools)
st_write(VernalPools, "data/for_Emilio/VernalPools.gpkg")

Ponds <- st_read(path_gdb, "Ponds")
plot(Ponds)
st_write(Ponds, "data/for_Emilio/Ponds.gpkg")

Phelps <- st_read(path_gdb, "Phelps")
plot(Phelps)
st_write(Phelps, "data/for_Emilio/Phelps.gpkg")

Lower_Slough <- st_read(path_gdb, "Lower_Slough")
plot(Lower_Slough)
st_write(Lower_Slough, "data/for_Emilio/Lower_Slough.gpkg")

