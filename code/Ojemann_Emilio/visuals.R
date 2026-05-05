
# Visual for NCOS with all zones + invert sampling sites
mapview(Ponds, map.types = "Esri.WorldImagery",
        col.regions = "darkolivegreen1",
        color = "black") +
  mapview(E_W_Main_Zones, map.types = "Esri.WorldImagery",
          col.regions = "firebrick1",
          color = "black") +
  mapview(Phelps, map.types = "Esri.WorldImagery",
          col.regions = "blue",
          color = "black") + 
  mapview(VernalPools, map.types = "Esri.WorldImagery",
          col.regions = "skyblue",
          color = "black") +
  # aquatic invert sampling sites
  mapview(sites_sf, map.types = "Esri.WorldImagery",
          col.region = "black",
          color = "yellow",
          alpha.regions = 1,
          cex = 4)



