source("code/00_setup.R")
source("code/00_sampling_sites_map.R")

spatial_invert_data <- invert_data |> 
  inner_join(sampling_sites, by = join_by(site == Location_Name)) |> 
  filter(site != "PIER") |> 
  filter(site != "MO1") 

#converting table to shape file
invert_data_sf <- st_as_sf(spatial_invert_data, coords = c("x", "y"))

#mapview(spatial_invert_data, map.types = "Esri.WorldImagery")

mapview(spatial_invert_data, xcol = "x", ycol = "y", map.types = "Esri.WorldImagery")

#write to file
write_csv(x = spatial_invert_data, file = "data/for_Emilio/invert_data_spatial.csv")

mapview(E_W_Main_Zones, map.types = "Esri.WorldImagery")

#Comment


