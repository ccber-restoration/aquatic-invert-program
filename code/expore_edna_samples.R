# explore locations of edna samples

library(sf)
library(mapview)
library(tidyverse)

batch_JVB1470 <- read_csv("data/edna/JVB1470/UniCOI/JVB1470-samples.csv")

batch_JVB1554 <- read_csv("data/edna/JVB1554/MiFishU/JVB1554-samples.csv")

batch_JVB1703 <- read_csv("data/edna/JVB1703/MiFishU/JVB1703-samples.csv")

batch_JVB2116 <- read_csv("data/edna/JVB2116/MiFishU/JVB2116-samples.csv")

batch_JVB2332 <- read_csv("data/edna/JVB2332/ArthCOI/JVB2332-samples.csv")

sample_metadata_combined <- bind_rows(batch_JVB1470, batch_JVB1554, batch_JVB1703, batch_JVB2116, batch_JVB2332)

write_csv(sample_metadata_combined, "data/edna/CCBER_edna_sample_metadata.csv")

 
sample_metadata_with_location <- sample_metadata_combined |> 
  filter(!is.na(Lat))

write_csv(sample_metadata_with_location, "data/edna/CCBER_edna_sample_metadata_with_location.csv")

samples_sf <- st_as_sf(sample_metadata_with_location, coords = c("Long", "Lat"), crs = "EPSG:4326")

mapview(samples_sf, map.types = "Esri.WorldImagery")
