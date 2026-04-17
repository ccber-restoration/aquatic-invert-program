# source previous script


source("code/Ojemann_Emilio/01_read_filter_bird_data.R")

# start creading network for vernal pool bird assemblage

# load packages ----

library(readxl)
library(tidyverse)

#packages we could use for visualizing networks:
library(bipartite)
library(igraph)

#package for adding icons to networks
library(rphylopic)

# read in data ----

# trophic links ---

links_path <- "data/for_Emilio/NCOS_aquatic_trophic_links_2026-02-10.xlsx"

#in network terminology this is an "edge list"
trophic_links <- read_xlsx(path = links_path, sheet = "trophic links") |> 
  select(c(bird_species, invert_taxon)) |> 
  #relocate(invert_taxon, .before = bird_species) %>% 
  filter(bird_species != "Whimbrel") |>
  filter(bird_species != "Hooded Merganser")


# get vectors of bird and invert names ----
birds <- unique(trophic_links$bird_species)
inverts <- unique(trophic_links$invert_taxon)

#TODO- finish creating vectors of bird species lists
# then filter those to ones that in the trophic link data

vp_birds_invertivorous <- vp_bird_summary %>% 
  filter(species %in% birds) 
  

# create matrix ----

## create matrix for invert network ----
tl_matrix <- trophic_links %>% 
  as.matrix()

trophic_network <- graph_from_edgelist(tl_matrix, directed = FALSE)

adj_matrix <- as_adjacency_matrix(trophic_network, sparse=FALSE)

### matrix_subset for vernal pools ----

matrix_subset_vp <- adj_matrix[vp_birds_invertivorous$species, inverts]

png(file = "figures/invert_network_vp.png", width = 600, height = 900, units = "px", res = 100)

#continue filling out trophic links data
#create draft network viz based on existing data
plotweb(web = matrix_subset_vp, text_size =0.8, horizontal = TRUE)

dev.off()


## TODO- next habitat ----