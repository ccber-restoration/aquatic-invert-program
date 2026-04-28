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


# for vernal pool birds
vp_birds_invertivorous <- vp_bird_summary %>% 
  filter(species %in% birds) 

# for phelps junction

phelps_birds_invertivorous <- phelps_bird_summary |> 
  filter(species %in% birds) 

# for NCOS ponds

ponds_birds_invertivorous <- ponds_bird_summary |> 
  filter(species %in% birds)

# for the East and West branches, plus the southern channel (Entire Slough)

ews_birds_invertivorous <- ews_bird_summary |> 
  filter(species %in% birds)
  

# create matrix ----

## create matrix for invert network ----
tl_matrix <- trophic_links %>% 
  as.matrix()

# convert the 2-column matrix to igraph network graph
# undirected graph for representing overlap of diets
trophic_network <- graph_from_edgelist(tl_matrix, directed = FALSE)

# convert object to adjacency matrix
adj_matrix <- as_adjacency_matrix(trophic_network, sparse=FALSE)

# ~~~~~~~~~~~~~~~~~~~~~~~ ----

# Visualize Networks ----

# See the documentation, in particular adding independent abundances and coloring links:

# https://cran.r-project.org/web/packages/bipartite/vignettes/PlottingWithBipartite.html

# Essentially you need a vector of abundances for each subset of taxa (inverts & birds)

# invert abundances are calculated in the "focal_taxa_abundances_by_habitat.R" script and saved to file as invert_abundances.csv

# load in intert abundances ----

invert_abundances <- read_csv("data/for_Emilio/invert_abundances.csv")

invert_abun_vp <- invert_abundances %>% 
  filter(zone == "vernal pool") %>% 
  filter(ln_count > 0)

# TODO- subset the abundances similarly for other zones


### vernal pools ----

matrix_subset_vp <- adj_matrix[vp_birds_invertivorous$species, inverts]

# color matrix for vernal pools
link_colors_vp <- matrix("gray80",
                       nrow = nrow(matrix_subset_vp),
                       ncol = ncol(matrix_subset_vp))

# assign colors for remaining prey taxa
# then overwrite each value with a different color for each column (prey taxon)

# Ostracoda
link_colors_vp[, colnames(matrix_subset_vp) == "Ostracoda"][
  matrix_subset_vp[, colnames(matrix_subset_vp) == "Ostracoda"] > 0] <- "#264653"

# Corixidae
link_colors_vp[, colnames(matrix_subset_vp) == "Corixidae"][
  matrix_subset_vp[, colnames(matrix_subset_vp) == "Corixidae"] > 0] <- "#287271"

# Chironomidae
link_colors_vp[, colnames(matrix_subset_vp) == "Chironomidae"][
  matrix_subset_vp[, colnames(matrix_subset_vp) == "Chironomidae"] > 0] <- "#2a9d8f"

# Oligochaeta
link_colors_vp[, colnames(matrix_subset_vp) == "Oligochaeta"][
  matrix_subset_vp[, colnames(matrix_subset_vp) == "Oligochaeta"] > 0] <- "#8ab17d"

# Copepoda
link_colors_vp[, colnames(matrix_subset_vp) == "Copepoda"][
  matrix_subset_vp[, colnames(matrix_subset_vp) == "Copepoda"] > 0] <- "#babb74"

# Ephydridae
link_colors_vp[, colnames(matrix_subset_vp) == "Ephydridae"][
  matrix_subset_vp[, colnames(matrix_subset_vp) == "Ephydridae"] > 0] <- "#e9c46a"

# Cladocera
link_colors_vp[, colnames(matrix_subset_vp) == "Cladocera"][
  matrix_subset_vp[, colnames(matrix_subset_vp) == "Cladocera"] > 0] <- "#f4a261"

# Nematoda
link_colors_vp[, colnames(matrix_subset_vp) == "Nematoda"][
  matrix_subset_vp[, colnames(matrix_subset_vp) == "Nematoda"] > 0] <- "#ee8959"

# Ceratopogonidae
link_colors_vp[, colnames(matrix_subset_vp) == "Ceratopogonidae"][
  matrix_subset_vp[, colnames(matrix_subset_vp) == "Ceratopogonidae"] > 0] <- "#e76f51"



# # add arguments for abundances and colors: link_color = link_colors1, 
# higher_abundances = invert_abundances_vector, lower_abundances = bird_invert_abundances_vector, 

# open png graphics
png(file = "figures/Ojemann_Emilio/invert_network_vp_color.png", width = 600, height = 900, units = "px", res = 100)


# plotting command
plotweb(web = matrix_subset_vp, text_size =0.8, horizontal = TRUE, link_color = link_colors_vp)

# close png
dev.off()


###  phelps junction ----
matrix_subset_phelps <- adj_matrix[phelps_birds_invertivorous$species, inverts]

# color matrix for phelps
link_colors_phelps <- matrix("gray80",
                         nrow = nrow(matrix_subset_phelps),
                         ncol = ncol(matrix_subset_phelps))

# assign colors for remaining prey taxa
# then overwrite each value with a different color for each column (prey taxon)

# Ostracoda
link_colors_phelps[, colnames(matrix_subset_phelps) == "Ostracoda"][
  matrix_subset_phelps[, colnames(matrix_subset_phelps) == "Ostracoda"] > 0] <- "#264653"

# Corixidae
link_colors_phelps[, colnames(matrix_subset_phelps) == "Corixidae"][
  matrix_subset_phelps[, colnames(matrix_subset_phelps) == "Corixidae"] > 0] <- "#287271"

# Chironomidae
link_colors_phelps[, colnames(matrix_subset_phelps) == "Chironomidae"][
  matrix_subset_phelps[, colnames(matrix_subset_phelps) == "Chironomidae"] > 0] <- "#2a9d8f"

# Oligochaeta
link_colors_phelps[, colnames(matrix_subset_phelps) == "Oligochaeta"][
  matrix_subset_phelps[, colnames(matrix_subset_phelps) == "Oligochaeta"] > 0] <- "#8ab17d"

# Copepoda
link_colors_phelps[, colnames(matrix_subset_phelps) == "Copepoda"][
  matrix_subset_phelps[, colnames(matrix_subset_phelps) == "Copepoda"] > 0] <- "#babb74"

# Ephydridae
link_colors_phelps[, colnames(matrix_subset_phelps) == "Ephydridae"][
  matrix_subset_phelps[, colnames(matrix_subset_phelps) == "Ephydridae"] > 0] <- "#e9c46a"

# Cladocera
link_colors_phelps[, colnames(matrix_subset_phelps) == "Cladocera"][
  matrix_subset_phelps[, colnames(matrix_subset_phelps) == "Cladocera"] > 0] <- "#f4a261"

# Nematoda
link_colors_phelps[, colnames(matrix_subset_phelps) == "Nematoda"][
  matrix_subset_phelps[, colnames(matrix_subset_phelps) == "Nematoda"] > 0] <- "#ee8959"

# Ceratopogonidae
link_colors_phelps[, colnames(matrix_subset_phelps) == "Ceratopogonidae"][
  matrix_subset_phelps[, colnames(matrix_subset_phelps) == "Ceratopogonidae"] > 0] <- "#e76f51"


# # add arguments for abundances and colors: link_color = link_colors1, 
# higher_abundances = invert_abundances_vector, lower_abundances = bird_invert_abundances_vector, 

# open png graphics
png(file = "figures/Ojemann_Emilio/invert_network_phelps_color.png", width = 6, height = 8, units = "in", res = 200)

# plot
plotweb(web = matrix_subset_phelps, text_size =1, horizontal = TRUE, link_color = link_colors_phelps)

# close png
dev.off()

###  NCOS ponds ----

matrix_subset_ponds <- adj_matrix[ponds_birds_invertivorous$species, inverts]

# color matrix for phelps
link_colors_ponds <- matrix("gray80",
                             nrow = nrow(matrix_subset_ponds),
                             ncol = ncol(matrix_subset_ponds))

# assign colors for remaining prey taxa
# then overwrite each value with a different color for each column (prey taxon)

# Ostracoda
link_colors_ponds[, colnames(matrix_subset_ponds) == "Ostracoda"][
  matrix_subset_ponds[, colnames(matrix_subset_ponds) == "Ostracoda"] > 0] <- "#264653"

# Corixidae
link_colors_ponds[, colnames(matrix_subset_ponds) == "Corixidae"][
  matrix_subset_ponds[, colnames(matrix_subset_ponds) == "Corixidae"] > 0] <- "#287271"

# Chironomidae
link_colors_ponds[, colnames(matrix_subset_ponds) == "Chironomidae"][
  matrix_subset_ponds[, colnames(matrix_subset_ponds) == "Chironomidae"] > 0] <- "#2a9d8f"

# Oligochaeta
link_colors_ponds[, colnames(matrix_subset_ponds) == "Oligochaeta"][
  matrix_subset_ponds[, colnames(matrix_subset_ponds) == "Oligochaeta"] > 0] <- "#8ab17d"

# Copepoda
link_colors_ponds[, colnames(matrix_subset_ponds) == "Copepoda"][
  matrix_subset_ponds[, colnames(matrix_subset_ponds) == "Copepoda"] > 0] <- "#babb74"

# Ephydridae
link_colors_ponds[, colnames(matrix_subset_ponds) == "Ephydridae"][
  matrix_subset_ponds[, colnames(matrix_subset_ponds) == "Ephydridae"] > 0] <- "#e9c46a"

# Cladocera
link_colors_ponds[, colnames(matrix_subset_ponds) == "Cladocera"][
  matrix_subset_ponds[, colnames(matrix_subset_ponds) == "Cladocera"] > 0] <- "#f4a261"

# Nematoda
link_colors_ponds[, colnames(matrix_subset_ponds) == "Nematoda"][
  matrix_subset_ponds[, colnames(matrix_subset_ponds) == "Nematoda"] > 0] <- "#ee8959"

# Ceratopogonidae
link_colors_ponds[, colnames(matrix_subset_ponds) == "Ceratopogonidae"][
  matrix_subset_ponds[, colnames(matrix_subset_ponds) == "Ceratopogonidae"] > 0] <- "#e76f51"


# # add arguments for abundances and colors: link_color = link_colors1, 
# higher_abundances = invert_abundances_vector, lower_abundances = bird_invert_abundances_vector, 

# open png graphics
png(file = "figures/Ojemann_Emilio/invert_network_ponds_color.png", width = 6, height = 8, units = "in", res = 200)

# plot
plotweb(web = matrix_subset_ponds, text_size =1, horizontal = TRUE, link_color = link_colors_ponds)

# close png
dev.off()


### EWS (overall Dev. Slough)  ----

matrix_subset_ews <- adj_matrix[ews_birds_invertivorous$species, inverts] 

# filtering abundance data to ews

invert_abun_ews <- invert_abundances |>  
  filter(zone == "main slough") |>  
  filter(ln_count > 0)

invert_abun_ews_vector <- invert_abun_ews




# color matrix for phelps
link_colors_ews <- matrix("gray80",
                            nrow = nrow(matrix_subset_ews),
                            ncol = ncol(matrix_subset_ews))

# assign colors for remaining prey taxa
# then overwrite each value with a different color for each column (prey taxon)

# Ostracoda
link_colors_ews[, colnames(matrix_subset_ews) == "Ostracoda"][
  matrix_subset_ews[, colnames(matrix_subset_ews) == "Ostracoda"] > 0] <- "#264653"

# Corixidae
link_colors_ews[, colnames(matrix_subset_ews) == "Corixidae"][
  matrix_subset_ews[, colnames(matrix_subset_ews) == "Corixidae"] > 0] <- "#287271"

# Chironomidae
link_colors_ews[, colnames(matrix_subset_ews) == "Chironomidae"][
  matrix_subset_ews[, colnames(matrix_subset_ews) == "Chironomidae"] > 0] <- "#2a9d8f"

# Oligochaeta
link_colors_ews[, colnames(matrix_subset_ews) == "Oligochaeta"][
  matrix_subset_ews[, colnames(matrix_subset_ews) == "Oligochaeta"] > 0] <- "#8ab17d"

# Copepoda
link_colors_ews[, colnames(matrix_subset_ews) == "Copepoda"][
  matrix_subset_ews[, colnames(matrix_subset_ews) == "Copepoda"] > 0] <- "#babb74"

# Ephydridae
link_colors_ews[, colnames(matrix_subset_ews) == "Ephydridae"][
  matrix_subset_ews[, colnames(matrix_subset_ews) == "Ephydridae"] > 0] <- "#e9c46a"

# Cladocera
link_colors_ews[, colnames(matrix_subset_ews) == "Cladocera"][
  matrix_subset_ews[, colnames(matrix_subset_ews) == "Cladocera"] > 0] <- "#f4a261"

# Nematoda
link_colors_ews[, colnames(matrix_subset_ews) == "Nematoda"][
  matrix_subset_ews[, colnames(matrix_subset_ews) == "Nematoda"] > 0] <- "#ee8959"

# Ceratopogonidae
link_colors_ews[, colnames(matrix_subset_ews) == "Ceratopogonidae"][
  matrix_subset_ews[, colnames(matrix_subset_ews) == "Ceratopogonidae"] > 0] <- "#e76f51"


# # add arguments for abundances and colors: link_color = link_colors1, 
# higher_abundances = invert_abundances_vector, lower_abundances = bird_invert_abundances_vector, 

glimpse(invert_abundances)

# open png graphics
png(file = "figures/Ojemann_Emilio/invert_network_ews_color.png", width = 6, height = 8, units = "in", res = 200)

# plot
plotweb(web = matrix_subset_ews, text_size =1, horizontal = TRUE, link_color = link_colors_ews)

# close png
dev.off()


#TODO- filter invertebrate nodes to taxa actually present at sampling sites
#continue filling out trophic links data
#create draft network viz based on existing data 
#
# Abundance data needed to create a weighted matrix
# link width = size of interaction
# Utilize plotweb() to get weight
# 
# Looking at:
# Habitats supporting the most involved networks
# Vernal pools vs. ponds vs. slough
# What taxa are seen everywhere, what taxa are not?
# 
# Bigger next steps:
### Seasonal breakdown
### Revisit time period availability
### What new data do we have to add?
### Can we do any prediction/simulation with the data?
### Poster/visualizing -- story map? interactive map? what figures best display the results we have


# COLORS ----
# Assign colors by prey type

inverts

unique(invert_abundances$invert_taxon)

# obtaining invert abundances
# filter to actual existing taxa




