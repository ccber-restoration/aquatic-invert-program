# source previous script

source("code/Ojemann_Emilio/01_read_filter_bird_data.R")

# load packages ----
library(readxl)
library(tidyverse)

#packages we could use for visualizing networks:
library(bipartite)
library(igraph)

#package for adding icons to networks
library(rphylopic)

library(VennDiagram)

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




#COLORS ----
# Assign colors by prey type

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


###  ncos ponds ----

matrix_subset_ponds <- adj_matrix[ponds_birds_invertivorous$species, inverts]

# color matrix for NCOS
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



### ews (main slough)  ----------

matrix_subset_ews <- adj_matrix[ews_birds_invertivorous$species, inverts] 



# color matrix for EWS
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



#ABUNDANCES ---- 

### #load in invert abundances 

invert_abundances <- read_csv("data/for_Emilio/invert_abundances.csv")

# keep relevant columns
# using log count because raw counts vary too much

log_invert_abundances <- invert_abundances[, c("invert_taxon", "ln_count", "zone")]

# edit Chironimidae to Chironomidae (the i to o)

log_invert_abundances$invert_taxon[
  log_invert_abundances$invert_taxon == "Chironimidae"
] <- "Chironomidae"

# # # GOAL: For each zone: # # #
# building two vectors, bird and invert abun vectors

#### ews (main slough) -----------

## bird abundance vector ##

# add log count column to bird summary 
bird_abun_ews <- ews_birds_invertivorous |> 
  mutate(ln_count = log(total_count + 1)) #prevent log(0)

# pull out numbers as a vector
bird_abun_ews_vector <- bird_abun_ews$ln_count
# add species name to each number so R knows which counts are to which birds
names(bird_abun_ews_vector) <- bird_abun_ews$species

## invert abundance ## 

# filter invert abundance table to only ews/main slough
invert_abun_ews_log <- log_invert_abundances |> 
  filter(zone == "main slough")

# pull out numbers as a vector
invert_abun_ews_vector <- invert_abun_ews_log$ln_count
# add names to each number so R knows which counts are to which birds
names(invert_abun_ews_vector) <- invert_abun_ews_log$invert_taxon

## matching vectors to the matrix using names ##

invert_abun_ews_vector <- invert_abun_ews_vector[colnames(matrix_subset_ews)]
bird_abun_ews_vector <- bird_abun_ews_vector[rownames(matrix_subset_ews)]

#### vernal pools ----

## bird abundance vector ##
bird_abun_vp <- vp_birds_invertivorous |>
  mutate(ln_count = log(total_count + 1))

bird_abun_vp_vector <- bird_abun_vp$ln_count
names(bird_abun_vp_vector) <- bird_abun_vp$species

## invert abundance vector ##
invert_abun_vp_log <- log_invert_abundances |>
  filter(zone == "vernal pool")

invert_abun_vp_vector <- invert_abun_vp_log$ln_count
names(invert_abun_vp_vector) <- invert_abun_vp_log$invert_taxon

## matching vectors to match the matrix ##
invert_abun_vp_vector <- invert_abun_vp_vector[colnames(matrix_subset_vp)]
bird_abun_vp_vector   <- bird_abun_vp_vector[rownames(matrix_subset_vp)]


#### phelps junction ----

## - bird abundance vector - ##
bird_abun_phelps <- phelps_birds_invertivorous |>
  mutate(ln_count = log(total_count + 1))

bird_abun_phelps_vector <- bird_abun_phelps$ln_count
names(bird_abun_phelps_vector) <- bird_abun_phelps$species

## - invert abundance vector - ##
# note: "NPB" = phelps zone for log_invert_abundance 
invert_abun_phelps_log <- log_invert_abundances |>
  filter(zone == "NPB")

invert_abun_phelps_vector <- invert_abun_phelps_log$ln_count
names(invert_abun_phelps_vector) <- invert_abun_phelps_log$invert_taxon

## - reorder both vectors to match the matrix - ##
invert_abun_phelps_vector <- invert_abun_phelps_vector[colnames(matrix_subset_phelps)]
bird_abun_phelps_vector   <- bird_abun_phelps_vector[rownames(matrix_subset_phelps)]


#### ncos ponds ---------

#---bird abundance---#
bird_abun_ponds <- ponds_birds_invertivorous |>
  mutate(ln_count = log(total_count + 1))

bird_abun_ponds_vector <- bird_abun_ponds$ln_count
names(bird_abun_ponds_vector) <- bird_abun_ponds$species

#---invert abundance vector---#
invert_abun_ponds_log <- log_invert_abundances |>
  filter(zone == "pond")

invert_abun_ponds_vector <- invert_abun_ponds_log$ln_count
names(invert_abun_ponds_vector) <- invert_abun_ponds_log$invert_taxon

##---get vectors to match---##
invert_abun_ponds_vector <- invert_abun_ponds_vector[colnames(matrix_subset_ponds)]
bird_abun_ponds_vector   <- bird_abun_ponds_vector[rownames(matrix_subset_ponds)]




#Saving as PNG ----
###---### 
# NOTE: 
# all figures with "final" in the file name have abundance AND color
# figures with "color" have only color
# 
# ews_color was inadvertently saved in the process of debugging the abundance code, thus only invert_network_ews and invert_network_ews_final_color are visible

##### ---- ews ----
png(file = "figures/Ojemann_Emilio/invert_network_ews_final_color.png", width = 6, height = 8, units = "in", res = 200)
plotweb(web = matrix_subset_ews, text_size = 1.1, horizontal = TRUE, curved_links = TRUE,
        link_color = link_colors_ews,
        higher_abundances = invert_abun_ews_vector,
        lower_abundances = bird_abun_ews_vector)
dev.off()


##### ---- vernal pools -----
png(file = "figures/Ojemann_Emilio/invert_network_vp_final_color.png", width = 6, height = 8, units = "in", res = 200)
plotweb(web = matrix_subset_vp, text_size = 1.1, horizontal = TRUE, curved_links = TRUE,
        link_color = link_colors_vp,
        higher_abundances = invert_abun_vp_vector,
        lower_abundances = bird_abun_vp_vector)
dev.off()

#### ---- phelps ----
png(file = "figures/Ojemann_Emilio/invert_network_phelps_final_color.png", width = 6, height = 8, units = "in", res = 200)
plotweb(web = matrix_subset_phelps, text_size = 1.1, horizontal = TRUE, curved_links = TRUE,
        link_color = link_colors_phelps,
        higher_abundances = invert_abun_phelps_vector,
        lower_abundances = bird_abun_phelps_vector)
dev.off()

#### ---- ponds----
png(file = "figures/Ojemann_Emilio/invert_network_ponds_final_color.png", width = 6, height = 8, units = "in", res = 200)
plotweb(web = matrix_subset_ponds, text_size = 1.1, horizontal = TRUE, curved_links = TRUE,
        link_color = link_colors_ponds,
        higher_abundances = invert_abun_ponds_vector,
        lower_abundances = bird_abun_ponds_vector)
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----

# VENN Diagram ----

# see tutorial here: https://r-graph-gallery.com/14-venn-diagramm

# Generate 3 sets of 200 words
set1 <- phelps_bird_summary$species
set2 <- ews_bird_summary$species
set3 <- vp_bird_summary$species
set4 <- ponds_bird_summary$species

# Chart
venn.diagram(
  x = list(set1, set2, set3, set4),
  category.names = c("Phelps Creek" , "Slough", "Vernal Pools", "Western Pond"),
  filename = "figures/Ojemann_Emilio/Venn_Diagram.png",
  output=TRUE
)


#   ---- TO-DO ---- 

# filter invertebrate nodes to taxa actually present at sampling sites
# --> would then need to rewrite colors
#   --> Ceratopogonidae never appears at NCOS
#   --> Ephydridae only appears in vp and ponds
# 
# continue filling out trophic links data (????)

# analysis  ----

# CURRENTLY DOING READING INTO THIS:
help("networklevel")

ews_network_metrics <- networklevel(matrix_subset_ews)

# relevant "networklevel" values:

# connectance = standardized number of species combinations or total numner of unique combinations of species
 
# links per species
# nestedness = the disorder of the network, is it ordered or does it deviate from order? Lower temperature = higher nestedness = more ecosystem stability b/c specialists survive by sharing the same resources as generalists, protecting the network fromc secondary extinctions.
# NODF = corrected nestedness. Higher values = higher nestedness
# 




###--- next steps? if any feasible?: ---###
# Seasonal breakdown
#Revisit time period availability
# What new data do we have to add?
# Can we do any prediction/simulation with the data?


###--- Poster/visualizing ---###
# -- story map? 
# interactive map? 
# what figures best display the results we have

