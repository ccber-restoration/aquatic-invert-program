# this script is for standardizing taxonomic names and pulling upstream taxonomic ranks

library(tidyverse)
#library(taxize)
library(taxadb)

#creates local snapshot of Catalog of Life taxonomic backbone
td_create("col")

#read in aquatic invert taxa
taxa <- read_csv("data/taxon_list.csv") %>% 
  select(scientificName) %>% 
  mutate(id = get_ids(scientificName, "col")) %>% 
  mutate(accepted_name = get_names(id, "col"))

taxa_table <- filter_name(taxa$accepted_name)

