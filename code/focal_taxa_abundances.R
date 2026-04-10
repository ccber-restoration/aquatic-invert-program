# the purpose of this script is to calculate abundances of the most common invertebrate groups (across all sampling sites)
# these values will be used to visualize potential trophic links between inverts and birds


# run setup script
source("code/00_setup.R")


#summarize sample types

sample_type_summary <- invert_data %>% 
  group_by(sample_type) %>% 
  summarize(n_samples = n())

#most samples are FB250, but that is only planktonic things...

invert_data_sweepnet <- invert_data %>% 
  filter(sample_type %in% c("SW250", "SW500"))

#list of 9 focal taxa:
# Ostracoda, Corixidae, Chironimidae, Oligochaeta, Copepoda, Ephydridae, Cladocera, Nematoda, Ceratopogonidae

# Corresponding column names: 
# ostracod, copepod, hemiptera_corixidae_boatman, cladocera, nematode


invert_data_sw_focal_sp <- invert_data_sweepnet %>% 
  select(date_data_entered:nematode, diptera_ceratopogonidae, diptera_chironomid, diptera_ephydridae,annelida_oligochaete)

#pivot long
inverts_focal_long <- invert_data_sw_focal_sp %>% 
  select(ostracod:annelida_oligochaete) %>% 
  pivot_longer(names_to = "taxon", cols = ostracod:annelida_oligochaete) 
# 9 focal taxa * 84 samples = 756 rows

taxon_abundance_summary <- inverts_focal_long %>% 
  group_by(taxon) %>% 
  summarize(n_samples = n(),
            mean_count = round(mean(value), 2),
            median_count = median(value),
            max_count = max(value),
            ln_count = log((mean_count + 1))) %>% 
  mutate(invert_taxon = replace_values(taxon,
      "annelida_oligochaete" ~ "Oligochaeta",
      "cladocera" ~ "Cladocera",
      "copepod" ~ "Copepoda",
      "diptera_ceratopogonidae" ~ "Ceratopogonidae",
      "diptera_chironomid" ~ "Chironimidae",
      "diptera_ephydridae" ~ "Ephydridae",
      "hemiptera_corixidae_boatman" ~ "Corixidae",
      "nematode" ~ "Nematoda",
      "ostracod" ~ "Ostracoda"
      
  )) %>% 
  relocate(invert_taxon, .after = taxon)

write_csv(taxon_abundance_summary, "data/for_Gabriella/invert_abundances.csv")
