# the purpose of this script is to calculate abundances of the most common invertebrate groups (across all sampling sites)
# these values will be used to visualize potential trophic links between inverts and birds


# run setup script
source("code/00_setup.R")

# relevant sites

# Lump NWP and NDC for analyis as ponds
# Lump NVP4 and NVP2 for vernal pools
# Lump NMC, NVBR, and NEC for the overall slough

sites_EO <- c("NPB", "NWP", "NDC", "NEC", "NVP1", "NVP4", "NVBR", "NMC")

#summarize sample types
sample_type_summary <- invert_data %>% 
  group_by(sample_type) %>% 
  summarize(n_samples = n())

# most samples are FB250, but that is only planktonic things...

# filter to 250 and 500 micron sweep net samples
invert_data_sweepnet <- invert_data %>% 
  filter(sample_type %in% c("SW250", "SW500")) %>% 
  # create sample_id (combination of site, date, sample type)
  unite(sample_id, site, date_on_vial, sample_type, sep = "_", remove = FALSE) %>% 
  # filter samples to sites of interest
  filter(site %in% sites_EO) 

# 47 samples, Aug 2023 to Aug 2025


# extract list of samples
metadata_samples_sw <- invert_data_sweepnet %>% 
  select(date_on_vial:sample_type) %>% 
  distinct()

unique(metadata_samples_sw$site)
# 47 samples

# now expand this to create a row for every sample and every taxon...

#list of 9 focal taxa:
# Ostracoda, Corixidae, Chironomidae, Oligochaeta, Copepoda, Ephydridae, Cladocera, Nematoda, Ceratopogonidae

# Corresponding column names: 
# ostracod, copepod, hemiptera_corixidae_boatman, cladocera, nematode

invert_data_sw_focal_sp <- invert_data_sweepnet %>% 
  select(date_data_entered:nematode, diptera_ceratopogonidae, diptera_chironomid, diptera_ephydridae, annelida_oligochaete)

#pivot long
inverts_focal_long <- invert_data_sw_focal_sp %>% 
  #select(ostracod:annelida_oligochaete) %>% 
  pivot_longer(names_to = "taxon", cols = ostracod:annelida_oligochaete) %>% 
  mutate(zone = case_when(
    site == "NWP" | site == "NDC" ~ "pond",
    site == "NVP1" | site == "NVP4" ~ "vernal pool",
    site == "NMC" | site == "NVBR" | site == "NEC" ~ "main slough",
    .default = site
  ))

# 9 taxa * 47 samples = 423 rows
# no need to zero-fill because that already happened (in wide format)


taxon_abundance_summary_by_habitat <- inverts_focal_long %>% 
  group_by(taxon, zone) %>% 
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
  relocate(invert_taxon, .after = taxon) %>% 
  select(-taxon)

# quick visualization
fig_invert_abundance_zone <- ggplot(data = taxon_abundance_summary_by_habitat, aes(x = invert_taxon, y = ln_count)) +
  geom_col() +
  facet_wrap(vars(zone))
  
fig_invert_abundance_zone

write_csv(taxon_abundance_summary_by_habitat, "data/for_Emilio//invert_abundances.csv")
