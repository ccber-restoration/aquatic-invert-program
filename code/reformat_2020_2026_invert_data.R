# The purpose of this script is to reformat data from the "old" Google Sheet before integrating it into the "new" Excel Spreadsheet on Box

# run setup script
source("code/00_setup.R")

library(writexl)

taxa_names <- colnames(invert_data) %>% 
  as.data.frame()

# pivot invert_data long

invert_data_long <- invert_data %>% 
  relocate(comments, .after = sample_type) %>% 
  pivot_longer(cols = c(ostracod:unknown), names_to = "taxon", values_to = "count") %>% 
  mutate(life_stage = NA) %>% 
  select(c(date_on_vial, site, sample_type, taxon, count, life_stage, comments)) %>% 
  filter(count > 0) %>% 
  mutate(taxon = taxon %>% 
           replace_values(
              "ostracod" ~ "Ostracod",
              "copepod" ~ "Copepoda",
              "hemiptera_corixidae_boatman" ~ "Corixidae",
              "cladocera" ~ "Cladocera",
              "nematode" ~ "Nematoda",
              "dipteran_sp" ~ "Diptera",
              "diptera_chironomid" ~ "Chironomidae",
              "diptera_ceratopogonidae" ~ "Ceratopogonidae",
              "diptera_culicidae" ~ "Culicidae",
              "diptera_culicidae_larvae" ~ "Culicidae_larvae",
              "diptera_anthomyiidae" ~ "Anthomyiidae",
              "diptera_ephydridae" ~ "Ephydridae",
              "diptera_stratiomyidae" ~ "Stratiomyidae",
              "amphipod" ~ "Amphipoda",
              "lepidoptera_crambidae" ~ "Crambidae",
              "hemiptera_mesoveliidae" ~ "Mesoveliidae",
              "hemiptera_notonectidae" ~ "Notonectidae",
              "annelida_sp" ~ "Annelida",
              "annelida_oligochaete" ~ "Oligochaeta",
              "annelida_polychaete" ~ "Polychaeta",
              "ephemeroptera" ~ "Ephemeroptera",
              "odonate" ~ "Odonata",
              "coleoptera_sp" ~ "Coleoptera",
              "coleoptera_dytiscidae" ~ "Dytiscidae",
              "coleoptera_dytiscidae_liodessus_affinis" ~ "Liodessus affinis",
              "coleoptera_dytiscidae_agabus" ~ "Agabus",
              "coleoptera_dytiscidae_colymbetinae" ~ "Colymbetinae",
              "coleoptera_curculionidae_weavil" ~ "Curculionidae",
              "coleoptera_hydrophilidae" ~ "Hydrophilidae",
              "coleoptera_tropiscernus" ~ "Tropisternus",
              "coleoperta_hydraenidae" ~ "Hydraenidae",
              "coleoptera_gyrindae_gyrinini" ~ "Gyrinini",
              "ephemeroptera_sp" ~ "Ephemeroptera",
              "tipulidae_larva" ~ "Tipulidae",
              "plecoptera_sp" ~ "Plecoptera",
               "arachnida_acari_mite" ~ "Acari",
              "trichoptera_sp" ~ "Trichoptera",
              "collembola_springtail" ~ "Collembola",
              "gastropod_snail" ~ "Gastropoda",
              "mollusk" ~ "Mollusca",
              "syrphidae" ~ "Syrphidae",
              "terrestrials" ~ "Terrestrials",
              "isopoda_sphaeroma" ~ "Sphaeroma",
              "clam_shrimp" ~ "Cyclestherida",
              "eristalis_tenax_larvae" ~ "Eristalis tenax",
              "larvae" ~ "larvae",
              "crawfish" ~ "Astacidea",
              "fish" ~ "Actinopterygii",
              "nz_mudsnail" ~ "Potamopyrgus antipodarum",
              "unknown" ~ "unknown"
              )
         
         ) |> 
  # clean sample types
  # THIS NEEDS TO BE CONSISTENT ACROSS DATAFRAMES
  mutate(sample_type = case_when(
    sample_type == "FB 250" ~ "FB250",
    sample_type == "FB 18L" ~ "FB18L",
    .default = sample_type
  )) |> 
  # make date character
  mutate(date_on_vial = as.Date(date_on_vial))
  
# list unique sample types
unique(invert_data_long$sample_type)

# write to file
writexl::write_xlsx(invert_data_long, "data/migration/invert_data_migration_to_new_excel.xlsx")

