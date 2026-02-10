# ============================================
# Project: NZ Mudsnail 
# File: 01_setup.R
# Author: Jon Seawards
# Date: 2026-01-20
# Purpose: Clean and filter invertebrate data for NZMS analysis
# ============================================

source("code/00_setup.R")

#filter taxa to just nzms

nzms_taxonomic_info <- taxa %>% 
  filter(verbatim_name == "NZ Mudsnail")

#declutter environment
rm(taxa)

# clean taxa data counts (remove NA's)
invert_data_clean <- invert_data %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0)))

# FILTER:events for nzm occurance while keeping other taxa
invert_data_nzm <- invert_data_clean %>%
  filter(nz_mudsnail > 0)

# SEPERATE: only events with nzm occurance and omit other taxa 
invert_data_nzm_separate <- invert_data_nzm %>%
  select(date_on_vial, site, sample_type, nz_mudsnail)

#FIXME ^^ - sites are not consistently formatted, depth measurement should be factored to standard?, lots of N/A.. 

# can use mutate with case_when to standardize factor (or character) level values (e.g. sample depths)


#clean water_quality data, format as numeric
water_quality_clean <- water_quality %>%
  mutate(across(c(p_h, 
                  dissolved_oxygen_mg_l,
                  dissolved_oxygen_percent,
                  conductivity_specific_m_s_cm,
                  conductivity_specific_u_s_cm,
                  conductivity_specific_ppt,
                  salinity_m_s_cm,
                  salinity_ppt,
                  temperature_c,
                  barometric_pressure_mm_hg), as.numeric))

#create data frame with NZMS occurrences and corresponding water quality
data_nzm_wq <- invert_data_nzm_separate %>% 
  left_join(y = water_quality_clean, join_by("site" == "site","date_on_vial" == "date")) %>% 
  group_by(site, date_on_vial) %>% 
  summarize( sample_type = first(sample_type),
             nz_mudsnail = first(nz_mudsnail),   # keep original columns from nzm_seperate
    
    across(
    c(p_h,
        dissolved_oxygen_mg_l,
        dissolved_oxygen_percent,
        conductivity_specific_m_s_cm,
        conductivity_specific_u_s_cm,
        conductivity_specific_ppt,
        salinity_m_s_cm,
        salinity_ppt,
        temperature_c,
        barometric_pressure_mm_hg),
      ~ mean(.x, na.rm = TRUE)), .groups = "drop")

# small cleanup of NA value 
data_nzm_wq <- data_nzm_wq %>%
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA, .x)))

