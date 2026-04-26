# ============================================
# Project: NZ Mudsnail 
# File: 01_setup.R
# Author: Jon Seawards
# Date: 2026-01-20
# Purpose: Clean and filter invertebrate data for NZMS analysis
# ============================================

library(dplyr)

source("code/00_setup.R")

#filter taxa to just nzms

nzms_taxonomic_info <- taxa %>% 
  filter(verbatim_name == "NZ Mudsnail")

#declutter environment
rm(taxa)

# FILTER:events for nzm occurance while keeping other taxa
invert_data_nzm <- invert_data %>%
  filter(nz_mudsnail > 0)

# SEPERATE: only events with nzm occurance and omit other taxa 
invert_data_nzm_separate <- invert_data_nzm %>%
  select(date_on_vial, site, sample_type, nz_mudsnail)


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

#FIXME ^^ can use mutate with case_when to standardize factor (or character) level values (e.g. sample depths)

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

-------------------------------------------------------------------------------

# Merging NZM Counts with Water Quality Data

# find earliest date where NZMS was actually present
first_nzm_date <- data_nzm_wq %>%
  filter(nz_mudsnail > 0) %>%
  summarise(first_date = min(date_on_vial, na.rm = TRUE)) %>%
  pull(first_date)

# make a smaller NZMS table with only the join columns + count
nzm_counts <- data_nzm_wq %>%
  select(site, date_on_vial, nz_mudsnail)

# join onto water quality data, replace missing counts with 0,
# then keep only observations on/after first NZMS occurrence
water_quality_nzm <- water_quality_clean %>%
  left_join(nzm_counts, by = c("site" = "site", "date" = "date_on_vial")) %>%
  mutate(nz_mudsnail = if_else(is.na(nz_mudsnail), 0, nz_mudsnail)) %>%
  filter(date >= first_nzm_date)

# create binary presence column
water_quality_nzm <- water_quality_nzm %>%
  mutate(nzm_presence = if_else(nz_mudsnail > 0, 1, 0))

#create object for site frequency
site_freq <- water_quality_nzm %>%
group_by(site) %>%
  summarise(
    detection_rate = mean(nz_mudsnail > 0, na.rm = TRUE),
    n_samples = n(),
    .groups = "drop"
  ) %>%
  filter(detection_rate > 0)
