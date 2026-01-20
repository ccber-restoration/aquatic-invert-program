# ============================================
# Project: NZ Mudsnail 
# File: 01_setup.R
# Author: Jon Seawards
# Date: 2026-01-20
# Purpose: Clean and filter invertebrate data for NZ mudsnail alaysis
# ============================================

# load packages ----
library(tidyverse)
library(readxl)
library(janitor)
library(readr)

# Read data (static excel file snapshot)

#TODO FHJ- delete this if you don't need it

Aquatic_Sampling_Data_2025_09_11_clean <- read_excel("data/Aquatic_Sampling_Data_2025-09-11_clean.xlsx")


#View(Aquatic_Sampling_Data_2025_09_11_clean)

invert_data <-read_excel(path = "data/Aquatic_Sampling_Data_2025-09-11_clean.xlsx", sheet = "Aquatic Insects" ) 

#view(invert_data)

# clean taxa data counts (remove NA's)

invert_data_clean <- invert_data %>%
  clean_names() %>% 
  mutate(across(where(is.numeric), ~ replace_na(.x, 0)))

# NZM - filter events for mudsnail occurance

# can delete this since we ran clean_names()
#mud_col <- "NZ Mudsnail"

invert_data_nzm <- invert_data_clean %>%
  filter(nz_mudsnail > 0)
#view(invert_data_nzm)

# NZM Only - Omit non NZM Taxa data

#FHJ note: this is metadata columns for the full data set, 
#create a new object if you want metadata columns for only nzms samples
meta_cols <- invert_data_clean %>% 
  select(date_on_vial, site, sample_type)
  

#FIXME- revisit whether this is necessary/efficient  
#mud_col <- "NZ Mudsnail"
nzm_only <- invert_data_clean %>%
  select(all_of(meta_cols), all_of(mud_col))
#view(nzm_only)

# NZM Only Present - Omits all NZM <0 data

nzm_only_present <- invert_data_clean %>%
  select(all_of(meta_cols), all_of(mud_col)) %>%
  filter(.data[[mud_col]] > 0)
#view(nzm_only_present)


#Import Water quality data and integreate with filtered datasets

water_quality <-read_excel(path = "data/Aquatic_Sampling_Data_2025-09-11_clean.xlsx", sheet = "Water Quality" )
#view(water_quality)
water_quality



#FIXME ^^ - start and end times + dates + sites are not consistently formatted, depth measurement should be factored to standard?, lots of N/A.. 

# a few notes from FHJ: use lubridate and hms packages to work with dates and times
# use read_excel arguments to specify column types and na values
# can use mutate with case_when to standardize factor (or character) level values (e.g. sample depths)

#TODO:
# 1. Integrate a cleaned and fixed water quality with dates of NZM occurance






