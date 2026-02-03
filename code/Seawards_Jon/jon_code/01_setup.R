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
invert_data <-read_excel(path = "data/Aquatic_Sampling_Data_2026-01-21.xlsx", sheet = "Aquatic Insects" ) 

# clean taxa data counts (remove NA's)
invert_data_clean <- invert_data %>%
  clean_names() %>% 
  mutate(across(where(is.numeric), ~ replace_na(.x, 0)))

# FILTER:events for nzm occurance while keeping other taxa
invert_data_nzm <- invert_data_clean %>%
  filter(nz_mudsnail > 0)

# SEPERATE: only events with nzm occurance and omit other taxa 
invert_data_nzm_separate <- invert_data_nzm %>%
  select(date_on_vial, site, sample_type, nz_mudsnail)

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






