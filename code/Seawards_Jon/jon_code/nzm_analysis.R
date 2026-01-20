# ============================================
# Project: NZ Mudsnail ]
# File: nzm_analysis.R
# Author: Jon Seawards
# Date: 2026-01-20
# Purpose: Clean invertebrate data and analyze NZ mudsnail detections
# ============================================

# install packages ---

install.packages("todor")

# load packages ----
library(tidyverse)
library(readxl)
library(janitor)
library(readr)
library(todor)

# Read data (static excel file snapshot)

  library(readxl)
Aquatic_Sampling_Data_2025_09_11_clean <- read_excel("data/Aquatic_Sampling_Data_2025-09-11_clean.xlsx")
View(Aquatic_Sampling_Data_2025_09_11_clean)

invert_data <-read_excel(path = "data/Aquatic_Sampling_Data_2025-09-11_clean.xlsx", sheet = "Aquatic Insects" )
view(invert_data)

# clean taxa data counts (remove NA's)

invert_data_clean <- invert_data %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0)))

# NZM - filter events for mudsnail occurance

mud_col <- "NZ Mudsnail"

invert_data_nzm <- invert_data_clean %>%
  filter(.data[[mud_col]] > 0)
view(invert_data_nzm)

# NZM Only - Omit non NZM Taxa data

meta_cols <- c("Date on Vial", "Site", "Sample Type")
mud_col <- "NZ Mudsnail"

nzm_only <- invert_data_clean %>%
  select(all_of(meta_cols), all_of(mud_col))
view(nzm_only)

# NZM Only Present - Omits all NZM <0 data

nzm_only_present <- invert_data_clean %>%
  select(all_of(meta_cols), all_of(mud_col)) %>%
  filter(.data[[mud_col]] > 0)
view(nzm_only_present)

# Exploratory Questions: (using: nzm_onlypresent)

# What sites has NZ Mudsnail been present? 

  # What sites has NZ Mudsnail been present in abundance >1? 

# What date was NZ mudsnail first detected?

# When NZ Mudsnail invaded, what was the median organismal density within a given sample?  (median number of organisms detected in a single positive sample)

    # What is the median NZ mudsnail density at each site with invasion presence? 



# Exploratory Questions: (using: invert_data_nzm)

# What year had the largest proportion of NZ Mudsnail invasion? (highest instances of invaded samples >1/total samples)

# What is the probability you will find NZ Mudsnail in abundance >1 in a given sample?

  # Are there variables that skew this probability with statistical relevance? (p< 0.05)
  #eg. sp conductivity, temperature, chl-a, pH, spatial location in estuary/hotspots 
# ^^ Use new integreated dataset to answer this with statistical test __. 


# Discussion Questions:

# What do the above answers tell us about the invasion patterns of NZ Mudsnail? 

# What theories can we support with the above evidence? 

# How can we use this information help us continue to monitor NZ Mdusnail presence in NCOS?

# What is the ecological risk assessment of this invaison?

# Areas for continued exploration? Unanswered questions and partially supported theories..



#TODO:
# 1. Import Water quality data and integreate with filtered datasets
# 2. Use 'unique' fxn to answer exploratory questions
# 3. Create exploratory figures





