# ============================================
# Project: NZ Mudsnail 
# File: 01_setup.R
# Author: Jon Seawards
# Date: 2026-01-20
# Purpose: Clean and filter invertebrate data for NZ mudsnail alaysis
# ============================================

source("code/00_setup.R")

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

# a few notes from FHJ: use lubridate and hms packages to work with dates and times
# use read_excel arguments to specify column types and na values
# can use mutate with case_when to standardize factor (or character) level values (e.g. sample depths)

#TODO:
# 1. Integrate a cleaned and fixed water quality with dates of NZM occurance


#Next steps: 

# because there are two distinct sets of wq measurements for a single invert sample, take the mean
#my approach would be group_by(site, date), then pipe to summarize() and overwrite the wq variables by taking the mean

#TODO- make water quality variables numeric first...

#can use something like this:

# df <- df %>%
#   mutate(across(c(col1, col2, col3), as.numeric))

#create data frame with NZMS occurrences and corresponding water quality
data_nzm_wq <- invert_data_nzm_separate %>% 
  left_join(y = water_quality, join_by("site" == "site","date_on_vial" == "date")) %>% 
  group_by(site, date_on_vial) %>% 
  summarize(p_h = mean(p_h))


  
  




