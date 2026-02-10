# ============================================
# Project: NZ Mudsnail
# File: nzms_exploration.R
# Author: Jon Seawards
# Date: 2026-01-20
# Purpose: Explore and anylize NZ mudsnail data
# ============================================

source("code/Seawards_Jon/jon_code/01_setup.R")

# Exploratory Questions: (using: invert_data_nzm_separate)

# At what sites in NCOS has NZMS been detected and when? 
invert_data_nzm_separate %>%
  distinct(site)
# NPB2, NPB1, MO1, NVB, NPB, NDC  

# all NPB codes are Phelps Creek
# NDC is Devereux Creek
#NVB - should be NVBR? - Venoco Bridge...
# MO1 = Mouth of slough

# What sites has NZ Mudsnail been present in abundance >1? 
invert_data_nzm_separate %>%
  filter(nz_mudsnail > 1) %>%
  distinct(site)
# 1 NPB2, NPB1, NPB  #

#TODO What date was NZ mudsnail first detected?

# When NZ Mudsnail invaded, what was their median organism density within a given sample?  (median number of organisms detected in a single positive sample)
invert_data_nzm_separate %>%
  summarize(median_density = median(nz_mudsnail, na.rm = TRUE)) %>%
  pull(median_density)
# 29.5 

#TODO- consider plotting as a histogram (using ggplot)


# What is the median NZ mudsnail density at each site with invasion presence? 
invert_data_nzm_separate %>%
  group_by(site) %>%
  summarize(median_density = median(nz_mudsnail),
            n_samples = n())


invert_data_nzm_separate %>%
  group_by(site) %>%
  summarize(median_density = median(nz_mudsnail)) %>%
  #sort by median density, descending
  arrange(-median_density) %>% 
  #overwrite site code, sorting by median density rather than alphabetical
  mutate(site = factor(site, levels = site)) %>% 
  ggplot(aes(x = site, y = median_density)) +
  geom_col() 

# Exploratory Questions: (using: invert_data_nzm)

# What year had the largest proportion of NZ Mudsnail invasion? (highest instances of invaded samples >1/total samples)

# What is the probability you will find NZ Mudsnail in abundance >1 in a given sample?

# Are there variables that skew this probability with statistical relevance? (p< 0.05)
#eg. sp conductivity, temperature, Chl-a, pH, spatial location in estuary/hotspots, other organism correlations 
# ^^ Use new integrated data set to answer this with statistical test __. 


# Discussion Questions:

# What do the above answers tell us about the invasion patterns of NZ Mudsnail? 

# What theories can we support with the above evidence? 

# How can we use this information help us continue to monitor NZ Mdusnail presence in NCOS?

# What is the ecological risk assessment of this invaison?

# Areas for continued exploration? Unanswered questions and partially supported theories..



#TODO:
# 1. Use 'unique' fxn to answer exploratory questions
# 3. Create exploratory figures

#A few more tips:

#if you put date on the x-axis, use the scale_x_date() argument rather than scale_x_continuous

#https://ggplot2.tidyverse.org/reference/scale_date.html