# ============================================
# Project: NZ Mudsnail
# File: nzms_exploration.R
# Author: Jon Seawards
# Date: 2026-01-20
# Purpose: Explore and anylize NZ mudsnail data
# ============================================

source("code/Seawards_Jon/jon_code/01_setup.R")

# Exploratory Questions: (using: nzm_onlypresent)

# What sites has NZ Mudsnail been present? 

# What sites has NZ Mudsnail been present in abundance >1? 

# What date was NZ mudsnail first detected?

# When NZ Mudsnail invaded, what was the median organism density within a given sample?  (median number of organisms detected in a single positive sample)

# What is the median NZ mudsnail density at each site with invasion presence? 



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