# =============================================================================
# Name:           setup_Fiorella.R
# Description:    Filter code to subset of needed data
# Author(s):      Fiorella Beck

# Inputs:         
#                 
# Outputs:        
# 
# Notes:          
#                 
# =============================================================================



#run the general setup script
source("code/00_setup.R")

#note: Francis will update 00_setup.R to also import water quality data

phelps_site <-c("NPB","NPB1","NPB2")
sample_SW<- c("SW250","SW500")

water_quality_NPB<-water_quality %>%
  filter(site %in% phelps_site)

invert_data_NPB<-invert_data%>%
  filter(site %in% phelps_site)%>%
  filter(sample_type %in% sample_SW)

Next steps:
  Filter taxa to bioassement categories, pivot_longer(),format list of indicator taxa