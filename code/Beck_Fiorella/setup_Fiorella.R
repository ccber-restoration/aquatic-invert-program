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

#list of Phelps Creek sampling sites
phelps_site <-c("NPB","NPB1","NPB2")

#list of dipnet (sweepnet) sample types
sample_SW<- c("SW250","SW500")

#filter water quality data to just phelps sites
water_quality_NPB<-water_quality %>%
  filter(site %in% phelps_site)

#filter invert data to just Phelps sites and dipnet samples
invert_data_NPB<-invert_data%>%
  filter(site %in% phelps_site)%>%
  filter(sample_type %in% sample_SW)




#Next steps:
#Filter taxa to bioassement categories, pivot_longer(),format list of indicator taxa

# demo figure for temperature
fig_phelps_temp <- ggplot(data = water_quality_NPB, aes(x = date, y = temperature_c, group = site, color = site)) +
  geom_point() +
  geom_line() +
  xlab("Date") +
  ylab("Temperature (C)") +
  theme_cowplot() +
  facet_wrap(vars(site), nrow = 3)
  
fig_phelps_temp


ggsave(filename = "figures/Beck_Fiorella/Phelps_Creek_temp_DRAFT.pdf",
       plot= fig_phelps_temp,
       bg = "white")



