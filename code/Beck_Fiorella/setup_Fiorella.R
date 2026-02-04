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
  facet_wrap(vars(site), nrow = 3) +
  ggtitle("Temperature (C) of Phelps Creek by Site")
  
fig_phelps_temp


fig_phelps_DO <- ggplot(data = water_quality_NPB, aes(x = date, y = dissolved_oxygen_mg_l, group = site, color = site)) +
  geom_point() +
  geom_line() +
  xlab("Date") +
  ylab("Dissolved Oxygen (mg/L)") +
  theme_cowplot() +
  facet_wrap(vars(site), nrow = 3)+
  ggtitle("DO levels (mg/L) of Phelps Creek by Site")
  

fig_phelps_DO

fig_phelps_pH <- ggplot(data = water_quality_NPB, aes(x = date, y = p_h, group = site, color = site)) +
  geom_point() +
  geom_line() +
  xlab("Date") +
  ylab("pH") +
  theme_cowplot() +
  facet_wrap(vars(site), nrow = 3)+
  ggtitle("pH levels of Phelps Creek by Site") 

fig_phelps_pH

fig_phelps_cond <- ggplot(data = water_quality_NPB, aes(x = date, y = conductivity_specific_m_s_cm, group = site, color = site)) +
  geom_point() +
  geom_line() +
  xlab("Date") +
  ylab("Specific Conductivity (mS/cm)") +
  theme_cowplot() +
  facet_wrap(vars(site), nrow = 3)+
  ggtitle("Specific Conductivity (mS/cm) levels of Phelps Creek by Site") +
  theme(axis.text.y = element_text(size = 5))

fig_phelps_cond



  

ggsave(filename = "figures/Beck_Fiorella/Phelps_Creek_temp_DRAFT.pdf",
       plot= fig_phelps_temp,
       bg = "white")




