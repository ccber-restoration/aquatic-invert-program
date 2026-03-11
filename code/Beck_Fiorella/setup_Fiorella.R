# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name:           setup_Fiorella.R
# Description:    Filter code to subset of needed data
# Author(s):      Fiorella Beck

# Inputs:         
#                 
# Outputs:        
# 
# Notes:          
#                 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# run the general setup script ----
source("code/00_setup.R")

#list of Phelps Creek sampling sites
phelps_site <-c("NPB","NPB1","NPB2")

#list of dipnet (sweepnet) sample types
sample_SW<- c("SW250","SW500")

#filter water quality data to just phelps sites
water_quality_NPB<-water_quality %>%
  filter(site %in% phelps_site) %>%
  mutate(dissolved_oxygen_mg_l = as.numeric(dissolved_oxygen_mg_l),
         conductivity_specific_m_s_cm = as.numeric(conductivity_specific_m_s_cm),
         salinity_ppt = as.numeric(salinity_ppt)) %>%
  mutate(season=case_when(
           month %in% c(1,2,3) ~ "Winter",
           month %in% c(4,5,6) ~ "Spring",
           month %in% c(7,8,9) ~ "Summer",
           month %in% c(10,11,12) ~ "Fall")) %>%
   relocate(season,.after = date)

#filter invert data to just Phelps sites and dipnet samples
invert_data_NPB<-invert_data%>%
  filter(site %in% phelps_site)%>%
  filter(sample_type %in% sample_SW) 
  

# plot water quality over time by site ----


#Next steps:
#Filter taxa to bioassement categories, pivot_longer(),format list of indicator taxa

## temp -----

fig_phelps_temp <- ggplot(data = water_quality_NPB, aes(x = date, y = temperature_c, group = site, color = site)) +
  geom_point() +
  geom_line() +
  xlab("Date") +
  ylab("Temperature (C)") +
  theme_cowplot() +
  #facet_wrap(vars(site), nrow = 3) +
  ggtitle("Temperature (C) of Phelps Creek by Site") +
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")
  
fig_phelps_temp

#ggplotly(fig_phelps_temp)

## DO -----

fig_phelps_DO <- ggplot(data = water_quality_NPB, aes(x = date, y = dissolved_oxygen_mg_l, group = site, color = site)) +
  geom_point() +
  geom_line() +
  xlab("Date") +
  ylab("Dissolved Oxygen (mg/L)") +
  theme_cowplot() +
  #facet_wrap(vars(site), nrow = 3)+
  ggtitle("DO levels (mg/L) of Phelps Creek by Site")
  

fig_phelps_DO

## pH ----

fig_phelps_pH <- ggplot(data = water_quality_NPB, aes(x = date, y = p_h, group = site, color = site)) +
  geom_point() +
  geom_line() +
  xlab("Date") +
  ylab("pH") +
  theme_cowplot() +
  #facet_wrap(vars(site), nrow = 3)+
  ggtitle("pH levels of Phelps Creek by Site") 

fig_phelps_pH

## conductivity ----

#TODO- make conductivity units microSiemens/cm, not milliSiemens
# can just switch to new_conductivity column
fig_phelps_cond <- ggplot(data = water_quality_NPB, aes(x = date, y = new_conductivity_u_s_cm, group = site, color = site)) +
  geom_point() +
  geom_line() +
  xlab("Date") +
  ylab("Specific Conductivity (uS/cm)") +
  theme_cowplot() +
  #facet_wrap(vars(site), nrow = 3, scales = "free_y")+
  ggtitle("Specific Conductivity (uS/cm) levels of Phelps Creek by Site")

fig_phelps_cond

#ggplotly(fig_phelps_cond)

## salinity ----

fig_phelps_salt <- ggplot(data = water_quality_NPB, aes(x = date, y = salinity_ppt, group = site, color = site)) +
  geom_point() +
  geom_line() +
  xlab("Date") +
  ylab("Salinity (ppt)") +
  theme_cowplot() +
  #facet_wrap(vars(site), nrow = 3, scales = "free_y")+
  ggtitle("Salinity (ppt) levels of Phelps Creek by Site") 

fig_phelps_salt


# Inverts ----
## Chironomid ----
fig_phelps_chironomid <- ggplot(data = invert_data_NPB, aes(x = site, y = diptera_chironomid, group = site, color = site)) +
  geom_boxplot()+
  xlab("Site") +
  ylab("Chironomid Abundance (count)") +
  theme_cowplot() +
  #facet_wrap(vars(site), nrow = 3, scales = "free_y")+
  ggtitle("Chironomid Abundance (count) by Site") 

fig_phelps_chironomid

## Copepod ----

fig_phelps_copepod <- ggplot(data = invert_data_NPB, aes(x = site, y = copepod, group = site, color = site)) +
  geom_boxplot() +
  xlab("Site") +
  ylab("Copepod Abundance (count)") +
  theme_cowplot() +
  #facet_wrap(vars(site), nrow = 3, scales = "free_y")+
  ggtitle("Copepod Abundance (count) by Site") 

fig_phelps_copepod

## Ostracod ----


fig_phelps_ostracod <- ggplot(data = invert_data_NPB, aes(x = site, y = ostracod, group = site, color = site)) +
  geom_boxplot() +
  xlab("Site") +
  ylab("Ostracod Abundance (count)") +
  theme_cowplot() +
  #facet_wrap(vars(site), nrow = 3, scales = "free_y")+
  ggtitle("Ostracod Abundance (count) by Site") 

fig_phelps_ostracod

## Gastropod ----


fig_phelps_gastropod <- ggplot(data = invert_data_NPB, aes(x = site, y = gastropod_snail, group = site, color = site)) +
  geom_boxplot() +
  xlab("Site") +
  ylab("Gastropod Abundance (count)") +
  theme_cowplot() +
  #facet_wrap(vars(site), nrow = 3, scales = "free_y")+
  ggtitle("Gastropod Abundance (count) by Site") 

fig_phelps_gastropod


## Amphipod----

fig_phelps_ampipod <- ggplot(data = invert_data_NPB, aes(x = site, y = amphipod, group = site, color = site)) +
  geom_boxplot() +
  xlab("Site") +
  ylab("Amphipod Abundance (count)") +
  theme_cowplot() +
  #facet_wrap(vars(site), nrow = 3, scales = "free_y")+
  ggtitle("Amphipod Abundance (count) by Site") 

fig_phelps_ampipod


## NZ mudsnail ------

fig_phelps_nz_mudsnail <- ggplot(data = invert_data_NPB, aes(x = site, y = nz_mudsnail, group = site, color = site)) +
  geom_boxplot() +
  xlab("Site") +
  ylab("New Zealand Mudsnail Abundance (count)") +
  theme_cowplot() +
  #facet_wrap(vars(site), nrow = 3, scales = "free_y")+
  ggtitle("New Zealand Mudsnail Abundance (count) by Site") 

fig_phelps_nz_mudsnail

#inverts over time

## Copepod ----

fig_phelps_copepod_time <- ggplot(data = invert_data_NPB, aes(x = year, y = copepod, group = site, color = site)) +
  geom_point() +
  #geom_line() +
  xlab("Year") +
  ylab("Copepod Abundance (count)") +
  theme_cowplot() +
  #facet_wrap(vars(site), nrow = 3, scales = "free_y")+
  ggtitle("Copepod Abundance (count) by Site") 

fig_phelps_copepod_time

## Chironomid ----
fig_phelps_chironomid_time <- ggplot(data = invert_data_NPB, aes(x = year, y = diptera_chironomid, group = site, color = site)) +
  geom_point() +
  #geom_line() +
  xlab("Site") +
  ylab("Chironomid Abundance (count)") +
  theme_cowplot() +
  facet_wrap(vars(site), nrow = 3, scales = "free_y")+
  ggtitle("Chironomid Abundance (count) by Site") 

fig_phelps_chironomid_time




# Plots by season ----

water_quality_NPB_winter <- water_quality_NPB %>%
  filter(season=="Winter")

## temp by season ----

# what was the hottest temperature at each site, in each year?

phelps_temp_extremes <- water_quality_NPB %>%
  
  group_by(site, year(date)) %>% 
  summarize(max_temp = max(temperature_c, na.rm = TRUE),
            min_temp = min(temperature_c, na.rm = TRUE),
            n = n())

#mean temperature by site and month (across years, note some small sample sizes)
phelps_monthly_temp <- water_quality_NPB %>% 
  group_by(site, month) %>% 
  summarize(mean_temp = mean(temperature_c, na.rm = TRUE),
            n = n())



fig_phelps_temp_winter <- ggplot(data = water_quality_NPB_winter, aes(x = month, y = temperature_c, group = site, color = site)) +
  geom_point() +
  xlab("Month") +
  ylab("Temperature (C)") +
  theme_cowplot() +
  #facet_wrap(vars(site), nrow = 3, scales = "free_y")+
  ggtitle("Temperature (C) levels of Phelps Creek by Site During Winter") +
  scale_x_continuous(breaks = c(1,2,3))
  #scale_x_date(date_breaks="1 month")

fig_phelps_temp_winter

## DO by season ----

fig_phelps_DO_winter <- ggplot(data = water_quality_NPB_winter, aes(x = month, y = dissolved_oxygen_mg_l, group = site, color = site)) +
  geom_point() +
  xlab("Month") +
  ylab("Dissolved Oxygen (mg/L)") +
  theme_cowplot() +
  facet_wrap(vars(site), nrow = 3, scales = "free_y")+
  ggtitle("Dissolved Oxygen (mg/L) levels of Phelps Creek by Site During Winter") +
  scale_x_date(date_breaks="1 month")

fig_phelps_DO_winter

fig_phelps_salinity_winter <- ggplot(data = water_quality_NPB_winter, aes(x = month, y = salinity_m_s_cm, group = site, color = site)) +
  geom_point() +
  xlab("Month") +
  ylab("Dissolved Oxygen (mg/L)") +
  theme_cowplot() +
  facet_wrap(vars(site), nrow = 3, scales = "free_y")+
  ggtitle("Dissolved Oxygen (mg/L) levels of Phelps Creek by Site During Winter") +
  scale_x_date(date_breaks="1 month")

fig_phelps_salinity_winter

  



# example code for _writing to file as pdf
ggsave(filename = "figures/Beck_Fiorella/Phelps_Creek_temp_DRAFT.png",
       plot= fig_phelps_temp,

       bg = "white")
ggsave(filename = "figures/Beck_Fiorella/Phelps_Creek_DO_DRAFT.png", plot= fig_phelps_DO, bg = "white")





