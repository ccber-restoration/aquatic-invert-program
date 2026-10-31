# the purpose of this script is to begin exploring, visualizing, and summarizing water quality measurements

# run 00_setup.R to obtain the water_quality dataframe

# Note that the input dataframe will change slightly after migrating to the "new" spreadsheet


library(naniar)

vis_miss(water_quality)

# Clean up names ----

#determine how many site names/look for errors
unique_names <- unique(water_quality$site)
print(unique_names)

# Site names errors: NPB2 (at road) (NPB2?), NDG (NDC?), VP4 (NVP4?), NEB (NPB?), 
# PB (NPB?), Phelps Hollister (NPB1), VBR1 (??), NVB (NVBR??)

# Correct name errors 
water_quality_correct <- water_quality  %>%
  mutate(site= case_when(site== "NVB" ~ "NVBR",
         site== "NPB2 (at road)" ~ "NPB2",
         site== "NDG" ~ "NDC",
         site== "PB" ~ "NPB",
         site== "VBR1" ~ "NVBR",
         site== "VP4" ~ "NVP4",
         TRUE ~ site)) # I left NEC alone bc I couldn't tell if it was NEC or NPB

#check changes
unique_names2 <- unique(water_quality_correct$site)
print(unique_names2)

#filter out unchanged but erroneous names 
filtered_water_quality<-water_quality_correct %>%
  filter(!site %in% c("NEB","Phelps Hollister"))

# Check Records for each site ----
filtered_water_quality %>%
  count(site)

#CUL1     11
# VP1       5*
# DSP       5* 
# ETNP      1*
# ETSP      1*
# MO1      26
# NDC      17
# NEC      26
# NMC      24
# NPB      51
# NPB1     27
# NPB2     10
# NVBR     34
# NVP2     10
# NVP4     12
# NWP      21
# PIER     25
# *=less than 10 records

#filter out sites with less than 10 data points 
water_quality10 <- filtered_water_quality %>%
  filter(!site %in% c("DSP","ETNP", "ETSP","VP1"))



#Plots: ----

## All sites: ----
water_quality10 <- water_quality10 %>%
  mutate(salinity_ppt = as.numeric(salinity_ppt))



### Salinity ----
Salinity <- ggplot(data = water_quality10, aes(x = year, y = salinity_ppt, group = site, color = site)) +
  geom_point() +
  geom_line()+
  xlab("year") +
  ylab("Salinity (ppt)") +
  theme_cowplot() +
  facet_wrap(vars(site))
 # ggtitle("Salinity by Site") 
                     

Salinity

Salinity_point <- ggplot(data = water_quality10, aes(x = date, y = salinity_ppt, group = site, color = site)) +
  geom_point() +
  xlab("year") +
  ylab("Salinity (ppt)") +
  theme_cowplot() +
 facet_wrap(vars(site)) +
  ylim(0, 0.5)
# ggtitle("Salinity by Site") 


Salinity_point
#NVP2, NVP4, NWP, NPB1, NPB2, NDC are similar

### pH ----

pH <- ggplot(data = water_quality10, aes(x = year, y = p_h, group = site, color = site)) +
  geom_point() +
  geom_line()+
  xlab("year") +
  ylab("pH") +
  theme_cowplot() +
  facet_wrap(vars(site))
# ggtitle("Salinity by Site") 


pH
#largely all similar

### DO ----

DO <- ggplot(data = water_quality10, aes(x = year, y = dissolved_oxygen_percent, group = site, color = site)) +
  geom_point() +
  geom_line()+
  xlab("year") +
  ylab("DO (%)") +
  theme_cowplot() +
  facet_wrap(vars(site))
# ggtitle("Salinity by Site") 

DO

### Temperature ----

Temperature <- ggplot(data = water_quality10, aes(x = year, y = temperature_c, group = site, color = site)) +
  geom_point() +
  geom_line()+
  xlab("year") +
  ylab("Temperature (c)") +
  theme_cowplot() +
  facet_wrap(vars(site))
# ggtitle("Salinity by Site") 

Temperature

### Pressure ----

water_quality10 <- water_quality10 %>%
  mutate(barometric_pressure_mm_hg = as.numeric(barometric_pressure_mm_hg))

Pressure <- ggplot(data = water_quality10, aes(x = year, y = barometric_pressure_mm_hg, group = site, color = site)) +
  geom_point() +
  geom_line()+
  xlab("year") +
  ylab("Barometric Pressure (mm/hg)") +
  theme_cowplot() +
  facet_wrap(vars(site)) 
  #scale_y_continuous(limits = c(750, 800))
# ggtitle("Salinity by Site") 

Pressure

# Salt vs Fresh ----

#check is any of sites are fresh (none are)
#according Senesac report NWP, NDC, NVP2 and NVP4 are fresh??
Salinity_site <- water_quality10 %>%
  filter(site== "NDC") 

Salinity_site <- water_quality10 %>%
  filter(site== "NDC") %>%
  filter(salinity_ppt < 0.5) %>%
 summarise(count = n())

#Upstream plots: ----


upstream_quality <- water_quality10 %>%
  filter(site %in% c("NPB2", "NPB1", "NPB", "NEC"))

##Salinity ----

Upstream_salinity <- ggplot(data = upstream_quality, aes(x = date, y = salinity_ppt, group = site, color = site)) +
  geom_bar(stat = "identity", position = "stack") +
  xlab("year") +
  ylab("Salinity (ppt)") +
  theme_cowplot() +
  facet_wrap(vars(site)) +
  geom_line() 

Upstream_salinity
#salinity between further upstream sites is much lower and hard to compare :(

##pH----
Upstream_pH <- ggplot(data = upstream_quality, aes(x = date, y = p_h, group = site, color = site)) +
  geom_point() +
  geom_line() +
  xlab("year") +
  ylab("pH") +
  theme_cowplot() +
  scale_y_continuous(limits = c(3, 9),breaks = seq(3, 9, by = 1))+
  facet_wrap(vars(site)) +
  geom_line() 
  


Upstream_pH
#largely consistent pattern around around 8-9 = basic

##DO ----

Upstream_DO <- ggplot(data = upstream_quality, aes(x = date, y = dissolved_oxygen_percent, group = site, color = site)) +
  geom_point() +
  geom_line() +
  xlab("year") +
  ylab("DO") +
  theme_cowplot() +
  #scale_y_continuous(limits = c(3, 9),breaks = seq(3, 9, by = 1))
  facet_wrap(vars(site))



Upstream_DO

##Conductivity ----

Upstream_Conductivity <- ggplot(data = upstream_quality, aes(x = date, y = new_conductivity_u_s_cm, group = site, color = site)) +
  geom_point() +
  geom_line() +
  xlab("year") +
  ylab("Conductivity") +
  theme_cowplot() +
  #scale_y_continuous(limits = c(3, 9),breaks = seq(3, 9, by = 1))
  facet_wrap(vars(site))

Upstream_Conductivity


##Temperature ----

Upstream_Temperature <- ggplot(data = upstream_quality, aes(x = date, y = temperature_c, group = site, color = site)) +
  geom_point() +
  geom_line() +
  xlab("year") +
  ylab("Temperature (c)") +
  theme_cowplot() +
  #scale_y_continuous(limits = c(3, 9),breaks = seq(3, 9, by = 1))
  facet_wrap(vars(site))

Upstream_Temperature

#Downstream plots: ----

downstream_quality <- water_quality10 %>%
  filter(site %in% c("NMC", "NVBR", "CUL1", "PIER", "MO1"))

##Salinity ----

Downstream_salinity <- ggplot(data = downstream_quality, aes(x = date, y = salinity_ppt, group = site, color = site)) +
  geom_point() +
  xlab("year") +
  ylab("Salinity (ppt)") +
  theme_cowplot() +
  facet_wrap(vars(site)) +
  geom_line()
  
Downstream_salinity

##pH ----

Downstream_pH <- ggplot(data = downstream_quality, aes(x = date, y = p_h, group = site, color = site)) +
  geom_point() +
  geom_line() +
  xlab("year") +
  ylab("pH") +
  theme_cowplot() +
  scale_y_continuous(limits = c(3, 9),breaks = seq(3, 9, by = 1))+
  facet_wrap(vars(site)) +
  geom_line() 

##DO ----

Downstream_DO <- ggplot(data = downstream_quality, aes(x = date, y = dissolved_oxygen_percent, group = site, color = site)) +
  geom_point() +
  geom_line() +
  xlab("year") +
  ylab("DO") +
  theme_cowplot() +
  #scale_y_continuous(limits = c(3, 9),breaks = seq(3, 9, by = 1))
  facet_wrap(vars(site))


##Conductivity ----

Downstream_Conductivity <- ggplot(data = downstream_quality, aes(x = date, y = new_conductivity_u_s_cm, group = site, color = site)) +
  geom_point() +
  geom_line() +
  xlab("year") +
  ylab("Conductivity") +
  theme_cowplot() +
  #scale_y_continuous(limits = c(3, 9),breaks = seq(3, 9, by = 1))
  facet_wrap(vars(site))

Downstream_Conductivity

##Temperature ----

Downstream_Temperature <- ggplot(data = downstream_quality, aes(x = date, y = temperature_c, group = site, color = site)) +
  geom_point() +
  geom_line() +
  xlab("year") +
  ylab("Temperature (c)") +
  theme_cowplot() +
  #scale_y_continuous(limits = c(3, 9),breaks = seq(3, 9, by = 1))
  facet_wrap(vars(site))

Downstream_Temperature








  