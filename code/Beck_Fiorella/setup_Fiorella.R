# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name:           setup_Fiorella.R
# Description:    Analyze biotic and abiotic conditions of Phelps creek 
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

# load a couple more packages

library(mapview) # interactive maps
library(plotly) # interactive ggplot figures

#read in bioassessment data

npb1_bioassessment_chemical <- read_csv("data/Fiorella_bioassessment_data/Bioassessment_Chemical_Data.csv") %>% 
  clean_names() %>% 
  mutate(label = "Bioassessment")

#we want to compare your measurements for temp, ph, DO, and salinity (and/or conductivity)

#TODO- finish writing this to calculate means by sample point (Riffles 1, 2, 3)
npb1_chemical_means <- npb1_bioassessment_chemical %>% 
  group_by(sample_point) %>% 
  summarize( mean_depth = mean(depth_m),
             mean_temperature = mean(temperature_c))


npb1_invert_abundance <- read_csv("data/Fiorella_bioassessment_data/Invertebrate_abundance.csv") %>%
  clean_names()
  


#list of Phelps Creek sampling sites
phelps_site <-c("NPB","NPB1","NPB2")

#list of dipnet (sweepnet) sample types
sample_SW<- c("SW250","SW500")

#filter water quality data to just phelps sites
water_quality_NPB <- water_quality %>%
  filter(site %in% phelps_site) %>%
  mutate(dissolved_oxygen_mg_l = as.numeric(dissolved_oxygen_mg_l),
         conductivity_specific_m_s_cm = as.numeric(conductivity_specific_m_s_cm),
         salinity_ppt = as.numeric(salinity_ppt)) %>%
  mutate(season=case_when(
           month %in% c(1,2,3) ~ "Winter",
           month %in% c(4,5,6) ~ "Spring",
           month %in% c(7,8,9) ~ "Summer",
           month %in% c(10,11,12) ~ "Fall")) %>%
   relocate(season,.after = date) %>% 
  mutate(label = "Quarterly \nmonitoring")

#filter invert data to just Phelps sites and dipnet samples
invert_data_NPB<-invert_data%>%
  filter(site %in% phelps_site)%>%
  filter(sample_type %in% sample_SW) 
  
#filter from three Phelps sites to just NPB1
water_quality_NPB1  <- water_quality_NPB %>% 
  filter(site == "NPB1")

#find mean winter quarter for all value

winter_temp_mean <- water_quality_NPB1 %>% 
  filter(season == "Winter") %>% 
  summarize(mean_temp = mean(temperature_c),
            median_temp = median(temperature_c))

winter_DO_mean <- water_quality_NPB1 %>% 
  filter(season == "Winter") %>% 
  summarize(mean_DO = mean(dissolved_oxygen_mg_l),
            median_DO = median(dissolved_oxygen_mg_l))


winter_pH_mean <- water_quality_NPB1 %>% 
  filter(season == "Winter") %>% 
  summarize(mean_pH = mean(p_h),
            median_pH = median(p_h))

winter_cond_mean <- water_quality_NPB1 %>% 
  filter(season == "Winter") %>% 
  summarize(mean_cond = mean(new_conductivity_u_s_cm),
            median_cond = median(new_conductivity_u_s_cm))


winter_sal_mean <- water_quality_NPB1 %>% 
  filter(season == "Winter") %>% 
  drop_na(salinity_ppt) %>%
  summarize(mean_sal = mean(salinity_ppt),
            median_sal = median(salinity_ppt))


  


#find mean winter quarter temp value


# plot water quality over time by NBP1 in Winter ----

## temp -----

fig_phelps_temp_NPB1 <- ggplot(data = water_quality_NPB1, aes(x = date, y = temperature_c, shape = label)) +
  geom_point(color = "royalblue3") +
  geom_point(data = npb1_bioassessment_chemical, aes(x = date, y = temperature_c, shape = label), size = 2, color = "royalblue3") +
  # geom_point(data = npb1_bioassessment_chemical, aes(x = date, y = temperature_c, color = site), shape = 17, size = 3) +
  geom_line(color = "royalblue3") +
  geom_hline(yintercept = winter_temp_mean$mean_temp, linetype = "dashed") +
  geom_hline(yintercept = winter_temp_mean$median_temp) +
  xlab("Date") +
  ylab("Temperature (C)") +
  theme_cowplot() +
  #facet_wrap(vars(site), nrow = 3) +
  #ggtitle("Temperature (C) of Phelps Creek (NPB1)") +
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y", limits = c(as.POSIXct("2022-01-01"), as.POSIXct("2027-01-01"))) +
  scale_y_continuous(limits = c(9,21))

fig_phelps_temp_NPB1

# ggsave call
ggsave(filename = "figures/Beck_Fiorella/NPB1_temp.pdf",
       plot = fig_phelps_temp_NPB1,
       width = 6,
       height = 4,
       units = "in")


## DO -----

fig_phelps_DO_NPB1 <- ggplot(data = water_quality_NPB1, aes(x = date, y = dissolved_oxygen_mg_l, shape = label)) +
  geom_point(color = "royalblue3") +
  geom_point(data = npb1_bioassessment_chemical, aes(x = date, y = do_mg_l, shape = label), size = 2, color = "royalblue3") +
  geom_line(color = "royalblue3") +
  geom_hline(yintercept = winter_DO_mean$mean_DO, linetype = "dashed") +
  geom_hline(yintercept = winter_DO_mean$median_DO) +
  xlab("Date") +
  ylab("Dissolved Oxygen (mg/l)") +
  theme_cowplot() +
  ggtitle("Dissolved Oxygen (mg/l) of Phelps Creek") +
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y", limits = c(as.POSIXct("2022-01-01"), as.POSIXct("2027-01-01"))) +
  scale_y_continuous(limits = c(0,12))

fig_phelps_DO_NPB1

ggsave(filename = "figures/Beck_Fiorella/NPB1_DO.pdf",
       plot = fig_phelps_DO_NPB1,
       width = 6,
       height = 4,
       units = "in")

## pH -----

fig_phelps_pH_NPB1 <- ggplot(data = water_quality_NPB1, aes(x = date, y = p_h, shape = label)) +
  geom_point(color = "royalblue3") +
  geom_point(data = npb1_bioassessment_chemical, aes(x = date, y = p_h, shape = label), size = 2, color = "royalblue3") +
  geom_line(color = "royalblue3") +
  geom_hline(yintercept = winter_pH_mean$mean_pH, linetype = "dashed") +
  geom_hline(yintercept = winter_pH_mean$median_pH) +
  xlab("Date") +
  ylab("pH") +
  theme_cowplot() +
  ggtitle("pH of Phelps Creek Over Time") +
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y", limits = c(as.POSIXct("2022-01-01"), as.POSIXct("2027-01-01")))
  #scale_y_continuous(limits = c(4,8))

fig_phelps_pH_NPB1

ggsave(filename = "figures/Beck_Fiorella/NPB1_pH.pdf",
       plot = fig_phelps_pH_NPB1,
       width = 6,
       height = 4,
       units = "in")

#ggplotly(fig_phelps_pH_NPB1)

##conductivity ----

fig_phelps_cond_NPB1 <- ggplot(data = water_quality_NPB1, aes(x = date, y = new_conductivity_u_s_cm, shape = label)) +
  geom_point(color = "royalblue3") +
  geom_point(data = npb1_bioassessment_chemical, aes(x = date, y = conductivity_u_s_cm, shape = label), size = 2, color = "royalblue3") +
  geom_line(color = "royalblue3") +
  geom_hline(yintercept = winter_cond_mean$mean_cond, linetype = "dashed") +
  geom_hline(yintercept = winter_cond_mean$median_cond) +
  xlab("Date") +
  ylab("Conductivity (uS/cm)") +
  theme_cowplot() +
  ggtitle("Conductivity (uS/cm) of Phelps Creek Over Time") +
  guides( shape=guide_legend(title = "Data source")) +
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y", limits = c(as.POSIXct("2022-01-01"), as.POSIXct("2027-01-01")))


fig_phelps_cond_NPB1

ggplot2::ggsave(filename = "figures/Beck_Fiorella/NPB1_cond.pdf",
       plot = fig_phelps_cond_NPB1,
       width = 6,
       height = 4,
       units = "in")

## salinity ----

fig_phelps_salinity_NPB1 <- ggplot(data = water_quality_NPB1, aes(x = date, y = salinity_ppt, shape = label)) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
   geom_point(color = "royalblue3") +
  geom_point(data = npb1_bioassessment_chemical, aes(x = date, y = salinity_ppt, shape = label), size = 2, color = "royalblue3") +
  geom_line(color = "royalblue3") +
  #geom_hline(yintercept = winter_sal_mean$mean_sal, linetype = "dashed") +
  #geom_hline(yintercept = winter_sal_mean$median_sal) +
 
  annotate("text", x=as.POSIXct("2024-01-01"), y=0.65, label="upper range of freshwater (0.5)") +
  xlab("Date") +
  ylab("Salinity (ppt)") +
  labs(shape = "Data source") +
  theme_cowplot() +
  #ggtitle("Salinity (ppt) of Phelps Creek Over Time") 
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y", limits = c(as.POSIXct("2022-01-01"), as.POSIXct("2027-01-01"))) 
  # demo font customization: 
  # theme(text = element_text(family = "Avenir"))

fig_phelps_salinity_NPB1

# when saving as png, add the bg argument (for background), default is transparent (alpha)
ggplot2::ggsave(filename = "figures/Beck_Fiorella/NPB1_sal_URCA.png",
       plot = fig_phelps_salinity_NPB1,
       width = 6,
       height = 4,
       bg = "white",
       units = "in")

# Bar Graph of invertebrate abundance  ----

npb1_invert_abundance <- npb1_invert_abundance %>%
  mutate(other = beetle + unknown) %>%
  select(-beetle, -unknown) 
  

npb1_invert_abundance_long <- npb1_invert_abundance %>%
  pivot_longer(cols = -riffle_number,
               names_to = "species",
               values_to = "abundance") %>% 
  mutate(species = fct_reorder(species, abundance, .desc = TRUE))

levels(npb1_invert_abundance_long$species)


barplot_inverte_type <- ggplot(npb1_invert_abundance_long, aes(x = species, y = abundance, fill = riffle_number, group = riffle_number)) +
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Invertebrate Type", y = "Abundance (count)", fill = "Riffle number") +
  
  #facet_wrap(vars(riffle_number)) +
  theme_cowplot() 

barplot_inverte_type

#ggplot2::ggsave(filename = "figures/Beck_Fiorella/boxplot.pdf",
       plot =boxplot_inverte_type,
       width = 6,
       height = 4,
       units = "in")


# plot of invert abundance  ----

## chironomids  ---- 

fig_npb1_chironomid <- ggplot(data = invert_data_NPB, aes(x = site, y = diptera_chironomid, group = site, color = site)) +
  geom_boxplot() +
  geom_boxplot(data = npb1_invert_abundance, aes(x = site, y = diptera_chironomid, group = site, color = site)) +
  xlab("Site") +
  ylab("Chironomid Abundance (count)") +
  theme_cowplot() +
  ggtitle("Chironomid Abundance (count) by Site") 
  
fig_npb1_chironomid 

# plot water quality over time by site ----


#Next steps:
#Filter taxa to bioassement categories, pivot_longer(),format list of indicator taxa

## temp -----

fig_phelps_temp <- ggplot(data = water_quality_NPB1, aes(x = date, y = temperature_c, group = site, color = site)) +
  geom_point() +
  geom_point(data = npb1_bioassessment_chemical, aes(x = date, y = temperature_c, color = site), shape = 17, size = 3) +
  geom_line() +
  geom_hline(yintercept = winter_temp_mean$mean_temp, linetype = "dashed") +
  geom_hline(yintercept = winter_temp_mean$median_temp) +
  xlab("Date") +
  ylab("Temperature (C)") +
  theme_cowplot() +
  #facet_wrap(vars(site), nrow = 3) +
  ggtitle("Temperature (C) of Phelps Creek by Site") +
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y", limits = c(as.POSIXct("2022-01-01"), as.POSIXct("2027-01-01"))) +
  scale_y_continuous(limits = c(9,21))
  
fig_phelps_temp

# example code for _writing to file as pdf
ggsave(filename = "figures/Beck_Fiorella/Phelps_Creek_temp_2026-03-11.pdf",
       plot= fig_phelps_temp,
       width = 6,
       height = 4,
       units = "in",
       bg = "white")

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

  
# Benchmark data----
Benchmark_data <- read_csv("code/Beck_Fiorella/Benchmark_data.csv")

benchmark_data<-Benchmark_data %>%
  clean_names() %>%
  mutate(sampledate = mdy_hms(sampledate, tz = "UTC"))  # or your timezone


# check unique names 
unique(benchmark_data$stationname)

# make benchmark data into a spatial object (sf object)
benchmark_data_sf <- st_as_sf(benchmark_data, coords = c("targetlongitude", "targetlatitude"), crs = 4326) 

# view interactive map
mapview(benchmark_data_sf)

## E. coli data----
e.coli<-benchmark_data %>%
  filter(analyte == "E. coli")
  


E.coli_fig <- ggplot(data = e.coli, aes(x = sampledate, y = result)) +
  geom_point(color = "royalblue3") +
  geom_point(data = e.coli, aes(x = sampledate, y = result), size = 2, color = "royalblue3") +
  scale_y_continuous(limits = c(0,24192.0)) +
  geom_hline(yintercept = 410, linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ggtitle("E.coli levels (MPN/100 mL) overtime") +
  # facet by station
  facet_wrap(vars(stationname))
  
  
E.coli_fig

E.coli_fig_2 <- ggplot(data = e.coli, aes(x = sampledate, y = result)) +
  geom_point(color = "royalblue3") +
  geom_point(data = e.coli, aes(x = sampledate, y = result), size = 2, color = "royalblue3") +
  scale_y_continuous(limits = c(0,5000)) +
  geom_hline(yintercept = 410, linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ggtitle("E.coli levels (MPN/100 mL) overtime")
  

E.coli_fig_2 #shorted range to see closer what is beyond recommended level


e.coli_above_rec_level<-benchmark_data %>%
  filter(analyte == "E. coli") %>%
  filter(result > 410) # of the total 465 samples 204 were above the recommended 410 (43.87%)


## Nitrogen ----
nitrogen<-benchmark_data %>%
  filter(analyte == "Nitrate as N, Dissolved")


nitrogen_fig <- ggplot(data = nitrogen, aes(x = sampledate, y = result)) +
  geom_point(color = "royalblue3") +
  geom_point(data = nitrogen, aes(x = sampledate, y = result), size = 2, color = "royalblue3") +
  geom_hline(yintercept = 4, linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  xlab("Sample date") +
  ylab("N (mg/L)") +
  ggtitle("Nitrogen levels (mg/L) over time")

nitrogen_fig

## Coliform ----

coliform<-benchmark_data %>%
  filter(analyte == "Coliform, Total")

coliform_fig <- ggplot(data = coliform, aes(x = sampledate, y = result)) +
  geom_point(color = "royalblue3") +
  geom_point(data = coliform, aes(x = sampledate, y = result), size = 2, color = "royalblue3") +
  geom_hline(yintercept = 10000, linetype = "dashed") +
  scale_y_continuous(limits = c(0,250000)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ggtitle("Coliform levels (MPN/100 mL) overtime")

coliform_fig

coliform_above_rec_level<-benchmark_data %>%
  filter(analyte == "Coliform, Total") %>%
  filter(result > 10000) # Of the total 459 samples 314 were over the recommended 
                         #limit of 10000 (68.4%)

## Enterococcus ----

enterococcus <-benchmark_data %>%
  filter(analyte == "Enterococcus") 

enterococcus_fig <- ggplot(data = enterococcus, aes(x = sampledate, y = result)) +
  geom_point(color = "royalblue3") +
  geom_point(data = enterococcus, aes(x = sampledate, y = result), size = 2, color = "royalblue3") +
  geom_hline(yintercept = 235, linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ggtitle("Enterococcus levels (MPN/100 mL) over time")
 
enterococcus_fig


enterococcus_above_rec_level<-enterococcus %>%
  filter(result > 235) # of the total 72 samples 35 were above the recommended 235 (48.61%)

mean(enterococcus$result) #mean is at level typical for agricultural runoff

## Temperature ----
  
temperature <-benchmark_data %>%
  filter(analyte == "Temperature") 

temperature_fig <- ggplot(data = temperature, aes(x = sampledate, y = result)) +
  geom_point(color = "royalblue3") +
  geom_point(data = temperature, aes(x = sampledate, y = result), size = 2, color = "royalblue3") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ggtitle("Temperature (C) over time")

temperature_fig

ggplotly(temperature_fig)

## Turbidity ----

turbidity <-benchmark_data %>%
  filter(analyte == "Turbidity, Total") 

turbidity_fig <- ggplot(data = turbidity, aes(x = sampledate, y = result)) +
  geom_point(color = "royalblue3") +
  geom_point(data = turbidity, aes(x = sampledate, y = result), size = 2, color = "royalblue3") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_hline(yintercept = 50, linetype = "dashed") +
  ggtitle("Turbidity (NTU) overtime") +
  facet_wrap(vars(stationname))

turbidity_fig

turbidity_above_rec_level<-benchmark_data %>%
  filter(analyte == "Turbidity, Total") %>%
  filter(result > 50) #less than 1% of the time (0.076%) the water had levels are above recommended turbitidy levels 

## pH ----

pH <-benchmark_data %>%
  filter(analyte == "pH") 

pH_fig <- ggplot(data = pH, aes(x = sampledate, y = result)) +
  geom_point(color = "royalblue3") +
  geom_point(data = pH, aes(x = sampledate, y = result), size = 2, color = "royalblue3") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  #geom_hline(yintercept = 50, linetype = "dashed") +
  ggtitle("pH overtime") +
  facet_wrap(vars(stationname))

pH_fig

## Conductivity ----

conductivity <-benchmark_data %>%
  filter(analyte == "SpecificConductivity, Total") 

conductivity_fig <- ggplot(data = conductivity, aes(x = sampledate, y = result)) +
  geom_point(color = "royalblue3") +
  geom_point(data = conductivity, aes(x = sampledate, y = result), size = 2, color = "royalblue3") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_hline(yintercept = 2000, linetype = "dashed") +
  ggtitle("Conductivity (uS/cm) overtime") +
  facet_wrap(vars(stationname))

conductivity_fig

conductivity_above_rec_level<-benchmark_data %>%
  filter(analyte == "SpecificConductivity, Total") %>%
  filter(result > 2000) # 424 of the 439 samples were over normal levels (92.54%)

## DO ----

DO <-benchmark_data %>%
  filter(analyte == "Oxygen, Dissolved, Total") 

DO_fig <- ggplot(data = DO, aes(x = sampledate, y = result)) +
  geom_point(color = "royalblue3") +
  geom_point(data = DO, aes(x = sampledate, y = result), size = 2, color = "royalblue3") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  #geom_hline(yintercept = 2000, linetype = "dashed") +
  ggtitle("Disolved Oxygen (mg/L) overtime") +
  facet_wrap(vars(stationname))

DO_fig


# example code for _writing to file as pdf
ggsave(filename = "figures/Beck_Fiorella/Phelps_Creek_temp_DRAFT.png",
       plot= fig_phelps_temp,

       bg = "white")

ggsave(filename = "figures/Beck_Fiorella/Phelps_Creek_DO_DRAFT.png", plot= fig_phelps_DO, bg = "white")





