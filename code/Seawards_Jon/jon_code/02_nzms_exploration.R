# ============================================
# Project: NZ Mudsnail
# File: nzms_exploration.R
# Author: Jon Seawards
# Date: 2026-01-20
# Purpose: Explore and analyze NZ mudsnail data
# ============================================

library(plotly)
library(cowplot)

source("code/Seawards_Jon/jon_code/01_setup.R")

# Basic Stats ----

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
# 1 NPB2, NPB1, NPB 

#What was the first obervation of NZ mudsnail in NCOS?
invert_data_nzm_separate[which.min(invert_data_nzm_separate$date_on_vial), ]
# date: 2022-08-08 , site: NPB2, Type: FB250 , Count: 43

#TODO consider filtering both water quality and invert data by date to only consider records after
#NZMS was first detected in eDNA


# When NZ Mudsnail invaded, what was their median organism density within a given sample?  (median number of organisms detected in a single positive sample)
invert_data_nzm_separate %>%
  summarize(median_density = median(nz_mudsnail, na.rm = TRUE)) %>%
  pull(median_density)
# 29.5 

# Plots ----
# What is the distribution of NZ Mudsnail abundance across all positive samples taken at NCOS?

hist_nzm <- ggplot(data = invert_data_nzm_separate, aes(x = nz_mudsnail)) +
  geom_histogram(binwidth = 50, color = "beige", fill = "steelblue", boundary = 0) + 
                   labs(
                     title = "New Zealand Mudsnail Distribution Per Sample",
                     x = "NZ Mudsnail Count",
                     y = "Number of Samples") 

# data is very right skewed, normal for invasive species data but will log transform to attempt to improve normality
hist_nzm

# FIXME, use boundary argument and bin width

hist_log_nzm <- ggplot(data = invert_data_nzm_separate, aes(x = log10(nz_mudsnail))) +
  geom_histogram(binwidth = 0.35, color = "beige", fill = "steelblue") + 
  labs(
    title = "New Zealand Mudsnail Distribution Per Sample (Log Transformed)",
    x = "NZ Mudsnail Count Log Scale",
    y = "Number of Samples")

hist_log_nzm

#skew is still present but less pronounced

#Dotplot visual of same trend
dot_nzm <- ggplot(invert_data_nzm_separate,
             aes(x = log10(nz_mudsnail))) +
  geom_dotplot(binwidth = 0.15, dotsize = 1, stackratio = 1, fill = "steelblue") +
  labs(
    title = "NZ Mudsnail Counts per Sample",
    x = "NZ Mudsnail count (log scale)"
    #y = "Number of samples"
    )

dot_nzm

# What is the median NZ mudsnail density at each site with invasion presence? 
temp_nzm  <- invert_data_nzm_separate %>%
  group_by(site) %>%
  summarize(median_density = median(nz_mudsnail),
            n_samples = n())

# sort by median density, descending. 
invert_data_nzm_separate %>%
  group_by(site) %>%
  summarize(median_density = median(nz_mudsnail)) %>%
  
#plot of median NZ Mudsnail density by site
  arrange(-median_density) %>% 
  #overwrite site code, sorting by median density rather than alphabetical
  mutate(site = factor(site, levels = site)) %>% 
 
  #ggplot 
 ggplot(aes(x = site, y = median_density)) +
  geom_col(color = "beige", fill = "steelblue") + 
  labs(
    title = "New Zealand Mudsnail Site Density",
    x = "Site",
    y = "Median Density") 


# Abundance vs Variables ----
#temp

temp_box <- ggplot(water_quality_nzm, aes(x = factor(nzm_presence, 
            labels = c("Absent", "Present")), y = temperature_c)) +
  geom_boxplot() +
  labs(x = "NZMS Occurrence", y = "Water Temperature (°C)",
    title = "Temperature Distribution by NZMS Presence") +
  theme_minimal()

temp_bin_curve <- ggplot(water_quality_nzm, aes(x = temperature_c, y = nzm_presence)) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.6) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  labs(
    x = "Water Temperature (°C)",
    y = "Probability of NZMS Presence",
    title = "NZMS Occurrence Across Water Temperature"
  ) +
  theme_minimal()

temp_bin_1 <- ggplot(water_quality_nzm, aes(x = temperature_c, y = nzm_presence)) +
  geom_jitter(height = 0.05)

temp_bin_2 <- ggplot(water_quality_nzm, aes(x = temperature_c, y = nzm_presence)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = TRUE)

`temp_nzm <- ggplot(data = data_nzm_wq, aes(x = temperature_c, y = nz_mudsnail, color = site)) +
  geom_point(size = 3) +
  #geom_point(color = "steelblue", fill = "steelblue") + 
  labs(
    title = "NZ Mudsnail Abundance vs Temperature",
    x = "Temperature (C)",
    y = "Count") + theme_cowplot()`

temperature_abunance <- ggplot(water_quality_nzm, aes(x = temperature_c, y = log10(nz_mudsnail + 1))) +
  geom_point(size = 2) +
  labs(
    x = "Water Temperature °C",
    y = "Log10 (Count + 1)",
    title = "Water Temperature vs. NZMS Abundance"
  ) +
  theme_minimal()

#view
ggplotly(temperature_abunance)
temp_nzm
temperature_abunance
#save to file
ggsave(filename = "figures/Seawards_Jon/temp_nzm.pdf", temp_nzm, width = 6, height = 4, units = "in")


#check ranges of water quality variables...
range(data_nzm_wq$temperature_c, na.rm = TRUE)
#  9.2 24.9
# 9.2, quite cold compared to other samples, was @ site NPB 

#salinity
sal_nzm <- ggplot(data = data_nzm_wq, aes(x = salinity_ppt, y = nz_mudsnail, color = site)) +
  geom_point() +
  #geom_point(color = "steelblue", fill = "steelblue") + 
  labs(
    title = "NZ Mudsnail Abundance vs Salinity",
    x = "Salinity (ppt)",
    y = "NZ Mudsnail")

ggplotly(sal_nzm)

#check ranges of water quality variables...
range(data_nzm_wq$salinity_ppt, na.rm = TRUE)
# 1.4 39.2 WIDE RANGE, most obs between 0 and 5
#ggplotly(sal_nzm)
# two outliers were from MO1 (mouth) and NVB (vennoco bridge), both abundance and freqnency of 1

#pH
ph_nzm <- ggplot(data = data_nzm_wq, aes(x = p_h, y = nz_mudsnail, color = site)) +
  geom_point() +
  #geom_point(color = "steelblue", fill = "steelblue") + 
  labs(
    title = "NZ Mudsnail Abundance vs pH",
    x = "pH",
    y = "NZ Mudsnail")

ggplotly(ph_nzm)
#check ranges of water quality variables...
range(data_nzm_wq$p_h, na.rm = TRUE)
# 7.08 9.56
# two high obs @ 9.1 and 9.5 are the same obs as salinity outliers at MO1 and NVB
# most are between 7.4 and 8.1


# Exploratory Questions: (using: invert_data_nzm) ----

# What year had the largest proportion of NZ Mudsnail invasion? (highest instances of invaded samples >1/total samples)



nzm_timeline <- ggplot(data_nzm_wq, aes(
    x = date_on_vial,
    y = nz_mudsnail,
    color = site)) +
  geom_point(size = 3) +
  scale_color_manual(values = c(
    "NPB" = "orange",
    "NPB1" = "orange",
    "NPB2" = "orange",
    "MO1" = "lightblue",
    "NDC" = "lightblue",
    "NVB" = "lightblue"),
    na.value = "lightblue") +
  labs(
    x = "Date",
    y = "NZMS Abundance",
    title = "Timeline of NZMS Positive Detections") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

# What is the probability you will find NZ Mudsnail in a given sample?

site_freq <- water_quality_nzm %>%
  group_by(site) %>%
  summarise(
    detection_rate = mean(nz_mudsnail > 0, na.rm = TRUE),
    n_samples = n(),
    .groups = "drop"
  ) %>%
  filter(detection_rate > 0)

freq_plot <- ggplot(site_freq, 
                    aes(x = reorder(site, detection_rate), 
                        y = detection_rate)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    x = "Site",
    y = "NZMS Detection Rate",
    title = "NZMS Detection Frequency by Site"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

freq_plot_2 <- ggplot(site_freq, 
                    aes(x = reorder(site, detection_rate), 
                        y = detection_rate)) +
  geom_col(fill = "steelblue") +
  labs(
    x = "Site",
    y = "NZMS Detection Rate",
    title = "NZMS Detection Frequency by Site"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

nzm_abundance_site <- ggplot(
  water_quality_nzm %>%
    group_by(site) %>%
    summarise(mean_abundance = mean(nz_mudsnail, na.rm = TRUE),
      .groups = "drop") %>%
    filter(mean_abundance > 0),
  
  aes(x = reorder(site, mean_abundance), 
      y = mean_abundance)) +
  geom_col(fill = "steelblue") +
  labs(
    x = "Site",
    y = "Mean NZMS Abundance",
    title = "NZMS Abundance by Site") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# Are there variables that skew this probability with statistical relevance? (p< 0.05)
#eg. sp conductivity, temperature, Chl-a, pH, spatial location in estuary/hotspots, other organism correlations 
# ^^ Use new integrated data set to answer this with statistical test __. 


# Discussion Questions:

# What do the above answers tell us about the invasion patterns of NZ Mudsnail? 

# What theories can we support with the above evidence? 

# How can we use this information help us continue to monitor NZ Mdusnail presence in NCOS?

# What is the ecological risk assessment of this invaison?

# Areas for continued exploration? Unanswered questions and partially supported theories..

#A few more tips:

#if you put date on the x-axis, use the scale_x_date() argument rather than scale_x_continuous

#https://ggplot2.tidyverse.org/reference/scale_date.html