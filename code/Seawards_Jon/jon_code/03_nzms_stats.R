# ============================================
# Project: NZ Mudsnail
# File: 03_nzms_stats.R
# Author: Jon Seawards
# Date: 2026-04-25
# Purpose: Statistics and Figures 
# ============================================

source("code/Seawards_Jon/jon_code/01_setup.R")

# Statistical Tests (non-parametric)
    
  #temperature,  p-value = 0.4053
wilcox.test(temperature_c ~ nzm_presence, data = water_quality_nzm)

  #conductivity, p-value = 0.8316
wilcox.test(new_conductivity_u_s_cm ~ nzm_presence, data = water_quality_nzm)

  #dissolved oxygen, p-value = 0.2147
wilcox.test(dissolved_oxygen_mg_l ~ nzm_presence, data = water_quality_nzm)

  #pH, p-value = 0.1168
wilcox.test(p_h ~ nzm_presence, data = water_quality_nzm)

#Figures
  #water quality

    #temperature 

temp_box <- ggplot(water_quality_nzm, aes(
    x = factor(nzm_presence, labels = c("Absent", "Present")),
    y = temperature_c)) +
  geom_boxplot( fill = "lightblue") +
  labs(
    x = "NZMS Occurrence",
    y = "Water Temperature (°C)",
    title = "Temperature Distribution by NZMS Presence") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

    #conductivity

cond_box_log_1 <- ggplot(water_quality_nzm, aes(
    x = factor(nzm_presence, labels = c("Absent", "Present")),
    y = log10(new_conductivity_u_s_cm + 1))) +
  geom_boxplot(fill = "lightblue",) +
  labs(
    x = "NZMS Occurrence",
    y = "Log10 Conductivity (µS/cm)",
    title = "Conductivity Distribution by NZMS Presence") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

    #dissolved oxygen

do_box <- ggplot(water_quality_nzm, aes(
  x = factor(nzm_presence, labels = c("Absent", "Present")),
  y = dissolved_oxygen_mg_l)) +
  geom_boxplot( fill = "lightblue") +
  labs(
    x = "NZMS Occurrence",
    y = "Dissolved Oxygen (mg/L)",
    title = "DO Distribution by NZMS Presence") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

    #pH

ph_box <- ggplot(water_quality_nzm, aes(
  x = factor(nzm_presence, labels = c("Absent", "Present")),
  y = p_h)) +
  geom_boxplot( fill = "lightblue") +
  labs(
    x = "NZMS Occurrence",
    y = "pH",
    title = "pH Distribution by NZMS Presence") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

  #population

nzm_timeline <- ggplot(data_nzm_wq, aes(
  x = date_on_vial,
  y = nz_mudsnail,
  color = site)) +
  geom_point(size = 3) +
  scale_color_manual(values = c(
    "NPB"  = "#1f78b4",  # darker blue
    "NPB1" = "#4ea3d8",  # medium blue
    "NPB2" = "#a6cee3",   # light blue
    "MO1" = "#dadaeb",   # purple
    "NDC" = "#fdb863",   # medium orange
    "NVB" = "#5a7d4d"),    # dark green
    na.value = "lightblue") +
  labs(
    x = "Date",
    y = "NZMS Abundance",
    title = "Timeline of NZMS Positive Detections") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))
  
  #FIXME: data wrangle should be piped into ggplot not a separate object in source01 
  # site positive sample frequency
nzm_site_freq <- ggplot(site_freq, 
                    aes(x = reorder(site, detection_rate), 
                        y = detection_rate)) +
  geom_col(fill = "steelblue") +
  labs(
    x = "Site",
    y = "NZMS Detection Rate",
    title = "NZMS Detection Frequency by Site") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

  # NZMS mean abundance by site
nzm_site_abundace <- ggplot(
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

#save to file
  #temp
ggsave(filename = "figures/Seawards_Jon/temp_box.pdf", temp_box, width = 6, height = 4, units = "in")

  #cond
ggsave(filename = "figures/Seawards_Jon/cond_box_log.pdf", cond_box_log_1, width = 6, height = 4, units = "in")

  #do
ggsave(filename = "figures/Seawards_Jon/do_box.pdf", do_box, width = 6, height = 4, units = "in")

  #ph
ggsave(filename = "figures/Seawards_Jon/ph_box.pdf", ph_box, width = 6, height = 4, units = "in")

  #timeline
ggsave(filename = "figures/Seawards_Jon/nzm_timeline.pdf", nzm_timeline, width = 6, height = 4, units = "in")

  # site frequency
ggsave(filename = "figures/Seawards_Jon/nzm_site_freq.pdf", nzm_site_freq, width = 6, height = 4, units = "in")

  #site abundance
ggsave(filename = "figures/Seawards_Jon/nzm_site_abundnace.pdf", nzm_site_abundace, width = 6, height = 4, units = "in")

