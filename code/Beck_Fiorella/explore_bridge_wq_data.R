library(tidyverse)
library(ggridges)

bridge_wq <- read_rds("data/bridge_water_quality/ncos_bridge_ysi_data_for_Fiorella.rds")


Venoco <- bridge_wq |> 
  filter(site_name == "Venoco Bridge") |> 
  filter(salinity_ppt < 80)

# try plotting histogram of salinity

Venoco_salinity_histogram <- ggplot(data = Venoco, aes(x = salinity_ppt)) +
  geom_histogram() +
  geom_vline(xintercept = 0.5) +
  geom_vline(xintercept = 30) +
  geom_vline(xintercept = 40)
  
Venoco_salinity_histogram

Venoco_prob_density <- ggplot(data = Venoco, aes(x = salinity_ppt)) +
  geom_density() +
  geom_vline(xintercept = 0.5) +
  geom_vline(xintercept = 30) +
  geom_vline(xintercept = 40)

Venoco_prob_density

#  empirical cumulative distribution function

# Using the built-in 'faithful' dataset
ggplot(Venoco, aes(x = salinity_ppt)) +
  stat_ecdf(geom = "step", colour = "#3a86d4", linewidth = 1) +
  labs(
    title = "Empirical Cumulative Density Plot",
    x = "Salinity (ppt)",
    y = "Cumulative Proportion F(x)"
  ) +
  theme_minimal() +
  geom_vline(xintercept = 0.5) +
  geom_vline(xintercept = 30) +
  geom_vline(xintercept = 40)


# ~~~~~~~~~~~~~~~~~~ ----


Phelps <- bridge_wq |> 
  filter(site_name == "Phelps Bridge") |> 
  filter(salinity_ppt < 80)

Phelps_salinity_histogram <- ggplot(data = Phelps, aes(x = salinity_ppt)) +
  geom_histogram() +
  geom_vline(xintercept = 0.5) +
  geom_vline(xintercept = 30) +
  geom_vline(xintercept = 40)

Phelps_salinity_histogram

Phelps_prob_density <- ggplot(data = Phelps, aes(x = salinity_ppt)) +
  geom_density() +
  geom_vline(xintercept = 0.5) +
  geom_vline(xintercept = 30) +
  geom_vline(xintercept = 40)

Phelps_prob_density

#  empirical cumulative distribution function

# Using the built-in 'faithful' dataset
ggplot(Phelps, aes(x = salinity_ppt)) +
  stat_ecdf(geom = "step", colour = "#3a86d4", linewidth = 1) +
  labs(
    title = "Empirical Cumulative Density Plot",
    x = "Salinity (ppt)",
    y = "Cumulative Proportion F(x)"
  ) +
  theme_minimal() +
  geom_vline(xintercept = 0.5) +
  geom_vline(xintercept = 30) +
  geom_vline(xintercept = 40)

# try plotting curves for all three sites together:
bridge_wq_filtered <- bridge_wq |> 
  filter(salinity_ppt <80) |> 
  filter(!is.na(site_name))

prob_density <- ggplot(data = bridge_wq_filtered, aes(x = salinity_ppt, color = site_name, fill = site_name)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 0.5) +
  geom_vline(xintercept = 30) +
  geom_vline(xintercept = 40)

prob_density

# try geom_ridges
prob_density_ridges <- ggplot(data = bridge_wq_filtered, aes(x = salinity_ppt, y = site_name, color = site_name, fill = site_name)) +
  geom_density_ridges(scale = 5, alpha = 0.5) +
  geom_vline(xintercept = 0.5) +
  geom_vline(xintercept = 30) +
  geom_vline(xintercept = 40) +
  theme_ridges()

prob_density_ridges




