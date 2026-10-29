library(tidyverse)

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
