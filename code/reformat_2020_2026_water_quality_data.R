
# run setup script
source("code/00_setup.R")

library(writexl)


# columns in new Excel: 

# Date	Start Time	End Time	Site	Observers	Depth value (cm)	Depth reference (bottom or surface)	Comments	pH	Dissolved Oxygen (mg/L)	Dissolved Oxygen (%)	Conductivity-specific (mS/cm)	Conductivity-specific (uS/cm)	Conductivity-specific (ppt)	Salinity (mS/cm)	Salinity (ppt)	Temperature (C)	Barometric Pressure (mmHg)

unique(water_quality$wq_measurement_depth_cm)

# make columns match the new Excel
water_quality_reformatted <- water_quality |> 
  select(-c(year, month)) |> 
  mutate(depth_value_cm = case_when(
      wq_measurement_depth_cm %in% c("10cm below surface", "10 cm below surface", "10cm from surface") ~ 10,
      wq_measurement_depth_cm %in% c("5cm above bottom", "5cm below surface", "5 cm above bottom", "5cm from bottom") ~ 5,
      wq_measurement_depth_cm == "surface" ~ 0,
      .default = NA
  )) |> 
  mutate(depth_reference = case_when(
    wq_measurement_depth_cm %in% c("10cm below surface", "5cm below surface", "10 cm below surface", "surface", "10cm from surface") ~ "surface",
    wq_measurement_depth_cm %in% c("5cm above bottom", "5 cm above bottom", "5cm from bottom" ) ~ "bottom",
    .default = NA
  )) |> 
  relocate(depth_value_cm:depth_reference, .after = observers) |> 
  relocate(comments, .after = depth_reference) |> 
  # remove columns not in the new sheet
  select(-wq_measurement_depth_cm) |> 
  select(-new_conductivity_u_s_cm) 
  
