# the purpose of this script is to begin exploring, visualizing, and summarizing water quality measurements

# run 00_setup.R to obtain the water_quality dataframe

# Note that the input dataframe will change slightly after migrating to the "new" spreadsheet

library(naniar)
library(dplyr)
library(ggridges)
library(gt)
library(tidyr)
library(ggplot2)

 # general use


version

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

#filter out sites with less than 10 data points 
water_quality10 <- filtered_water_quality %>%
  filter(!site %in% c("DSP","ETNP", "ETSP","VP1"))


# Visualizing data: ----

## All sites: ----

water_quality10 <- water_quality10 %>%
  mutate(salinity_ppt = as.numeric(salinity_ppt)) #force salinity to be numberic



### Salinity ----

# visualize salinity for all sites
Salinity <- ggplot(data = water_quality10, aes(x = year, y = salinity_ppt, group = site, color = site)) +
  geom_point() +
  geom_line()+
  xlab("year") +
  ylab("Salinity (ppt)") +
  theme_cowplot() +
  facet_wrap(vars(site)) 
 # ggtitle("Salinity by Site") 
                     

Salinity

# visualize salinity without lines
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

#visualize pH for all sites 
pH <- ggplot(data = water_quality10, aes(x = date, y = p_h, group = site, color = site)) +
  geom_point() +
  geom_line()+
  xlab("year") +
  ylab("pH") +
  theme_cowplot() +
  facet_wrap(vars(site))
# ggtitle("Salinity by Site") 


pH #largely all similar

### DO ----

# visualize DO for all sites 
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

#visualize temperature for all sites 
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

# force barometic pressure to be numeric 
water_quality10 <- water_quality10 %>%
  mutate(barometric_pressure_mm_hg = as.numeric(barometric_pressure_mm_hg))

# visualize pressure for all sites
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

 
# Deep dive: ----
## Salinity ----
  
## Density Plot ----

#visualize density salinity for all sites 
wq_density_10 <- ggplot(data = water_quality10, aes(x = salinity_ppt, y = site, group = site, color = site)) +
  geom_density_ridges(scale = 5, alpha = 0.5) +
  geom_vline(xintercept = 0.5) + # fresh water
  geom_vline(xintercept = 5) + # oligohaline 
  geom_vline(xintercept = 18) + # mesohaline
  geom_vline(xintercept = 30) + # polyhaline
  geom_vline(xintercept = 40) +# seawater
  theme_ridges() 
  #coord_cartesian(xlim = c(0, 50)) 
  

wq_density_10

##Table: ----


breaks <- c(-Inf, 0.5, 5, 18, 30, 40, Inf) #create ranges for breaks in table
labels <- c("Fresh", "Oligohaline", "Mesohaline", "Polyhaline", "Euhaline", "Hypersaline") #label breaks on tabel
#Ranges: "0-0.5 (Fresh)", "0.6-5 (oligohaline)", "6-18 (mesohaline)", "19-30 (Polyhaline)", "31-40 (Euhaline)", ">40 (Hypersaline)

Salinity_ranges_tb <- water_quality10 %>% #create df form salinity table
  mutate(range = factor(cut(salinity_ppt, breaks = breaks, labels = labels, include.lowest = TRUE, right = TRUE ),
    levels = labels)) %>%
  filter(!is.na(salinity_ppt)) 
  
Salt_table_result <- Salinity_ranges_tb %>% 
  group_by(site, range) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = range, values_from = count, values_fill = 0) %>%
  select(site, all_of(labels)) 

range_levels <- levels(Salinity_ranges_tb$range)
Salt_table_result<- Salt_table_result %>%
  select(site, all_of(range_levels))

Salt_table_result

#in percent
Salt_table_result_row <- Salt_table_result
num_cols <- sapply(Salt_table_result_row, is.numeric)
Salt_table_result_row[num_cols] <- round(prop.table(as.matrix(Salt_table_result_row[num_cols]), margin = 1) * 100, 2)

print("Percent table (row-wise):")
print(Salt_table_result_row)

## GT table ----


custom_order <- c("PIER","MO1", "NVBR", "NEC","CUL1", "NMC", "NPB", "NPB1", "NWP", "NPB2","NDC", "NVP2", "NVP4")

Salt_gt <- Salt_table_result %>%
  mutate(site = factor(site, levels = custom_order)) %>%
  arrange(site)
  

gt(Salt_gt) %>%
  tab_header(
    title = "Salinity Ranges",
    subtitle = "Data within EPA ranges") %>% #create a gt table
  cols_align(
    align = "center",  
    columns = everything()) %>%
  tab_style(
    style = list(
      cell_fill(color = "#8CB7DF"),   # Background color
      cell_text(color = "black") ), # Text color
    locations = cells_body(
      rows = site %in% c("NPB2", "NPB1", "NPB", "NWP"))) %>%
  tab_style(
    style = list(
      cell_fill(color = "#5992C6"),   # Background color
      cell_text(color = "black") ), # Text color
    locations = cells_body(
      rows = site %in% c("MO1", "NEC", "NVBR", "NMC", "PIER","CUL1"))) %>%
  tab_style(
    style = list(
      cell_fill(color = "#C7DBEF"),   # Background color
      cell_text(color = "black") ), # Text color
    locations = cells_body(
      rows = site %in% c("NDC", "NVP2", "NVP4")))

## GT % ----
#in percent

custom_order <- c("PIER","MO1", "NVBR", "NEC","CUL1", "NMC", "NPB", "NPB1", "NWP", "NPB2","NDC", "NVP2", "NVP4")

Salt_percent_gt <- Salt_table_result_row %>%
  mutate(site = factor(site, levels = custom_order)) %>%
  arrange(site)


gt(Salt_percent_gt) %>%
  tab_header(
    title = "Salinity Ranges",
    subtitle = "Data within EPA ranges") %>% #create a gt table
  cols_align(
    align = "center",  
    columns = everything()) %>%
  tab_style(
    style = list(
      cell_fill(color = "#8CB7DF"),   # Background color
      cell_text(color = "black") ), # Text color
    locations = cells_body(
      rows = site %in% c("NPB2", "NPB1", "NPB", "NWP"))) %>%
  tab_style(
    style = list(
      cell_fill(color = "#5992C6"),   # Background color
      cell_text(color = "black") ), # Text color
    locations = cells_body(
      rows = site %in% c("MO1", "NEC", "NVBR", "NMC", "PIER","CUL1"))) %>%
  tab_style(
    style = list(
      cell_fill(color = "#C7DBEF"),   # Background color
      cell_text(color = "black") ), # Text color
    locations = cells_body(
      rows = site %in% c("NDC", "NVP2", "NVP4")))


##Oligohaline sites: ---- 

### Density ----

oligohaline_sites<- water_quality10 %>% #create a df of oligohaline sites
  filter(site %in% c("NPB2", "NPB1", "NPB", "NWP"))


oligo_density_plot <- ggplot(data = oligohaline_sites, aes(x = salinity_ppt, y = site, group = site, color = site)) +
  geom_density_ridges(scale = 5, alpha = 0.5) +
  geom_vline(xintercept = 0.5) + # fresh water
  geom_vline(xintercept = 5) + # oligohaline 
  geom_vline(xintercept = 18) + # mesohaline
  #geom_vline(xintercept = 30) + # polyhaline
  #geom_vline(xintercept = 40) +# seawater
  theme_ridges() 
  # coord_cartesian(xlim = c(0, 25)) #zoom in

oligo_density_plot


### Linear ----

oligo_line_plot <- ggplot(data = oligohaline_sites, aes(x = date, y = salinity_ppt, group = site, color = site)) +
  geom_point() +
  xlab("year") +
  ylab("pH") +
  theme_cowplot() +
  facet_wrap(vars(site))+
  ggtitle("Salinity by Site") +
  ylim(0, 25)


oligo_line_plot

##Variable sites: -----

variable_sites<- water_quality10 %>% #create df of sites that were variable in salinity ranges 
  filter(site %in% c("MO1", "NEC", "NVBR", "NMC", "PIER","CUL1"))

variable_density_plot <- ggplot(data = variable_sites, aes(x = salinity_ppt, y = site, group = site, color = site)) +
  geom_density_ridges(scale = 5, alpha = 0.5) +
  geom_vline(xintercept = 0.5) + # fresh water
  geom_vline(xintercept = 5) + # oligohaline 
  geom_vline(xintercept = 18) + # mesohaline
  geom_vline(xintercept = 30) + # polyhaline
  geom_vline(xintercept = 40) +# seawater
  theme_ridges() +
  coord_cartesian(xlim = c(0, 100)) # zoom in (excludes NBVR highest value 174 ppt)

variable_density_plot


#### Fresh to Oligohaline sites: ----

Fresh_Oligo_sites<- water_quality10 %>% #create df of sites salinity that range from fresh to oligohaline
  filter(site %in% c("NDC", "NVP2", "NVP4"))

Fresh_Oligo_plot <- ggplot(data = Fresh_Oligo_sites, aes(x = salinity_ppt, y = site, group = site, color = site)) +
  geom_density_ridges(scale = 5, alpha = 0.5) +
  geom_vline(xintercept = 0.5) + # fresh water
  geom_vline(xintercept = 5) + # oligohaline 
  #geom_vline(xintercept = 18) + # mesohaline
  #geom_vline(xintercept = 30) + # polyhaline
  #geom_vline(xintercept = 40) +# seawater
  theme_ridges()
  

Fresh_Oligo_plot

#DO ----

##Table ----

num_do_wq<-water_quality10 %>%
  filter(dissolved_oxygen_mg_l != "over") #Filter out over 

num_do_wq <- num_do_wq %>%
  mutate(dissolved_oxygen_mg_l = as.numeric(dissolved_oxygen_mg_l)) # make DO numeric 

breaks_DO <- c(0,.5,3,4.99,Inf) #create ranges for breaks 
labels_DO <- c("Anoxic","Hypoxic","Intermediate","Good") #create labels for tabel breaks 


DO_tb <- num_do_wq %>%
  mutate( dissolved_oxygen_mg_l = round(dissolved_oxygen_mg_l, 2),range = factor(cut(dissolved_oxygen_mg_l, breaks = breaks_DO, labels = labels_DO),
                        levels = labels_DO)) %>%
  filter(!is.na(dissolved_oxygen_mg_l))

DO_table_result <- DO_tb %>%
  group_by(site, range) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = range, values_from = count, values_fill = 0) %>%
  select(site, all_of(labels_DO)) 

range_levels <- levels(DO_tb$range)
DO_table_result<- DO_table_result %>%
  select(site, all_of(range_levels))

DO_table_result


## GT table: ----

custom_order2 <- c("NVBR","MO1", "NWP", "PIER","NVP4","NDC","NMC","NPB","NEC","NPB1","CUL1", "NPB2","NVP2")

DO_gt <- DO_table_result %>%
  mutate(site = factor(site, levels = custom_order2)) %>%
  arrange(site)

num_do_wq %>%
  count(site)

DO_gt %>%
  gt() %>%
  tab_header(
    title = "Dissolved Oxygen Ranges",
    subtitle = "Data within EPA ranges") %>%
  cols_align(
    align = "center",  
    columns = everything()) %>%
  tab_style(
  style = list(
    cell_fill(color = "#5992C6"),   # Background color
    cell_text(color = "black") ), # Text color
  locations = cells_body(
    rows = site %in% c("NDC", "NWP", "NVP4", "MO1", "NVBR", "PIER", "NMC"))) %>%
  tab_style(
    style = list(
      cell_fill(color = "#C7DBEF"),   # Background color
      cell_text(color = "black") ), # Text color
    locations = cells_body(
      rows = site %in% c("NEC", "NPB", "NPB1")))


# NBP2 and NVP2 both have smaller sample sizes, filter out?
#%>%
  #fmt_number(columns = avg_value, decimals = 1)

##DO Plots: ----

density_DO <- ggplot(data = num_do_wq, aes(x = dissolved_oxygen_mg_l, y = site, group = site, color = site)) +
  geom_density_ridges(scale = 5, alpha = 0.5) +
  theme_ridges() +
  coord_cartesian(xlim = c(0, 27)) + #zoom in
  geom_vline(xintercept = 0.5) + # Anoxic 
  geom_vline(xintercept = 3) + # Hypoxic  
  geom_vline(xintercept = 4) + # Intermediate
  geom_vline(xintercept = 4) + # Good
  theme_ridges() 



density_DO

##NPB sites ----

DO_NPBs<-num_do_wq %>%
  filter(site %in% c("NPB2", "NPB1", "NPB"))
  

density_NBPs <- ggplot(data = DO_NPBs, aes(x = dissolved_oxygen_mg_l, y = site, group = site, color = site)) +
  geom_density_ridges(scale = 5, alpha = 0.5) +
  theme_ridges() +
  coord_cartesian(xlim = c(0, 27)) + #zoom in
  geom_vline(xintercept = 0.5) + # Anoxic 
  geom_vline(xintercept = 3) + # Hypoxic  
  geom_vline(xintercept = 4) + # Intermediate
  geom_vline(xintercept = 4) + # Good
  theme_ridges() 

density_NBPs

##Healthy sites ----


DO_good<-num_do_wq %>%
  filter(site %in% c( "NVP4", "NWP"))


density_good <- ggplot(data = DO_good, aes(x = dissolved_oxygen_mg_l, y = site, group = site, color = site)) +
  geom_density_ridges(scale = 5, alpha = 0.5) +
  theme_ridges() +
  coord_cartesian(xlim = c(0, 27)) + #zoom in
  geom_vline(xintercept = 0.5) + # Anoxic 
  geom_vline(xintercept = 3) + # Hypoxic  
  geom_vline(xintercept = 4) + # Intermediate
  geom_vline(xintercept = 4) + # Good
  theme_ridges() 

density_good

# PH Plots: ----

density_pH <- ggplot(data = water_quality10, aes(x = p_h, y = site, group = site, color = site)) +
  geom_density_ridges(scale = 5, alpha = 0.5) +
  theme_ridges() + 
  coord_cartesian(xlim = c(4, 10)) #zoom in

## Denisty plot ----


density_ph <- ggplot(data = water_quality10, aes(x = p_h, y = site, group = site, color = site)) +
  geom_density_ridges(scale = 5, alpha = 0.5) +
  theme_ridges() +
  coord_cartesian(xlim = c(0, 27)) + #zoom in
  geom_vline(xintercept = 0) + # Acidic 
  geom_vline(xintercept = 7) + # Neutral  
  geom_vline(xintercept = 11) + # Alakine
  theme_ridges() 

density_ph


## pH Table ----

breaks_ph <- c(-Inf,6.5, 7,7.1,8.5,Inf) # -inf to 7 acidic, 7 to 7.99 nuetral, 7.99 to inf is alkaline 
labels_ph <- c("Below prefered","Acidic","Neutral","Alakine", "Above prefered") #create labels for tabel breaks 


ph_tb <- water_quality10 %>%
    mutate(p_h = round(p_h, 2),range = factor(cut(p_h, breaks = breaks_ph, labels = labels_ph),levels = labels_ph)) %>%
  filter(!is.na(p_h))

ph_table_result <- ph_tb %>%
  group_by(site, range) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = range, values_from = count, values_fill = 0) %>%
  select(site, all_of(labels_ph)) 

range_levels_ph <- levels(ph_tb$range)
ph_table_result<- ph_table_result %>%
  select(site, all_of(range_levels_ph))

ph_table_result

## GT Table ----

custom_order3 <- c("NVBR","MO1","PIER","NEC","NWP","NMC","NVP4","CUL1","NDC","NVP2","NBP1","NPB","NPB1","NPB2")
ph_gt <- ph_table_result %>%
  mutate(site = factor(site, levels = custom_order3)) %>%
  arrange(site)

ph_gt %>%
  gt() %>%
  tab_header(
    title = "pH Ranges",
    subtitle = "Data within EPA ranges") %>%
  cols_align(
    align = "center",  
    columns = everything()) %>%
  tab_style(
    style = list(
      cell_fill(color = "#5992C6"),   # Background color
      cell_text(color = "black") ), # Text color
    locations = cells_body(
      rows = site %in% c("NVBR", "MO1", "PIER","NEC","NWP", "NMC","NVP4","CUL1", "NDC", "NVP2"))) %>%
  tab_style(
    style = list(
      cell_fill(color = "#C7DBEF"),   # Background color
      cell_text(color = "black") ), # Text color
    locations = cells_body(
      rows = site %in% c("NPB2","NPB", "NPB1")))
  
  
#tab_style( style = list( cell_fill(color = "#D9654B"), cell_text(color = "white") ),locations = cells_body(rows = site == "CUL1")) # Condition to match specific row

# Temperature ----

## Denisty plot ----
density_temp <- ggplot(data = water_quality10, aes(x = temperature_c, y = site, group = site, color = site)) +
  geom_density_ridges(scale = 5, alpha = 0.5) +
  theme_ridges()  

density_temp 


#Pressure----

## Denisty plot ----

density_pressure <- ggplot(data = water_quality10, aes(x =  barometric_pressure_mm_hg, y = site, group = site, color = site)) +
  geom_density_ridges(scale = 5, alpha = 0.5) +
  theme_ridges()  

density_pressure 



# Other: Upstream plots: ----


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








  