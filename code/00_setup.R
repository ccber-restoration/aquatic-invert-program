
# 0.  load packages ----
library(tidyverse) # general use
library(readxl) # read excel files
library(janitor) # data cleaning
library(hms) # for working with times (time of day)
library(cowplot)
library(plotly) #interactive graphs

#library(googlesheets4) # read in data from Google Sheets

# 1. read in data ----


#Currently not used:
# read in data directly from google sheets
#invert_data_drive <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1rcYilbrxduQswiJpCK6TopZaIWFJxpK8nklt3INtFVM/edit?gid=0#gid=0")

#read in static excel file (will become outdated)

#set to current version of Excel file
data_path <- "data/Aquatic Sampling Data-2026-02-03.xlsx"

# see list of sheets
excel_sheets(data_path)

## 1.1 invertebrate data ----
invert_data <- read_excel(path = data_path,
                          sheet = "Aquatic Insects"
                          ) %>% 
  #clean names
  clean_names() %>% 
  #remove columns, including columns that are (incomplete) summaries rather than raw data
  select(-c(diptera_total, hemiptera_total, annelida_total, coleoptera_total, total_number)) %>% 
  mutate(year = year(date_on_vial),
         month = month(date_on_vial)) %>% 
  relocate(year:month, .after = date_on_vial)
 
# explore site codes...                                                        
unique_sites <- unique(invert_data$site) 

unique_sites

#23 site codes, plus NAs

#Note we moved information on partial samples (half or third samples) that was previously within in the site name column 
#to new column ("partial_sample_note")

#explore sample types 
unique(invert_data$sample_type)
#11 sample types plus NA
#consider consolidating sample sample types by broader category (FB vs. CORE/Core, SW)
# FB = filtered beaker (planktonic)
# CORE = benthic
#SW = sweep?
# SAV?

#list of people sorting samples
unique(invert_data$person_that_sorted_the_sample)

#names not standardized, so duplicates
#also includes combinations of names (multiple students sorting)

# TODO- clean taxon column names
#create separate data frame with:
#taxon names (as in Google Sheet)
#taxon name (post janitor)
#taxon name for display
#More taxonomic information (e.g. higher-level groupings)

#read in overall list of taxa
taxa <- read_csv(file = "data/taxon_list.csv")

## 1.2 water quality data ----
water_quality <- read_excel(path = data_path,
                            sheet = "Water Quality",
                            na = c("", "n/a", "N/A")) %>%
  #make column names more user-friendly
  clean_names() %>%
  #change m_s_cm conductivity column to numeric
  mutate(conductivity_specific_m_s_cm = as.numeric(conductivity_specific_m_s_cm)) %>% 
  #times are being read as date-times, change to time in hms format (24 hr time)
  mutate(year = year(date),
         month = month(date),
         start_time = as_hms(start_time),
         end_time = as_hms(end_time),
         new_conductivity_u_s_cm = case_when(
           #if micro column is blank, use milli
           is.na(conductivity_specific_u_s_cm) ~ conductivity_specific_m_s_cm*1000,
           .default = conductivity_specific_u_s_cm
         )
  ) %>% 
  #move new column
  relocate(new_conductivity_u_s_cm, .before = conductivity_specific_ppt) %>% 
  relocate(year:month, .after = date)
         
  

#list site codes:
unique_sites_wq <- unique(water_quality$site)

unique_sites_wq
#known issues:

#FIXME: 

# site names not fully standardized
# measurement depth not standardized
# Some DO values are incredibly high...


