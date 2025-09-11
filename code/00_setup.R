
# load packages ----
library(tidyverse)

#read excel files
library(readxl)

# read in data from Google Drive
library(googlesheets4)

library(janitor)

#read in static excel file (will become outdated)

# see list of sheets
excel_sheets("data/Aquatic_Sampling_Data_2025-09-11.xlsx")

invert_data <- read_excel(path = "data/Aquatic_Sampling_Data_2025-09-11.xlsx",
                          sheet = "Aquatic Insects"
                          ) %>% 
  #clean names
  clean_names() %>%
  #create properly formatted date column
  mutate(date = as.Date(date_on_vial, origin = "1900-01-01"))

#need to make sure dates get read in correctly


# explore site codes...                                                        
#note- there is content in two columns to the right of the Comments column
unique(invert_data$Site) 

#29 site codes, plus NAs
#some codes indicate that a row is a partial sample (e.g. NBP 1/3, NBP 2/3, NBP 3/3)
#these should be aggretated back to the sample level

#explore sample types 
unique(invert_data$`Sample Type`)
#11 sample types plus NA
#consider consolidating sample sample types by broader category (FB vs. CORE/Core, SW)
# FB = filtered beaker (planktonic)
# CORE = benthic
#SW = sweep?
# SAV?



# read in data from google sheets
#invert_data_drive <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1rcYilbrxduQswiJpCK6TopZaIWFJxpK8nklt3INtFVM/edit?gid=0#gid=0")




