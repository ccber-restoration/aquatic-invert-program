
# load packages ----
library(tidyverse)
library(readxl) #read excel files
#library(googlesheets4) # read in data from Google Sheets
library(janitor) # data cleaning

#read in data ----
# read in data from google sheets
#invert_data_drive <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1rcYilbrxduQswiJpCK6TopZaIWFJxpK8nklt3INtFVM/edit?gid=0#gid=0")


#read in static excel file (will become outdated)

# see list of sheets
excel_sheets("data/Aquatic_Sampling_Data_2025-09-11_clean.xlsx")

invert_data <- read_excel(path = "data/Aquatic_Sampling_Data_2025-09-11_clean.xlsx",
                          sheet = "Aquatic Insects"
                          ) %>% 
  #clean names
  clean_names() #%>%
  #create properly formatted date column
  #mutate(date = as.Date(date_on_vial, origin = "1900-01-01"))

#need to make sure dates get read in correctly

# explore site codes...                                                        
#note- there is content in two columns to the right of the Comments column
unique(invert_data$site) 

#23 site codes, plus NAs

#Note that information on partial samples was added to new column (half or third samples)

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


