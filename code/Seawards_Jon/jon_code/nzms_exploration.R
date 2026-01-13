source("code/00_setup.R")

nzms <- invert_data %>% 
  filter(!(is.na(nz_mudsnail)))

#FIXME note that one count value has a non-numeric character causing problems

#To summarize:

#How many samples?
#When?
#Where?
#What sampling methods?
#Abundance patterns?