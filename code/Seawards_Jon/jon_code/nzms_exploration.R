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

#A few more tips:

#if you put date on the x-axis, use the scale_x_date() argument rather than scale_x_continuous

#https://ggplot2.tidyverse.org/reference/scale_date.html