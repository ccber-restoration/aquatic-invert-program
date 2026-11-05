## purpose of this script is to practice using github and ggplot

# run 00_setup.R script, which reads in Excel file and does some processing/cleaning
source("code/00_setup.R")


# loads a package used to make figures interactive
library(plotly)





#Reorder sample types
reorder_invert_data <-invert_data |> 
  mutate(sample_type = fct_relevel(sample_type, "FB250", "FB 250", "FB250 core", "FB 18L", "Core 1cm", "CORE 3-4", "CORE: cm 3", "SW250", "SW500","Algae", "SAV")) |> 
  mutate(sample_type = fct_rev(sample_type))




# create plot 
# dataframe is invert_data
# x axis is date on vial,
# color for points is the sample type
figure_timeline <- ggplot(data = reorder_invert_data,
                          mapping = aes(x = date_on_vial, y = sample_type, color = sample_type)) +
  
  # add points to figure with a small amount of randomness so that points aren't directly on top of each other
  # only move points in the (arbitrary) vertical direction, not horizontally
  geom_jitter(height = 0.1) 




#for non interactive graph with new labels 
figure_timeline

figure_timeline + 
  labs (x="Years", 
        y= "Methods"
  ) + 
  #for non interactive ticks 
scale_y_discrete(labels = c(
  "SAV" = "SAV",
  "Algae" = "Algae",
  "SW500" = "Sweep Net 500",
  "SW250" = "Sweep Net 250", 
  "CORE: cm 3"= "Core: cm 3",
  "CORE 3-4" = "Core 3-4",
  "Core 1cm" = "Core 1 cm",
  "FB 18L" = "Filter Breaker 18L", 
  "FB250 core" = "Filter Breaker 250", 
  "FB 250" = "Filter Breaker 250", 
  "FB250" = "Filter Breaker 250"))



#This piece allows the the title to go on the interactive version too 

figure_timeline <- figure_timeline +
  labs(x="Years",
       y= "Methods"
       ) +

#This changes the ticks for interactive 

  scale_y_discrete(labels = c(
    "SAV" = "SAV",
    "Algae" = "Algae",
    "SW500" = "Sweep Net 500",
    "SW250" = "Sweep Net 250", 
    "CORE: cm 3"= "Core: cm 3",
    "CORE 3-4" = "Core 3-4",
    "Core 1cm" = "Core 1 cm",
    "FB 18L" = "Filiter Breaker 18L", 
    "FB250 core" = "Filter Breaker 250", 
    "FB 250" = "Filter Breaker 250", 
    "FB250" = "Filter Breaker 250"))
                   

# make interactive
ggplotly(figure_timeline)


#I am not sure if the rest of the code is needed since up top seems to do the job we need it to do 


# subset invert data to unique sample dates
invert_data_unique_dates <- invert_data |> 
  # select just two columns of interest: date and sample type
  select(date_on_vial, sample_type) |> 
  distinct()


# create new timeline figure, ignoring site ----
figure_timeline_simple <- ggplot(data = invert_data_unique_dates,
                                 mapping = aes(x = date_on_vial, y = 1, color = sample_type)) +
  # add points to figure with a small amount of randomness so that points aren't directly on top of each other
  # only move points in the (arbitrary) vertical direction, not horizontally
  geom_jitter(height = 0.1) +
  # set the y axis limits to 0 to 2
  scale_y_continuous(limits = c(0, 2)) +
  # use a separate panel for each method


figure_timeline_simple

ggplotly(figure_timeline_simple)


