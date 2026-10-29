## purpose of this script is to practice using github and ggplot

# run 00_setup.R script, which reads in Excel file and does some processing/cleaning
source("code/00_setup.R")

# loads a package used to make figures interactive
library(plotly)

#Reorder sample types
reorder_invert_data <-invert_data |> 
  mutate(sample_type = fct_relevel(sample_type, "FB250", "SW250", "SW500")) |> 
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
 
 

figure_timeline

# make interactive
ggplotly(figure_timeline)

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
