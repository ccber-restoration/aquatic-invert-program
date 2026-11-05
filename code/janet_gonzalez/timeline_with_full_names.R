## purpose of this script is to practice using github and ggplot

# run 00_setup.R script, which reads in Excel file and does some processing/cleaning
source("code/00_setup.R")


# loads a package used to make figures interactive
library(plotly)

# create custom labels for sample types

labels_sample_type <- c(
  "SAV" = "Submerged \n Aquatic \n Vegetation",
  "Algae" = "Algae",
  "SW500" = "Sweep Net 500",
  "SW250" = "Sweep Net 250", 
  "CORE: cm 3"= "Core: cm 3",
  "CORE 3-4" = "Core 3-4",
  "Core 1cm" = "Core 1 cm",
  "FB 18L" = "Filtered Beaker 18L", 
  "FB250 core" = "Filtered Beaker 250", 
  "FB 250" = "Filtered Beaker 250", 
  "FB250" = "Filtered Beaker 250")


# Reorder sample types
reorder_invert_data <-invert_data |> 
  # make sure to exclude rare sample types
  filter_out(sample_type %in% c("Core 1cm", "CORE 3-4", "CORE: cm 3", "FB250 core", "FB 18L", "N/A")) |> 
  mutate(sample_type = case_when(
    sample_type == "FB 250" ~ "FB250",
    .default = sample_type
  )) |> 
  mutate(sample_type = fct_relevel(sample_type, "FB250", "SW250", "SW500","Algae", "SAV")) |> 
  # reverses the order of levels
  mutate(sample_type = fct_rev(sample_type))

unique(reorder_invert_data$sample_type)


# create plot 
# dataframe is invert_data
# x axis is date on vial,
# color for points is the sample type
figure_timeline <- ggplot(data = reorder_invert_data,
                          mapping = aes(x = date_on_vial, y = sample_type, color = sample_type)) +
  
  # add points to figure with a small amount of randomness so that points aren't directly on top of each other
  # only move points in the (arbitrary) vertical direction, not horizontally
  geom_jitter(height = 0.2) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  # use full sample type names as labels, rather than raw values
  scale_y_discrete(labels = labels_sample_type) +
  # label axes
  labs (x="Date", 
        y= "Method"
  ) +
  # remove legend
  theme(legend.position = "none")


# view figure
figure_timeline


# make interactive
ggplotly(figure_timeline)


# use ggsave() to export figure to pdf
ggsave(filename = "figures/Gonzalez_Janet/sample_type_timeline_labeled.pdf",
       plot = figure_timeline,
       width = 5.5,
       height = 3,
       units = "in")



# CURRENTLY NOT USED:
#I am not sure if the rest of the code is needed since up top seems to do the job we need it to do 


# subset invert data to unique sample dates
invert_data_unique_dates <- reorder_invert_data |> 
  # select just two columns of interest: date and sample type
  select(date_on_vial, sample_type) |> 
  distinct()


# create new timeline figure, ignoring site ----
figure_timeline_simple <- ggplot(data = invert_data_unique_dates,
                                 mapping = aes(x = date_on_vial, y = sample_type, color = sample_type)) +
  # add points to figure with a small amount of randomness so that points aren't directly on top of each other
  # only move points in the (arbitrary) vertical direction, not horizontally
  geom_jitter(height = 0.1) + 
  # for non interactive ticks 
  scale_y_discrete(labels = labels_sample_type)
  # set the y axis limits to 0 to 2
  # scale_y_continuous(limits = c(0, 2))


figure_timeline_simple

ggplotly(figure_timeline_simple)


