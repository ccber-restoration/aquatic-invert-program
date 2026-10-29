source("code/00_setup.R")

# loads a package used to make figures interactive
library(plotly)


# summarize number of samples per quarter

summary_by_quarter <- invert_data |> 
  # create a column for quarter
  mutate(quarter = quarter(date_on_vial),
         doy = yday(date_on_vial)) |> 
  group_by(year, quarter, sample_type) |> 
  summarize(n_samples = n()) |> 
  arrange(year, quarter)



# March 15th is the ~74th day of year
# March 31st is the 90th day of year

quarter(as.Date("2026-01-29"))

# create plot 
# dataframe is invert_data
# x axis is date on vial,
# color for points is the sample type
figure_timeline <- ggplot(data = invert_data,
                          mapping = aes(x = date_on_vial, y = site, color = sample_type)) +
  # add points to figure with a small amount of randomness so that points aren't directly on top of each other
  # only move points in the (arbitrary) vertical direction, not horizontally
  geom_jitter(height = 0.2, width = 0, size = 2) +
  labs(title = "Sample timeline, based on invert sheet downloaded 2026-09-04")

# view figure
figure_timeline


