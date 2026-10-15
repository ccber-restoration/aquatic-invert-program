## purpose of this script is to practice using github and ggplot

# run 
source("code/00_setup.R")

library(plotly)


# plot 
figure_timeline <- ggplot(data = invert_data, mapping = aes(x = date_on_vial, y = 1, color = sample_type)) +
  geom_jitter(height = 0.1) +
  scale_y_continuous(limits = c(0,2)) +
  facet_wrap(vars(sample_type))

figure_timeline

ggplotly(figure_timeline)
