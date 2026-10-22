# the purpose of this script is to begin exploring, visualizing, and summarizing water quality measurements

# run 00_setup.R to obtain the water_quality dataframe

# Note that the input dataframe will change slightly after migrating to the "new" spreadsheet

library(naniar)

vis_miss(water_quality)
