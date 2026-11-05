library(ggplot2)
library(zoo) # Provides yearqtr class and scale_x_yearqtr

# 1. Sample Data
df <- data.frame(
  date = seq(as.Date("2025-01-01"), as.Date("2026-12-01"), by = "month"),
  value = runif(24, 10, 50)
)

# 2. Convert date column to a formal year-quarter structure
df$quarter <- as.yearqtr(df$date)

# 3. Plotting
ggplot(df, aes(x = quarter, y = value)) +
  geom_point() +
  # geom_line(color = "darkorange", size = 1) +
  scale_x_yearqtr(
    format = "%Y-Q%q",                      # Displays format as: 2026-Q1
    n = 8                                  # Approximate number of quarterly breaks to show
  ) +
  theme_minimal()

