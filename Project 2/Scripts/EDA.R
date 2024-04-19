# Load the packages
library(here)
library(tsibble)
library(ggplot2)

# Read the data using a path relative to the project root including the 'Project 2' folder
data <- read.csv(here("Project 2", "Data", "seatemp.csv"), header = TRUE, skip = 2)

# Convert the data frame to a tsibble
ts_data <- data |>
  transform(Date = as.Date(paste(year, month, day, sep = "-"))) |>
  as_tsibble(index = Date)

# Plot the temperature data using ggplot2
ggplot(ts_data, aes(x = Date, y = mean.temperature.deg.C)) +
  geom_line() +
  labs(title = "Daily Sea Surface Temperature",
       x = "Date",
       y = "Temperature (°C)") +
  theme_minimal()