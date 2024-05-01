# Load the packages
library(here)
library(tsibble)
library(ggplot2)
library(fpp3)


### DATA ###
############

### Atlantic data

# Read the data using a path relative to the project root including the 'Project 2' folder
atl_data <- read.csv(here("Project 2", "Data", "Atlantic sea.csv"), header = TRUE, skip = 6)

# Convert the data frame to a tsibble
ts_data <- atl_data |>
  transform(Date = as.Date(paste(year, month, day, sep = "-"))) |>
  as_tsibble(index = Date)

# Check for missing values
summary(is.na(ts_data$mean.temperature.deg.C)) # <-- no missing values

# Summary statistics 
summary(ts_data$mean.temperature.deg.C)


### Visualisation ###
#####################

### Timeseries plot ###

# Plot the temperature data using ggplot2
ggplot(ts_data, aes(x = Date, y = mean.temperature.deg.C)) +
  geom_line() +
  labs(title = "Daily Sea Surface Temperature",
       x = "Date",
       y = "Temperature (°C)") +
  theme_minimal()

# Same with autoplot
ts_data |> autoplot(mean.temperature.deg.C) + ggtitle("Daily Sea Surface Temperature") + ylab("Temperature (°C)") + xlab("Time")

### Seasonal plot ###

# Plot the seasonality with gg_season
ts_data |>
  gg_season(mean.temperature.deg.C, labels = "both") +
  labs(y = "Temperature (°C)",
       title = "Daily Sea Surface Temperature")

### STL Decomposition ###
#########################

# STL decomp
dcmp <- ts_data |> model(STL(mean.temperature.deg.C)) 
components(dcmp)

# Plot the trend
ts_data |> 
  autoplot(mean.temperature.deg.C, color='gray') + autolayer(components(dcmp), trend, color='red') + xlab("Year") + ylab("Temperature (°C)") + ggtitle("Daily Sea Surface Temperature")

# Show all compononents of our time serie
components(dcmp) %>% autoplot() + xlab("Year")

# Plot season adjusted
ts_data |>
  autoplot(mean.temperature.deg.C, color='gray') + autolayer(components(dcmp), season_adjust, color='blue') + xlab("Year") + ylab("Temperature (°C)") + ggtitle("Daily Sea Surface Temperature")
