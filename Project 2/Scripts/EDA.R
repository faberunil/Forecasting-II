# Load the packages
library(here)
library(tsibble)
library(ggplot2)
library(fpp3)


### Atlantic - EDA  ####
########################

#### Data preparation ####

# Read the data
atl_data <- read.csv(here("Project 2", "Data", "Atlantic sea.csv"), header = TRUE, skip = 6)

# Drop mean.temperature.uncertainty, and mean.temperature.kelvin columns
atl_data <- subset(atl_data, select = -c(mean.temperature.uncertainty, mean.temperature.kelvin, day))

# Calculate monthly mean temperature
atl_data <- atl_data |>
  group_by(year, month) |>
  summarise(mean_temp = mean(mean.temperature.deg.C, na.rm = TRUE),
            .groups = 'drop') |>
  mutate(month_year = yearmonth(paste(year, month, sep = "-")))

# Create a tsibble object
ts_atl <- as_tsibble(atl_data, index = month_year)


### VISUAlISATION ###


### Timeseries plot
ts_data |> autoplot(mean.temperature.deg.C) + ggtitle("Daily Sea Surface Temperature") + ylab("Temperature (°C)") + xlab("Time")

### Seasonal plot

# Plot the seasonality with gg_season
ts_atl |>
  gg_season(mean_temp, labels = "both") +
  labs(y = "Temperature (°C)",
       title = "Daily Sea Surface Temperature")

### STL Decomposition ###
#########################

# STL decomp
dcmp <- ts_atl |> model(STL(mean_temp)) 
components(dcmp)

# Plot the trend
ts_atl |> 
  autoplot(mean_temp, color='gray') + autolayer(components(dcmp), trend, color='red') + xlab("Year") + ylab("Temperature (°C)") + ggtitle("Daily Sea Surface Temperature")

# Show all compononents of our time serie
components(dcmp) %>% autoplot() + xlab("Year")

# Plot season adjusted
ts_atl |>
  autoplot(mean_temp, color='gray') + autolayer(components(dcmp), season_adjust, color='blue') + xlab("Year") + ylab("Temperature (°C)") + ggtitle("Daily Sea Surface Temperature")

### Gulf of Mexico - EDA  ####
##############################

#### Data preparation ####

# Read the data
gulf_data <- read.csv(here("Project 2", "Data", "Gulf of mexico.csv"), header = TRUE, skip = 6)

# Drop mean.temperature.uncertainty, and mean.temperature.kelvin columns
gulf_data <- subset(gulf_data, select = -c(mean.temperature.uncertainty, mean.temperature.kelvin, day))

# Calculate monthly mean temperature
gulf_data <- gulf_data |>
  group_by(year, month) |>
  summarise(mean_temp = mean(mean.temperature.deg.C, na.rm = TRUE),
            .groups = 'drop') |>
  mutate(month_year = yearmonth(paste(year, month, sep = "-")))

# Create a tsibble object
ts_gulf <- as_tsibble(gulf_data, index = month_year)

### VISUAlISATION ###

### Timeseries plot
ts_gulf |> autoplot(mean_temp) + ggtitle("Daily Sea Surface Temperature") + ylab("Temperature (°C)") + xlab("Time")

### Seasonal plot

# Plot the seasonality with gg_season
ts_gulf |>
  gg_season(mean_temp, labels = "both") +
  labs(y = "Temperature (°C)",
       title = "Daily Sea Surface Temperature")

### STL Decomposition ###
#########################

# STL decomp
dcmp_gulf <- ts_gulf |> model(STL(mean_temp))
components(dcmp_gulf)

# Plot the trend
ts_gulf |> 
  autoplot(mean_temp, color='gray') + autolayer(components(dcmp_gulf), trend, color='red') + xlab("Year") + ylab("Temperature (°C)") + ggtitle("Daily Sea Surface Temperature")

# Show all compononents of our time serie
components(dcmp_gulf) %>% autoplot() + xlab("Year")

# Plot season adjusted
ts_gulf |>
  autoplot(mean_temp, color='gray') + autolayer(components(dcmp_gulf), season_adjust, color='blue') + xlab("Year") + ylab("Temperature (°C)") + ggtitle("Daily Sea Surface Temperature")


