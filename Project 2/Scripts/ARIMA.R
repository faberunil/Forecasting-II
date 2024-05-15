### Forecasting 2 - Project 2 ###
#################################

### !!! NOT USED IN THE PRESENTATION !!! ###

library(here)
library(tsibble)
library(ggplot2)
library(fpp3)
library(dplyr)


### Atlantic - ARIMA Forecast ####
##################################

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

#### ARIMA Forecast ####

# Fitting ARIMA Model
arima_atl_fit <- ts_atl |>
  model(auto = ARIMA(mean_temp, stepwise = FALSE, approximation = FALSE))
report(arima_atl_fit)

# Plot the forecast for the next N years
arima_atl_fit |> forecast(h = 120) |> autoplot(ts_atl)

# Model Accuracy
arima_atl_fit |> accuracy()

# Residuals of the model
arima_atl_fit |> gg_tsresiduals(lag_max = 120)


### Gulf of Mexico - ARIMA Forecast ####
########################################

#### Data preparation

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

#### ARIMA Forecast ####

# Fitting ARIMA Model
arima_gulf_fit <- ts_gulf |>
  model(auto = ARIMA(mean_temp, stepwise = FALSE, approximation = FALSE))
report(arima_gulf_fit)

# Plot the forecast for the next 10 year
arima_gulf_fit |> forecast(h = 120) |> autoplot(ts_gulf)

# Model Accuracy
arima_gulf_fit |> accuracy()

# Residuals of the model
arima_gulf_fit |> gg_tsresiduals(lag_max = 120)

