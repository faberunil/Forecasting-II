### Forecasting 2 - Project 2 ###
#################################

library(here)
library(tsibble)
library(ggplot2)
library(fpp3)


### Atlantic - ARIMA Forecast ####
##################################

#### Data preparation

# Read the data 
atl_data <- read.csv(here("Project 2", "Data", "Atlantic sea.csv"), header = TRUE, skip = 6)

# Convert the data frame to a tsibble
ts_atl <- atl_data |>
  transform(Date = as.Date(paste(year, month, day, sep = "-"))) |>
  as_tsibble(index = Date)

#### Auto ARIMA Forecast

# Fitting the ARIMA model with auto ARIMA
atl_fit <- ts_atl |>
  model(auto = ARIMA(mean.temperature.deg.C, stepwise = FALSE, approximation = FALSE))

atl_fit

# Plot the forecast for the next N years
atl_fit |> forecast(h = "3 years") |> autoplot(ts_atl)

# Residuals of the model
atl_fit |> gg_tsresiduals(lag_max = "3 years")

# Model Accuracy
atl_fit |> accuracy()

### Gulf of Mexico - ARIMA Forecast ####
########################################

#### Data preparation

# Read the data 
gulf_data <- read.csv(here("Project 2", "Data", "Gulf of mexico.csv"), header = TRUE, skip = 6)

# Convert the data frame to a tsibble
ts_gulf <- gulf_data |>
  transform(Date = as.Date(paste(year, month, day, sep = "-"))) |>
  as_tsibble(index = Date)

#### Auto ARIMA Forecast

# Fitting the ARIMA model with auto ARIMA
gulf_fit <- ts_gulf |>
  model(auto = ARIMA(mean.temperature.deg.C, stepwise = FALSE, approximation = FALSE))

gulf_fit

# Plot the forecast for the next 10 year
gulf_fit |> forecast(h = 365) |> autoplot(ts_gulf)

# Residuals of the model
gulf_fit |> gg_tsresiduals(lag_max = 365)

# Model Accuracy
gulf_fit |> accuracy()