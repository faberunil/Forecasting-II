### Forecasting 2 - Project 2 ###
#################################

library(here)
library(tsibble)
library(ggplot2)
library(fpp3)
library(dplyr)


### Atlantic - ETS Forecast ####
################################

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

#### ETS Forecast ####

# Fitting ETS Auto Model : ETS(M, Ad, A)
ets_atl_fit <- ts_atl |> model(ETS(mean_temp))
report(ets_atl_fit)

# Plot the forecast for the next N year
ets_atl_fit |> forecast(h = 120) |> autoplot(ts_atl)

# Model Accuracy
ets_atl_fit |> accuracy()

# Residuals of the model
ets_atl_fit |> gg_tsresiduals(lag_max = 120)


### Gulf of Mexico - ETS Forecast ####
######################################


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

#### ETS Forecast ####

# Fitting ETS Auto Model : ETS(A, A, A)
ets_gulf_fit <- ts_gulf |> model(ETS(mean_temp ~ error("A") + trend("A") + season("A")))
report(ets_gulf_fit)

# Plot the forecast for the next N year
ets_gulf_fit |> forecast(h = 120) |> autoplot(ts_gulf)

# Model Accuracy
ets_gulf_fit |> accuracy()

# Residuals of the model
ets_gulf_fit |> gg_tsresiduals(lag_max = 120)


