### Forecasting 2 - Project 2 ###
#################################

library(here)
library(tsibble)
library(ggplot2)
library(fpp3)
library(dplyr)


### Atlantic - ETS Forecast ####
##################################

#### Data preparation

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

#### ETS Forecast

# Fitting the ETS model
ets_atl_fit <- ts_atl |> model(ETS(mean_temp))

atl_fit <- ts_atl |> model(ETS(mean_temp ~ error("A") + trend("A") + season("A")))

report(atl_fit)

# Plot the forecast for the next N year
atl_fit |> forecast(h = 365) |> autoplot(ts_atl)

# Model Accuracy
atl_fit |> accuracy()

# Residuals of the model
atl_fit |> gg_tsresiduals(lag_max = "3 years")
