### Forecasting 2 - Project 2 ###
#################################

library(here)
library(tsibble)
library(ggplot2)
library(fpp3)


### Atlantic - ETS Forecast ####
##################################

#### Data preparation

# Read the data 
atl_data <- read.csv(here("Project 2", "Data", "Atlantic sea.csv"), header = TRUE, skip = 6)

# Convert the data frame to a tsibble
ts_atl <- atl_data |>
  transform(Date = as.Date(paste(year, month, day, sep = "-"))) |>
  filter(Date >= "2000-01-01") |>
  as_tsibble(index = Date)

#### ETS Forecast

# Fitting the ETS model
ets_atl_fit <- ts_atl |> model(ETS(mean.temperature.deg.C))
help(forecast)

atl_fit <- ts_atl |> model(ETS(mean.temperature.deg.C ~ error("A") + trend("A") + season("A")))

report(atl_fit)

# Plot the forecast for the next N year
atl_fit |> forecast(h = 100) |> autoplot(ts_atl)

# Model Accuracy
atl_fit |> accuracy()

# Residuals of the model
atl_fit |> gg_tsresiduals(lag_max = "3 years")
