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

<<<<<<< HEAD
# Convert the data frame to a tsibble
ts_atl <- atl_data |>
  transform(Day = as.Date(paste(year, month, day, sep = "-"))) |>
  filter(Day >= "2000-01-01") |>
  as_tsibble(index = Day)
=======
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
>>>>>>> 8d4bfbf8ef98f700865ba953d19dddd6fed178be

#### ETS Forecast

# Fitting the ETS model
<<<<<<< HEAD
ets_atl_fit <- ts_atl |> model(ETS(mean.temperature.deg.C))

atl_fit <- ts_atl |> model(ETS(mean.temperature.deg.C ~ error("A") + trend("A") + season("A")))
=======
ets_atl_fit <- ts_atl |> model(ETS(mean_temp))

atl_fit <- ts_atl |> model(ETS(mean_temp ~ error("A") + trend("A") + season("A")))
>>>>>>> 8d4bfbf8ef98f700865ba953d19dddd6fed178be

report(atl_fit)

# Plot the forecast for the next N year
atl_fit |> forecast(h = 365) |> autoplot(ts_atl)

# Model Accuracy
atl_fit |> accuracy()

# Residuals of the model
atl_fit |> gg_tsresiduals(lag_max = "3 years")
