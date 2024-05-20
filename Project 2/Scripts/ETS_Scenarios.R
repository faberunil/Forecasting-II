library(fable)
library(dplyr)
library(ggplot2)
library(tsibble)
library(ggplot2)

### Atlantic - Scenarios Forecast ####
######################################

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

#### Scenarios Forecast ####

# Fit the normal ETS model
ets_normal_fit <- ts_atl |> model(ETS(mean_temp ~ error("M") + trend("A") + season("A")))

# Normal forecast
forecast_normal <- forecast(ets_normal_fit, h = 120) 

# Convert the fable object to a tibble
normal_forecasts <- as_tibble(forecast_normal)

# Create time index for adjustments
normal_forecasts <- normal_forecasts %>%
  mutate(years_since_start = (row_number() - 1) / 12)  

# Apply a gradual increase
pessimistic_forecasts <- normal_forecasts %>%
  mutate(scaling_factor = case_when(
    years_since_start <= 3  ~ 1.005,
    years_since_start <= 6  ~ 1.01,
    years_since_start <= 9  ~ 1.015,
    TRUE                    ~ 1.02
  ),
  .mean = .mean * scaling_factor)

# Apply a gradual decrease
optimistic_forecasts <- normal_forecasts %>%
  mutate(scaling_factor = case_when(
    years_since_start <= 3  ~ 0.995,
    years_since_start <= 6  ~ 0.99,
    years_since_start <= 9  ~ 0.985,
    TRUE                    ~ 0.98
  ),
  .mean = .mean * scaling_factor)

# Plot the scenarios
ggplot(ts_atl, aes(x = month_year, y = mean_temp)) +
  geom_line(color = "black", size = 0.5, aes(label = "Historical Data")) +
  geom_line(data = normal_forecasts, aes(y = .mean, color = "Normal"), size = 0.6) +
  geom_line(data = pessimistic_forecasts, aes(y = .mean, color = "Pessimistic"), size = 0.6) +
  geom_line(data = optimistic_forecasts, aes(y = .mean, color = "Optimistic"), size = 0.6) +
  scale_color_manual(values = c("Normal" = "blue", "Pessimistic" = "red", "Optimistic" = "green")) +
  labs(title = "Forecast of the Atlantic Sea Temperature with Different Scenarios",
       x = "Year", y = "Mean Temperature") +
  theme_minimal()


### Gulf of Mexico - Scenarios Forecast ####
############################################

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

#### Scenarios Forecast ####

# Fit the normal ETS model
ets_normal_fit_gulf <- ts_gulf |> model(ETS(mean_temp ~ error("M") + trend("A") + season("A")))

# Normal forecast
forecast_normal_gulf <- forecast(ets_normal_fit_gulf, h = 120)

# Convert the fable object to a tibble
normal_forecasts_gulf <- as_tibble(forecast_normal_gulf)

# Create time index for adjustments
normal_forecasts_gulf <- normal_forecasts_gulf %>%
  mutate(years_since_start = (row_number() - 1) / 12)

# Apply a gradual increase
pessimistic_forecasts_gulf <- normal_forecasts_gulf %>%
  mutate(scaling_factor = case_when(
    years_since_start <= 3  ~ 1.005,
    years_since_start <= 6  ~ 1.01,
    years_since_start <= 9  ~ 1.015,
    TRUE                    ~ 1.02
  ),
  .mean = .mean * scaling_factor)

# Apply a gradual decrease
optimistic_forecasts_gulf <- normal_forecasts_gulf %>%
  mutate(scaling_factor = case_when(
    years_since_start <= 3  ~ 0.995,
    years_since_start <= 6  ~ 0.99,
    years_since_start <= 9  ~ 0.985,
    TRUE                    ~ 0.98
  ),
  .mean = .mean * scaling_factor)

# Plot the scenarios
ggplot(ts_gulf, aes(x = month_year, y = mean_temp)) +
  geom_line(color = "black", size = 0.5, aes(label = "Historical Data")) +
  geom_line(data = normal_forecasts_gulf, aes(y = .mean, color = "Normal"), size = 0.6) +
  geom_line(data = pessimistic_forecasts_gulf, aes(y = .mean, color = "Pessimistic"), size = 0.6) +
  geom_line(data = optimistic_forecasts_gulf, aes(y = .mean, color = "Optimistic"), size = 0.6) +
  scale_color_manual(values = c("Normal" = "blue", "Pessimistic" = "red", "Optimistic" = "green")) +
  labs(title = "Forecast of the Gulf of Mexico Sea Temperature with Different Scenarios",
       x = "Year", y = "Mean Temperature") +
  theme_minimal()


