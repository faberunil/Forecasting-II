### Forecasting 2 - Project 2 ###
#################################

library(here)
library(dplyr)
library(ggplot2)
library(zoo)
library(lubridate)
library(forecast)
library(fpp3)
library(fable)


#### Data preparation ####
##########################

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

# Read the data
co2_data <- read.csv(here("Project 2", "Data", "CO2.csv"), header = TRUE)

co2_data <- co2_data |>
  group_by(Year, Month) |>
  mutate(month_year = yearmonth(paste(Year, Month, sep = "-")))

# Create a tsibble object
ts_co2 <- as_tsibble(co2_data, index = month_year)

# Convert column names to lowercase for consistency
names(ts_co2) <- tolower(names(ts_co2))
names(ts_atl) <- tolower(names(ts_atl))

# Merge the datasets on the 'month_year' column
merged_df <- merge(ts_atl, ts_co2, by = "month_year", suffixes = c("_temp", "_co2"))
ts_merged <- as_tsibble(merged_df, index = month_year)

### EDA - CO2 emissions ###
###########################

# Plot the CO2 time series 
ts_co2 |> autoplot(mean_c02, ylab = "CO2 Levels (ppm)", xlab = "Time") +
  ggtitle("CO2 Levels Over Time") +
  theme_minimal()

# Seasonal plot
ts_co2 |>
  gg_season(mean_c02, labels = "both") +
  labs(y = "Monthly Mean CO2 Levels (ppm)", title = "Seasonal Plot of Monthly CO2 Levels")

# STL decomp
co2_dcmp <- ts_co2 |> model(STL(mean_c02)) 
components(co2_dcmp)

# Show all compononents of our time serie
components(co2_dcmp) %>% autoplot() + xlab("Year")

# Plot the trend
ts_co2 |> 
  autoplot(mean_c02, color='gray') + autolayer(components(co2_dcmp), trend, color='red') + xlab("Year") + ylab("CO2 Levels") + ggtitle("Monthly CO2 Emissions")

# Plot season adjusted
ts_co2 |>
  autoplot(mean_c02, color='gray') + autolayer(components(co2_dcmp), season_adjust, color='blue') + xlab("Year") + ylab("CO2 Levels") + ggtitle("Monthly CO2 Emissions")

### Plotting relation between CO2 and Sea Temperature ###

ggplot(merged_df, aes(x = mean_c02, y = mean_temp)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Regression Analysis: Mean Temperature vs. Mean CO2 Levels",
       x = "Monthly Mean CO2 Levels", y = "Monthly Mean Temperature") +
  theme_minimal()

### Change over time ###

# Calculate the month-on-month percentage change
monthly_changes <- ts_merged %>%
  mutate(
    pct_change_c02 = (mean_c02 - lag(mean_c02)) / lag(mean_c02) * 100,
    pct_change_temp = (mean_temp - lag(mean_temp)) / lag(mean_temp) * 100
  )

# Remove NA values that appear in the first row after lagging
monthly_changes <- monthly_changes %>%
  filter(!is.na(pct_change_c02) & !is.na(pct_change_temp))

# Plotting the month-on-month percentage changes
ggplot(data = monthly_changes, aes(x = month_year)) +
  geom_line(aes(y = pct_change_c02, color = "CO2 Emissions")) +
  geom_line(aes(y = pct_change_temp, color = "Sea Temperature")) +
  labs(
    title = "Month-on-Month Percentage Change in CO2 Emissions and Sea Temperature",
    x = "Month-Year",
    y = "Percentage Change (%)",
    color = "Variable"
  ) +
  scale_color_manual(values = c("CO2 Emissions" = "red", "Sea Temperature" = "blue")) +
  theme_minimal() +
  theme(legend.title = element_text(color = "black", size = 10, face = "bold"),
        legend.text = element_text(color = "black"))

### Scaled plot of both time series ###

# Scale the data
ts_merged <- ts_merged %>%
  mutate(
    scaled_c02 = (mean_c02 - min(mean_c02)) / (max(mean_c02) - min(mean_c02)),  # Scaling CO2
    scaled_temp = (mean_temp - min(mean_temp)) / (max(mean_temp) - min(mean_temp))  # Scaling Temperature
  )

# Plot the scaled data 
ggplot(data = ts_merged, aes(x = month_year)) +
  geom_line(aes(y = scaled_c02, color = "CO2 Emissions")) +  # CO2 plot
  geom_line(aes(y = scaled_temp, color = "Sea Temperature")) +  # Sea Temperature plot
  labs(
    title = "Normalized Changes Over Time in CO2 Emissions and Sea Temperature",
    x = "Time",
    y = "Normalized Values"
  ) +
  scale_color_manual(values = c("CO2 Emissions" = "red", "Sea Temperature" = "blue"),
                     name = "Metric",
                     labels = c("CO2 Emissions", "Sea Temperature")) +
  theme_minimal() +
  theme(legend.title = element_text(color = "black", size = 10, face = "bold"),
        legend.text = element_text(color = "black"))

### Correlation ###
###################

correlation <- cor(merged_df$mean_temp, merged_df$mean_c02, use = "complete.obs")  # Use complete.obs to handle missing values
correlation

### Atlantic - TSLM Forecast ####
#################################


### TSLM FORECAST ###
#####################

# Creating lagged CO2 variable
ts_merged <- ts_merged %>%
  mutate(mean_c02_lag10 = lag(mean_c02, 120))  

# Fit a TSLM model with lagged CO2 over 10 years
tslm_model <- ts_merged %>%
  filter(!is.na(mean_c02_lag10)) %>%  # Remove NA entries that result from lagging
  model(TSLM(mean_temp ~ mean_c02_lag10 + trend() + season()))

# Report the model
report(tslm_model)

### Generate new data based on 4 scenarios 

# Capture the last known CO2 value
last_co2_value <- last(ts_merged$mean_c02_lag10)

# Generate baseline new data
base_new_data <- new_data(ts_merged, n = 120)


# Prepare scenario data - NOTE : actual avg. CO2 emissions increase was ~1.1% last year
scenario_data <- base_new_data %>%
  mutate(
    high_emission = last_co2_value * (1 + 0.04) ^ (seq_len(n()) / 12),
    moderate_emission = last_co2_value * (1 + 0.02) ^ (seq_len(n()) / 12),
    low_emission = last_co2_value * (1 + 0.01) ^ (seq_len(n()) / 12),
    decreasing_emission = last_co2_value * (1 + 0.003) ^ (seq_len(n()) / 12)  # Stabalise increase at 0.25%
  )

# Forecast using the tslm_model for each scenario
forecast_high <- forecast(tslm_model, new_data = mutate(base_new_data, mean_c02_lag10 = scenario_data$high_emission))
forecast_moderate <- forecast(tslm_model, new_data = mutate(base_new_data, mean_c02_lag10 = scenario_data$moderate_emission))
forecast_low <- forecast(tslm_model, new_data = mutate(base_new_data, mean_c02_lag10 = scenario_data$low_emission))
forecast_stable <- forecast(tslm_model, new_data = mutate(base_new_data, mean_c02_lag10 = scenario_data$decreasing_emission))

# Convert forecasts to fable for easy plotting
forecast_high_fable <- as_fable(forecast_high, response = "mean_temp")
forecast_moderate_fable <- as_fable(forecast_moderate, response = "mean_temp")
forecast_low_fable <- as_fable(forecast_low, response = "mean_temp")
forecast_stable_fable <- as_fable(forecast_stable, response = "mean_temp")

# Plotting the forecasts
ggplot(ts_merged, aes(x = month_year, y = mean_temp)) +
  geom_line(aes(color = "Actual Data")) +
  geom_line(data = forecast_high_fable, aes(y = .mean, color = "High Emission")) +
  geom_line(data = forecast_moderate_fable, aes(y = .mean, color = "Moderate Emission")) +
  geom_line(data = forecast_low_fable, aes(y = .mean, color = "Low Emission")) +
  geom_line(data = forecast_stable_fable, aes(y = .mean, color = "Stable Emission")) +
  labs(title = "Forecasted Mean Temperature Based on Different CO2 Emission Scenarios",
       x = "Time", y = "Mean Temperature") +
  scale_color_manual(values = c("Actual Data" = "black", "High Emission" = "red", "Moderate Emission" = "orange", "Low Emission" = "lightgreen", "Stable Emission" = "blue"),
                     name = "Scenario") +
  theme_minimal() +
  guides(color = guide_legend(title = "Data Type"))


### Gulf - TSLM Forecast ####
#############################

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

# Convert column names to lowercase for consistency
names(ts_gulf) <- tolower(names(ts_gulf))

# Merge the datasets on the 'month_year' column
merged_gulf_df <- merge(ts_gulf, ts_co2, by = "month_year", suffixes = c("_temp", "_co2"))
ts_merged_gulf <- as_tsibble(merged_gulf_df, index = month_year)


### TSLM FORECAST ###
#####################

# Creating lagged CO2 variable
ts_merged_gulf <- ts_merged_gulf %>%
  mutate(mean_c02_lag10 = lag(mean_c02, 120))

# Fit a TSLM model with lagged CO2 over 10 years
tslm_model_gulf <- ts_merged_gulf %>%
  filter(!is.na(mean_c02_lag10)) %>%  # Remove NA entries that result from lagging
  model(TSLM(mean_temp ~ mean_c02_lag10 + trend() + season()))

# Report the model
report(tslm_model_gulf)

### Generate new data based on 4 scenarios

# Capture the last known CO2 value
last_co2_value_gulf <- last(ts_merged_gulf$mean_c02_lag10)

# Generate baseline new data
base_new_data_gulf <- new_data(ts_merged_gulf, n = 120)

# Prepare scenario data - NOTE : actual avg. CO2 emissions increase was ~1.1% last year
scenario_data_gulf <- base_new_data_gulf %>%
  mutate(
    high_emission = last_co2_value_gulf * (1 + 0.04) ^ (seq_len(n()) / 12),
    moderate_emission = last_co2_value_gulf * (1 + 0.02) ^ (seq_len(n()) / 12),
    low_emission = last_co2_value_gulf * (1 + 0.01) ^ (seq_len(n()) / 12),
    stable_emission = last_co2_value_gulf * (1 + 0.003) ^ (seq_len(n()) / 12)  # Stabalise increase at 0.25%
  )

# Forecast using the tslm_model for each scenario
forecast_high_gulf <- forecast(tslm_model_gulf, new_data = mutate(base_new_data_gulf, mean_c02_lag10 = scenario_data_gulf$high_emission))
forecast_moderate_gulf <- forecast(tslm_model_gulf, new_data = mutate(base_new_data_gulf, mean_c02_lag10 = scenario_data_gulf$moderate_emission))
forecast_low_gulf <- forecast(tslm_model_gulf, new_data = mutate(base_new_data_gulf, mean_c02_lag10 = scenario_data_gulf$low_emission))
forecast_stable_gulf <- forecast(tslm_model_gulf, new_data = mutate(base_new_data_gulf, mean_c02_lag10 = scenario_data_gulf$stable_emission))

# Convert forecasts to fable for easy plotting
forecast_high_fable_gulf <- as_fable(forecast_high_gulf, response = "mean_temp")
forecast_moderate_fable_gulf <- as_fable(forecast_moderate_gulf, response = "mean_temp")
forecast_low_fable_gulf <- as_fable(forecast_low_gulf, response = "mean_temp")
forecast_stable_fable_gulf <- as_fable(forecast_stable_gulf, response = "mean_temp")

# Plotting the forecasts
ggplot(merged_gulf_df, aes(x = month_year, y = mean_temp)) +
  geom_line(aes(color = "Actual Data")) +
  geom_line(data = forecast_high_fable_gulf, aes(y = .mean, color = "High Emission")) +
  geom_line(data = forecast_moderate_fable_gulf, aes(y = .mean, color = "Moderate Emission")) +
  geom_line(data = forecast_low_fable_gulf, aes(y = .mean, color = "Low Emission")) +
  geom_line(data = forecast_stable_fable_gulf, aes(y = .mean, color = "Stable Emission")) +
  labs(title = "Forecasted Mean Temperature Based on Different CO2 Emission Scenarios",
       x = "Time", y = "Mean Temperature") +
  scale_color_manual(values = c("Actual Data" = "black", "High Emission" = "red", "Moderate Emission" = "orange", "Low Emission" = "lightgreen", "Stable Emission" = "blue"),
                     name = "Scenario") +
  theme_minimal() +
  guides(color = guide_legend(title = "Data Type"))

