### Forecasting 2 - Project 2 ###
#################################

library(here)
library(dplyr)
library(ggplot2)
library(zoo)
library(lubridate)
library(forecast)
library(fpp3)

### Atlantic - TSLM Forecast ####
#################################

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
co2_data <- read.csv(here("Project 2", "Data", "ts_C02.csv"), header = TRUE)

co2_data <- co2_data |>
  group_by(Year, Month) |>
  mutate(month_year = yearmonth(paste(Year, Month, sep = "-")))

# Create a tsibble object
ts_co2 <- as_tsibble(co2_data, index = month_year)

# Convert column names to lowercase for consistency
names(ts_atl) <- tolower(names(ts_atl))
names(ts_co2) <- tolower(names(ts_co2))

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

# Calculate percentage changes
merged_df <- merged_df %>%
  arrange(date) %>%
  mutate(
    perc_change_temp = 100 * (mean_temp - lag(mean_temp, n = 1)) / lag(mean_temp, n = 1),
    perc_change_co2 = 100 * (mean_c02 - lag(mean_c02, n = 1)) / lag(mean_c02, n = 1)
  )
### Correlation ###
###################

correlation <- cor(merged_df$mean_temp, merged_df$mean_c02, use = "complete.obs")  # Use complete.obs to handle missing values
correlation

### TSLM FORECAST ###
#####################

# Fit a TSLM with CO2 as a predictor
tslm_model <- ts_merged |> model(TSLM(mean_temp ~ mean_c02))
report(tslm_model)

# Generate new data
co2_new_data <- new_data(ts_merged, n = 120) %>%
  mutate(mean_c02 = rep(last(ts_merged$mean_c02) * 1.05, each = 120))

# Forecast using the tslm_model
forecasted_values <- forecast(tslm_model, new_data = co2_new_data) %>%
  as_fable(response = "mean_temp")

# Plot the original data along with the forecast
autoplot(ts_merged, mean_temp) +
  autolayer(forecasted_values, mean_temp, series = "Forecasted") +
  labs(title = "Forecasted Mean Temperature Based on CO2 Levels",
       x = "Time", y = "Mean Temperature") +
  theme_minimal()

####################################################################################################################
####################################################################################################################

### EDA - CO2 emisssions and Sea Temperature ###
################################################

### Plot of evolution of Temperature and C02 overtime ##

# Creating a date column for plotting
merged_df$date <- as.Date(paste(merged_df$year_temp, merged_df$month_temp, "01", sep = "-"), "%Y-%m-%d")

# Plotting
ggplot(merged_df, aes(x = date)) +
  geom_line(aes(y = mean_temp, color = "Temperature"), size = 1) +
  geom_line(aes(y = mean_c02, color = "CO2"), size = 1) +
  scale_y_continuous(
    name = "Mean Temperature (°C)",
    sec.axis = sec_axis(~ ., name = "Mean CO2 Levels (ppm)")
  ) +
  labs(title = "Evolution of Sea Temperature (Atlantic) and CO2 over Time",
       x = "Time") +
  scale_color_manual(values = c("Temperature" = "blue", "CO2" = "red")) +
  theme_minimal()


# Plotting
ggplot(merged_df, aes(x = date)) +
  geom_line(aes(y = perc_change_temp, color = "Percentage Change in Temperature"), size = 1) +
  geom_line(aes(y = perc_change_co2, color = "Percentage Change in CO2"), size = 1) +
  labs(title = "Percentage Change in Temperature and CO2 Levels Over Time",
       x = "Time", y = "Percentage Change (%)") +
  scale_color_manual(values = c("Percentage Change in Temperature" = "blue", "Percentage Change in CO2" = "red")) +
  theme_minimal()

### Seasonality ###

# Extract month from date for seasonal decomposition
merged_df$month <- month(merged_df$date, label = TRUE, abbr = FALSE)

# Calculate average values per month across all years
monthly_averages <- merged_df %>%
  group_by(month) %>%
  summarize(average_temp = mean(mean_temp), average_co2 = mean(mean_c02), .groups = 'drop')

# Convert month to a factor for proper ordering in the plot
monthly_averages$month <- factor(monthly_averages$month, levels = month.name)

# Plotting
ggplot(data = monthly_averages, aes(x = month)) +
  geom_line(aes(y = average_temp, color = "Average Temperature", group = 1), size = 1) +
  geom_line(aes(y = average_co2, color = "Average CO2", group = 1), size = 1) +
  scale_y_continuous(
    name = "Average Temperature (°C)",
    sec.axis = sec_axis(~ ., name = "Average CO2 Levels (ppm)")
  ) +
  labs(title = "Seasonal Patterns in Temperature and CO2 Levels",
       x = "Month", y = "Average Measurement") +
  scale_color_manual(values = c("Average Temperature" = "blue", "Average CO2" = "red")) +
  theme_minimal()


### Monthly Comparaison ###

# Extract month and year from date for plotting
merged_df$month <- month(merged_df$date, label = TRUE, abbr = TRUE)
merged_df$year <- year(merged_df$date)

# Plotting
ggplot(merged_df, aes(x = month, group = year)) +
  geom_line(aes(y = mean_temp, color = as.factor(year)), size = 1, alpha = 0.6) + 
  geom_line(aes(y = mean_c02, color = as.factor(year)), size = 1, alpha = 0.6) +
  scale_color_manual(values = rainbow(length(unique(merged_df$year)))) +
  facet_wrap(~ year, scales = "free_y") +
  labs(title = "Monthly Evolution of Temperature and CO2 Levels Over Years",
       x = "Month", y = "Measurement") +
  theme_minimal() +
  theme(legend.position = "none")


### Yearly Comparaison ###

# Extract month and year from date for plotting
merged_df$month <- month(merged_df$date, label = TRUE, abbr = TRUE)
merged_df$year <- as.factor(year(merged_df$date))

# Calculate averages across all years for the same month to set y-limits
average_by_month <- merged_df %>%
  group_by(month) %>%
  summarize(avg_temp = mean(mean_temp), avg_co2 = mean(mean_c02))

# Set y-axis limits based on min/max of average temperature and CO2
ylim_temp <- range(average_by_month$avg_temp)
ylim_co2 <- range(average_by_month$avg_co2)

# Plotting
ggplot(merged_df, aes(x = month)) +
  geom_line(aes(y = mean_temp, color = year, group = interaction(year, "temp")), size = 1) +
  geom_line(aes(y = mean_c02, color = year, group = interaction(year, "co2")), size = 1, linetype = "dashed") +
  scale_color_manual(values = rainbow(length(levels(merged_df$year)))) +
  scale_y_continuous(name = "Temperature (°C) and CO2 Levels (ppm)",
                     limits = c(min(ylim_temp[1], ylim_co2[1]), max(ylim_temp[2], ylim_co2[2]))) +
  labs(title = "Yearly Comparison of Monthly Temperature and CO2",
       x = "Month", y = "Measurement") +
  theme_minimal() +
  theme(legend.position = "bottom")



### Monthly Comparison of Temperature and C02 Levels Across Years ###

# Merge datasets based on 'month_year' column
merged_df <- merge(temperature_df, co2_df, by = "month_year", suffixes = c("_temp", "_co2"))

# Create a Date column for easier manipulation
merged_df$date <- as.Date(paste(merged_df$year_temp, merged_df$month_temp, "01", sep = "-"), "%Y-%m-%d")

# Extract month and year for plotting
merged_df$month <- month(merged_df$date, label = TRUE, abbr = TRUE)
merged_df$year <- year(merged_df$date)

# Plotting with dual axes
p <- ggplot(merged_df, aes(x = month)) +
  geom_line(aes(y = mean_temp, group = year, color = as.factor(year)), size = 1) +
  scale_y_continuous(name = "Mean Temperature (°C)", 
                     sec.axis = sec_axis(~ . * 1, name = "Mean CO2 Levels (ppm)")) +
  scale_color_viridis_d() +
  labs(title = "Monthly Comparison of Temperature and CO2 Levels Across Years",
       x = "Month", y = "Temperature (°C)") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Smoothing lines to show trends
p + geom_smooth(aes(y = mean_temp), method = "loess", se = FALSE, color = "darkgray", size = 2) +
  geom_smooth(aes(y = mean_c02), method = "loess", se = FALSE, color = "black", size = 2)

### Correlation ###

correlation <- cor(merged_df$mean_temp, merged_df$mean_c02, use = "complete.obs")  # Use complete.obs to handle missing values

print(paste("Correlation coefficient between temperature and CO2: ", correlation))


### TSLM FORECAST ###
#####################

# Ensure the date column is formatted correctly
merged_df$date <- as.Date(merged_df$date)

# Create time series objects for temperature and CO2
start_year <- min(format(merged_df$date, "%Y"))
start_month <- min(format(merged_df$date, "%m"))

ts_atl <- ts(merged_df$mean_temp, start = c(start_year, start_month), frequency = 12)
ts_co2 <- ts(merged_df$mean_c02, start = c(start_year, start_month), frequency = 12)

# Check the alignment and length
print(paste("Length of temp_ts:", length(ts_atl)))
print(paste("Length of co2_ts:", length(ts_co2)))

# Fit a TSLM with CO2 as a predictor
tslm_model <- tslm(ts_atl ~ ts_co2)

# Check the summary of the model
summary(tslm_model)

# Calculate average yearly increase from historical data
co2_increase_per_month <- mean(diff(merged_df$mean_c02))

# Last known CO2 level
last_co2 <- tail(merged_df$mean_c02, 1)

# Create a future CO2 data frame for the next 12 months
future_co2 <- data.frame(co2_ts = seq(last_co2, by = co2_increase_per_month, length.out = 12))

# Display future CO2 levels
print(future_co2)

# Forecast using the new CO2 levels
forecast_tslm <- forecast(tslm_model, newdata = future_co2)

# Plot the forecast
plot(forecast_tslm, main = "Forecast of Temperature Based on Projected CO2 Levels", xlab = "Months", ylab = "Temperature")






