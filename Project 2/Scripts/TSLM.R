library(ggplot2)
library(dplyr)

# Read the datasets
temperature_df <- read.csv("ts_Temperature.csv")
co2_df <- read.csv("ts_C02.csv")

# Convert column names to lowercase for consistency
names(temperature_df) <- tolower(names(temperature_df))
names(co2_df) <- tolower(names(co2_df))

# Merge the datasets on the 'month_year' column
merged_df <- merge(temperature_df, co2_df, by = "month_year", suffixes = c("_temp", "_co2"))

# Linear regression model
model <- lm(mean_temp ~ mean_c02, data = merged_df)

# Summary of the model
summary(model)

# Plotting
ggplot(merged_df, aes(x = mean_c02, y = mean_temp)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Regression Analysis: Mean Temperature vs. Mean CO2 Levels",
       x = "Mean CO2 Levels", y = "Mean Temperature") +
  theme_minimal()

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





##Change over time###

library(ggplot2)
library(dplyr)
library(zoo)

# Read the datasets
temperature_df <- read.csv("ts_Temperature.csv")
co2_df <- read.csv("ts_C02.csv")

# Convert column names to lowercase for consistency
names(temperature_df) <- tolower(names(temperature_df))
names(co2_df) <- tolower(names(co2_df))

# Calculate percentage changes
merged_df <- merged_df %>%
  arrange(date) %>%
  mutate(
    perc_change_temp = 100 * (mean_temp - lag(mean_temp, n = 1)) / lag(mean_temp, n = 1),
    perc_change_co2 = 100 * (mean_c02 - lag(mean_c02, n = 1)) / lag(mean_c02, n = 1)
  )

# Drop NA values that result from diff calculation
merged_df <- na.omit(merged_df)

# Plotting
ggplot(merged_df, aes(x = date)) +
  geom_line(aes(y = perc_change_temp, color = "Percentage Change in Temperature"), size = 1) +
  geom_line(aes(y = perc_change_co2, color = "Percentage Change in CO2"), size = 1) +
  labs(title = "Percentage Change in Temperature and CO2 Levels Over Time",
       x = "Time", y = "Percentage Change (%)") +
  scale_color_manual(values = c("Percentage Change in Temperature" = "blue", "Percentage Change in CO2" = "red")) +
  theme_minimal()





##Seasonality##

library(ggplot2)
library(dplyr)
library(lubridate)

# Read the datasets
temperature_df <- read.csv("ts_Temperature.csv")
co2_df <- read.csv("ts_C02.csv")

# Convert column names to lowercase for consistency
names(temperature_df) <- tolower(names(temperature_df))
names(co2_df) <- tolower(names(co2_df))

# Create a date column for easier manipulation
merged_df$date <- as.Date(paste(merged_df$year_temp, merged_df$month_temp, "01", sep = "-"), "%Y-%m-%d")

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






###Monthly Comparaison###


library(ggplot2)
library(dplyr)
library(lubridate)

# Read the datasets
temperature_df <- read.csv("ts_Temperature.csv")
co2_df <- read.csv("ts_C02.csv")

# Convert column names to lowercase for consistency
names(temperature_df) <- tolower(names(temperature_df))
names(co2_df) <- tolower(names(co2_df))

# Create a date column for easier manipulation
merged_df$date <- as.Date(paste(merged_df$year_temp, merged_df$month_temp, "01", sep = "-"), "%Y-%m-%d")

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









###Yearly Comparaison###


library(ggplot2)
library(dplyr)
library(lubridate)

# Read the datasets
temperature_df <- read.csv("ts_Temperature.csv")
co2_df <- read.csv("ts_C02.csv")

# Convert column names to lowercase for consistency
names(temperature_df) <- tolower(names(temperature_df))
names(co2_df) <- tolower(names(co2_df))


# Create a date column for easier manipulation
merged_df$date <- as.Date(paste(merged_df$year_temp, merged_df$month_temp, "01", sep = "-"), "%Y-%m-%d")

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





###Monthly Comparison of Temperature and C02 Levels Across Years###

library(ggplot2)
library(dplyr)
library(lubridate)

# Load the datasets
temperature_df <- read.csv("ts_Temperature.csv")
co2_df <- read.csv("ts_C02.csv")

# Lowercase column names for consistency
names(temperature_df) <- tolower(names(temperature_df))
names(co2_df) <- tolower(names(co2_df))

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
# Assuming merged_df is your dataframe with 'mean_temp' and 'mean_c02'
correlation <- cor(merged_df$mean_temp, merged_df$mean_c02, use = "complete.obs")  # Use complete.obs to handle missing values

print(paste("Correlation coefficient between temperature and CO2: ", correlation))








###TSLM FORECAST###



library(forecast)
library(dplyr)

# Assuming you've already loaded and merged your data as 'merged_df'
# Ensure the date column is formatted correctly
merged_df$date <- as.Date(merged_df$date)

# Create time series objects for temperature and CO2
# Assuming monthly data starting from the earliest date in your dataset
start_year <- min(format(merged_df$date, "%Y"))
start_month <- min(format(merged_df$date, "%m"))

temp_ts <- ts(merged_df$mean_temp, start = c(start_year, start_month), frequency = 12)
co2_ts <- ts(merged_df$mean_c02, start = c(start_year, start_month), frequency = 12)

# Check the alignment and length
print(paste("Length of temp_ts:", length(temp_ts)))
print(paste("Length of co2_ts:", length(co2_ts)))


# Fit a TSLM with CO2 as a predictor
tslm_model <- tslm(temp_ts ~ co2_ts)

# Check the summary of the model
summary(tslm_model)

library(forecast)
library(dplyr)

# Example of estimating future CO2 levels
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


