# Load the packages
library(here)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(fpp3)
library(purrr)
library(zoo)
library(tsibble)
library(fabletools)
library(forecast)
library(lubridate)

# Read the data using a path relative to the project root including the 'Project 2' folder
data <- read_excel(here("Project 1", "Data", "Dataset_tourism.xlsx"))

currency <- read.csv(here("Project 1", "Data", "CHF_JPY_Data.csv"))

# Clean currency data
# Only Price and Date columns
currency <- currency |>
  select(ï..Date, Price) |>
  mutate(ï..Date = as.Date(ï..Date, format = "%m/%d/%Y")) |>
  rename(JPY = Price) |>
  rename(Date = ï..Date)

# Assuming currency$Date is already a Date class object
currency$Date <- as.yearmon(currency$Date)
currency$Date <- yearmonth(currency$Date)

# Convert the data frame to a tsibble
currency_ts <- as_tsibble(currency, index = Date)

##############
#### Luzern ####
##############
# Create a tsibble
month_conversion <- c("Januar" = "1", "Februar" = "2", "März" = "3", "April" = "4", 
                      "Mai" = "5", "Juni" = "6", "Juli" = "7", "August" = "8",
                      "September" = "9", "Oktober" = "10", "November" = "11", "Dezember" = "12")

# Filter data for Vaud
Luzern_data <- data |>
  filter(Kanton == "Luzern", Herkunftsland == "Japan")

ts_luz <- Luzern_data |>
  mutate(Monat = month_conversion[Monat]) |> # Convert month names to numbers
  mutate(Date = yearmonth(paste(Jahr, Monat, sep = "-"))) |>
  as_tsibble(index = Date)

#### TSLM Forecast ####
# Fit an ARIMA model to the currency data
currency_model <- model(currency_ts, ARIMA = ARIMA(JPY))

# Forecast the next 7 months
currency_forecast <- forecast(currency_model, h = 7)

# Combine historical and forecasted currency data
full_currency_forecast <- bind_rows(currency_ts, as_tibble(currency_forecast))

# change JPY col from dist to numeric
# Take the median from the distribution
full_currency_forecast$JPY <- median(full_currency_forecast$JPY)

# filter currency data to only include same dates as ts_luz
currency_filts <- currency_ts |>
  filter(Date %in% ts_luz$Date)

# filter currency data to only include same dates as ts_luz
currency_filts <- currency_ts |>
  filter(Date %in% ts_luz$Date)

# Combining and cleaning data
ts_combined <- ts_luz %>%
  left_join(currency_filts, by = "Date") %>%
  as_tsibble(index = Date)
"
# Determine start time for the ts object
start_year <- year(min(ts_combined$Date))
start_month <- month(min(ts_combined$Date))

# Since 'ts' object needs numeric vector, extracting the 'value' and 'JPY' assuming they are aligned properly
value_ts <- ts(ts_combined$value, start = c(start_year, start_month), frequency = 12)
JPY_ts <- ts(ts_combined$JPY, start = c(start_year, start_month), frequency = 12)

# Calculate the necessary length for the entire series
total_length <- length(value_ts)  # This assumes value_ts is correctly calculated

# Count the months from the start of value_ts to the start and end of COVID period
start_covid <- 12 * (2020 - start_year) + (2 - start_month) + 1  # Start of COVID-19 impact
end_covid <- 12 * (2021 - start_year) + (3 - start_month) + 1  # End of COVID-19 impact

# Create the covid dummy ensuring it aligns exactly with the main ts
covid <- ts(c(rep(0, max(0, start_covid - 1)), 
              rep(1, max(0, end_covid - start_covid + 1)), 
              rep(0, total_length - end_covid)),
            start = c(start_year, start_month), frequency = 12)
"

# Fit a TSLM model
model_fit <- tslm(value ~ JPY + trend + season, data = ts_combined)

# Check model summary and diagnostics
summary(model_fit)
accuracy(model_fit)

# create a tsibble with all JPY values and all known value values, if there arent known values, fill with NA
ts_combined_full <- full_currency_forecast |>
  left_join(ts_luz, by = "Date") |>
  select(Date, value, JPY) |>
  as_tsibble(index = Date)

# add the covid dummy to the ts_combined_full and fill in values all unknown are filled with 0
ts_combined_full <- ts_combined_full |>
  mutate(covid = 0)

# reorder so JPY is second after date which is the index
ts_combined_full <- ts_combined_full |>
  select(Date, JPY, value, covid)

# remove first data point
ts_combined_full <- ts_combined_full[-1,]

# Ensure the Date column is treated as a Date object
ts_combined_full <- ts_combined_full %>%
  mutate(Date = as.Date(Date))

# predict the values for the unknown values
predictions <- forecast(model_fit, new_data = ts_combined_full)

# Plot the results
autoplot(ts_combined_full, value) +
  autolayer(ts_combined_full, .mean, series = "Prediction") +
  autolayer(ts_combined_full, .lower, .upper, series = "95% Prediction Interval") +
  xlab("Year") +
  ylab("Number of Overnight Stays") +
  ggtitle("TSLM Forecast for Luzern") +
  guides(colour = guide_legend(title = "Legend")) +
  theme(legend.position = "bottom")

# Model summary and diagnostics
summary(model_fit)
accuracy(model_fit)

# AIC can be a useful measure if applicable in your modeling context
AIC(model_fit)