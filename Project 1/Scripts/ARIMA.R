# Load the packages
library(here)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(fpp3)
library(fable)
library(purrr)
library(fabletools)


# Read the data using a path relative to the project root including the 'Project 2' folder
data <- read_excel(here("Project 1", "Data", "Dataset_tourism.xlsx"))


##############
#### Vaud ####
##############

# Filter data for Vaud
vaud_data <- data |>
  filter(Kanton == "Vaud", Herkunftsland == "Herkunftsland - Total")

# Create a tsibble
month_conversion <- c("Januar" = "1", "Februar" = "2", "März" = "3", "April" = "4", 
                      "Mai" = "5", "Juni" = "6", "Juli" = "7", "August" = "8",
                      "September" = "9", "Oktober" = "10", "November" = "11", "Dezember" = "12")

ts_vaud <- vaud_data |>
  mutate(Monat = month_conversion[Monat]) |> # Convert month names to numbers
  mutate(Date = yearmonth(paste(Jahr, Monat, sep = "-"))) |>
  as_tsibble(index = Date)

#### ARIMA Forecast ####
# Optional: Automatic model selection using auto.arima equivalent
best_fit_vd <- ts_vaud |>
  model(ARIMA(value, stepwise = TRUE, approximation = FALSE, trace = TRUE))

print(best_fit_vd)
accuracy(best_fit_vd)

# Forecast using the best fit
best_forecasts_vd <- best_fit_vd |>
  forecast(h = 15)

# Plot the best forecasts
autoplot(best_forecasts_vd) +
  labs(title = "Best Fit Seasonal ARIMA Forecast Vaud", x = "Year", y = "Value") +
  theme_minimal()

##############
#### Arima with covid ####
##############
Covidts_vd <- ts_vaud |>
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"),  # Ensure Date is correctly formatted
         covid = if_else(Date >= as.Date("2020-03-01") & Date <= as.Date("2021-03-31"), 1, 0))

Covidts_vd$Date <- yearmonth(Covidts_vd$Date)
Covidts_vd <- as_tsibble(Covidts_vd, index = Date)

# Fit the ARIMA model with the COVID dummy variable
Covid_fitvd <- Covidts_vd |>
  model(
    ARIMA(value ~ covid + pdq(1, 0, 1) + PDQ(2, 1, 2))
  )

# Assuming future_dates is already defined and needs the COVID dummy
future_datesvd <- new_data(Covidts_vd, n = 15) |>
  mutate(covid = 0)  # Explicitly setting COVID dummy to 0 for the forecast period

# Forecast using the best fit
covidforecasts_vd <- Covid_fitvd |>
  forecast(new_data = future_datesvd)

# Plot the best forecasts
autoplot(covidforecasts_vd) +
  labs(title = "Best Fit Seasonal ARIMA Forecast Vaud", x = "Year", y = "Value") +
  theme_minimal()

##################################################################################
#### Luzern ######################################################################
##################################################################################

# Filter data for Vaud
Luzern_data <- data |>
  filter(Kanton == "Luzern", Herkunftsland == "Japan")

ts_luz <- Luzern_data |>
  mutate(Monat = month_conversion[Monat]) |> # Convert month names to numbers
  mutate(Date = yearmonth(paste(Jahr, Monat, sep = "-"))) |>
  as_tsibble(index = Date)

# Optional: Automatic model selection using auto.arima equivalent
best_fit_lz <- ts_luz |>
  model(ARIMA(value, stepwise = TRUE, approximation = FALSE, trace = TRUE))

print(best_fit_lz)
accuracy(best_fit_lz)

# Forecast using the best fit
best_forecasts_lz <- best_fit_lz |>
  forecast(h = 15)

# Plot the best forecasts
autoplot(best_forecasts_lz) +
  labs(title = "Best Fit Seasonal ARIMA Forecast Luzern", x = "Year", y = "Value") +
  theme_minimal()

##############
#### Arima with x variable ####
##############
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

# filter currency data to only include same dates as ts_luz
currency_filts <- currency_ts |>
  filter(Date %in% ts_luz$Date)

# Combining and cleaning data
ts_combined <- ts_luz |>
  left_join(currency_filts, by = "Date") |>
  as_tsibble(index = Date)

# Fit ARIMA model with external regressor included directly in the model
JPY_fit <- ts_combined |>
  model(
    ARIMA(value ~ JPY + pdq(1, 0, 0) + PDQ(2, 1, 1))
  )

# Simplifying the forecast and preparation process
currency_forecast <- currency_ts |>
  model(ARIMA(JPY)) |>
  forecast(h = 7)

# Combine, ensure median is calculated correctly
full_currency_forecast <- bind_rows(currency_ts, as_tibble(currency_forecast)) |>
  mutate(
    JPY = median(JPY, na.rm = TRUE),
  )

# Preparing future data for forecasting
future_dates <- new_data(ts_combined, n = 15) |>
  left_join(full_currency_forecast |> select(Date, JPY), by = "Date") |>
  as_tsibble(index = Date)

# Final forecasting and plotting
forecasts <- JPY_fit |>
  forecast(new_data = future_dates)

# Efficient plotting
autoplot(forecasts) +
  labs(title = "ARIMA Forecast with Currency Data", x = "Date", y = "Value") +
  theme_minimal()
##############
#### Arima with covid ####
##############
# Create covid dummy for ts_combined
Covidts_combined <- ts_combined |>
  mutate(covid = if_else(Date >= as.Date("2020-03-01") & Date <= as.Date("2021-03-31"), 1, 0))

# Create the COVID-19 dummy for the future dates
Covidfuture_dates <- future_dates |>
  mutate(covid = if_else(Date >= as.Date("2020-03-01") & Date <= as.Date("2021-03-31"), 1, 0))

# Step 3: Fit the ARIMA model with the COVID dummy variable
JPYCovid_fit <- Covidts_combined |>
  model(
    ARIMA(value ~ JPY + covid + pdq(1, 0, 0) + PDQ(2, 1, 1))
  )

# Forecast using the model with new data that includes the COVID dummy
JPYCovidforecasts <- JPYCovid_fit |>
  forecast(new_data = Covidfuture_dates)

# Plotting the forecast with the historical data and COVID adjustments
autoplot(JPYCovidforecasts) +
  labs(title = "ARIMA Forecast with Currency Data and COVID Impact", x = "Date", y = "Value") +
  theme_minimal()

##############
#### Arima with Covid ####
##############
Covid_fit <- Covidts_combined |>
  model(
    ARIMA(value ~ covid + pdq(1, 0, 0) + PDQ(2, 1, 1))
  )

# Forecast using the model with new data that includes the COVID dummy
Covidforecasts <- Covid_fit |>
  forecast(new_data = Covidfuture_dates)

# Plotting the forecast with the historical data and COVID adjustments
autoplot(Covidforecasts) +
  labs(title = "ARIMA Forecast with COVID Impact", x = "Date", y = "Value") +
  theme_minimal()

##################################################################################
#### metrics ######################################################################
##################################################################################

AIC <- c(best_fit_vd$aic, Covid_fitvd$aic, best_fit_lz$aic, JPY_fit$aic, JPYCovid_fit$aic, Covid_fit$aic)
print(AIC)
