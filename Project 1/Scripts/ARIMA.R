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
library(readr)


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
covidforecasts_vd |> autoplot(ts_vaud) +
  labs(title = "ARIMA Forecast for Vaud with Covid Impact", x = "Year", y = "Value") +
  theme_minimal()

### SAVE CHOOSEN MODEL IN CSV ###

sigma2 <- glance(Covid_fitvd) %>% pull(sigma2)

covidforecasts_vd <- covidforecasts_vd |>
  mutate(
    Lower_interval = .mean - 1.96*sqrt(sigma2),
    Upper_interval = .mean + 1.96*sqrt(sigma2)
  )

# turn covidforecasts_vd into a dataframe
covidforecasts_vd <- as.data.frame(covidforecasts_vd)

# take only the date, .mean, Lower_interval, Upper_interval columns (also rename .mean to value)
covidforecasts_vd_csv <- covidforecasts_vd |>
  select(Date, Forecast_Value = .mean, Lower_interval, Upper_interval)

# Export forecast as a CSV file
write_csv(covidforecasts_vd_csv, here("Project 1", "Data", "forecasts_vd.csv"))

##################################################################################
#### Luzern ######################################################################
##################################################################################

# Filter data
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
best_forecasts_lz |> autoplot(ts_luz) +
  labs(title = "Best Fit Seasonal ARIMA Forecast Luzern", x = "Year", y = "Value") +
  theme_minimal()

##############
#### Arima with JPY  ####
##############
currency <- read.csv(here("Project 1", "Data", "CHF_JPY_Data.csv"))

# Clean currency data
# Only Price and Date columns
currency <- currency |>
  select(Date, Price) |>
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) |>
  rename(JPY = Price)

# Assuming currency$Date is already a Date class object
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
forecasts |> autoplot(ts_luz) +
  labs(title = "ARIMA Forecast with Currency Data", x = "Date", y = "Value") +
  theme_minimal()

##############
#### Arima with covid + JPY ####
##############
# Create covid dummy for ts_combined
Covidts_combined <- ts_combined |>
  mutate(covid = if_else(Date >= as.Date("2020-03-01") & Date <= as.Date("2021-03-31"), 1, 0))

# Create the COVID-19 dummy for the future dates
Covidfuture_dates <- future_dates |>
  mutate(covid = if_else(Date >= as.Date("2020-03-01") & Date <= as.Date("2021-03-31"), 1, 0))

# Fit the ARIMA model with the COVID dummy variable
JPYCovid_fit <- Covidts_combined |>
  model(
    ARIMA(value ~ JPY + covid + pdq(1, 0, 0) + PDQ(2, 1, 1))
  )

# Forecast using the model with new data that includes the COVID dummy
JPYCovidforecasts <- JPYCovid_fit |>
  forecast(new_data = Covidfuture_dates)

# Plotting the forecast with the historical data and COVID adjustments
JPYCovidforecasts |> autoplot(ts_luz) +
  labs(title = "ARIMA Forecast with Currency Data and COVID Impact", x = "Date", y = "Value") +
  theme_minimal()

### SAVE CHOOSEN MODEL IN CSV ###

sigma2jp <- glance(JPYCovid_fit) %>% pull(sigma2)

JPYCovidforecasts <- JPYCovidforecasts |>
  mutate(
    Lower_interval = .mean - 1.96*sqrt(sigma2jp),
    Upper_interval = .mean + 1.96*sqrt(sigma2jp)
  )

# turn covidforecasts_vd into a dataframe
JPYCovidforecasts <- as.data.frame(JPYCovidforecasts)

# take only the date, .mean, Lower_interval, Upper_interval columns (also rename .mean to value)
JPYCovidforecasts_csv <- JPYCovidforecasts |>
  select(Date, Forecast_Value = .mean, Lower_interval, Upper_interval)

# set value under 0 to 0
JPYCovidforecasts_csv <- JPYCovidforecasts_csv |>
  mutate(across(where(is.numeric), ~if_else(. < 0, 0, .)))

# Export forecast as a CSV file
write_csv(JPYCovidforecasts_csv, here("Project 1", "Data", "forecasts_jplz.csv"))

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

# Calculate AIC, ME, and MASE for the best ARIMA model for Vaud
summary_vd <- glance(best_fit_vd)
accuracy_vd <- accuracy(best_fit_vd)
aic_vd <- summary_vd$AIC
mae_vd <- accuracy_vd$MAE
mase_vd <- accuracy_vd$MASE

# Calculate AIC, ME, and MASE for the COVID ARIMA model for Vaud
summary_covid_vd <- glance(Covid_fitvd)
accuracy_covid_vd <- accuracy(Covid_fitvd)
aic_covid_vd <- summary_covid_vd$AIC
mae_covid_vd <- accuracy_covid_vd$MAE
mase_covid_vd <- accuracy_covid_vd$MASE

# Calculate AIC, ME, and MASE for the best ARIMA model for Luzern
summary_lz <- glance(best_fit_lz)
accuracy_lz <- accuracy(best_fit_lz)
aic_lz <- summary_lz$AIC
mae_lz <- accuracy_lz$MAE
mase_lz <- accuracy_lz$MASE

# Calculate AIC, ME, and MASE for the ARIMA model with external regressor for Luzern
summary_JPY_fit <- glance(JPY_fit)
accuracy_JPY <- accuracy(JPY_fit)
aic_JPY <- summary_JPY_fit$AIC
mae_JPY <- accuracy_JPY$MAE
mase_JPY <- accuracy_JPY$MASE

# Calculate AIC, ME, and MASE for the COVID ARIMA model with external regressor for Luzern
summary_JPYCovid_fit <- glance(JPYCovid_fit)
accuracy_JPYCovid <- accuracy(JPYCovid_fit)
aic_JPYCovid <- summary_JPYCovid_fit$AIC
mae_JPYCovid <- accuracy_JPYCovid$MAE
mase_JPYCovid <- accuracy_JPYCovid$MASE

# Print all metrics
metricsarima_df <- data.frame(
  Model = c("Arima Vaud", "COVID Arima Vaud", "Arima Luzern", "JPY Arima Luzern", "JPY COVID Arima Luzern"),
  AIC = c(aic_vd, aic_covid_vd, aic_lz, aic_JPY, aic_JPYCovid),
  MAE = c(mae_vd, mae_covid_vd, mae_lz, mae_JPY, mae_JPYCovid),
  MASE = c(mase_vd, mase_covid_vd, mase_lz, mase_JPY, mase_JPYCovid)
)

print(metricsarima_df)

save(metricsarima_df, file = "Project 1/Data/metricsarima_df.rdata")
