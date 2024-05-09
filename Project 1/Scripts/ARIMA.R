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
  mutate(Month = yearmonth(paste(Jahr, Monat, sep = "-"))) |>
  as_tsibble(index = Month)

#### ARIMA Forecast ####

# m = 12 for monthly data
seasonality_period <- 12

# Optional: Automatic model selection using auto.arima equivalent
best_fit <- ts_vaud |>
  model(ARIMA(value, stepwise = TRUE, approximation = FALSE, trace = TRUE))

print(best_fit)

# Forecast using the best fit
best_forecasts <- best_fit |>
  forecast(h = 15)

# Plot the best forecasts
autoplot(best_forecasts) +
  labs(title = "Best Fit Seasonal ARIMA Forecast", x = "Year", y = "Value") +
  theme_minimal()