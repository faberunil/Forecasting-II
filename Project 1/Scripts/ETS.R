# Load the packages
library(here)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(fpp3)
library(fable)
library(fabletools)

# Read the data using a path relative to the project root including the 'Project 2' folder
data <- read_excel(here("Project 1", "Data", "Dataset_tourism.xlsx"))


##############
#### Vaud ####
##############
##############
## ETS AAA ###
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

#### ETS Forecast

# Fitting the ETS model
vaud_fit <- ts_vaud |> model(ETS(value))
ANA_vaud_fit <- ts_vaud |> model(ETS(value ~ error("A") + trend("N") + season("A")))

report(ANA_vaud_fit)

# Plot the forecast for the next 15 months
ANA_vaud_fit |> forecast(h = 15) |> autoplot(ts_vaud)

ANA_vaud_forecast <- ANA_vaud_fit |> forecast(h = 15)
# Print the forecasted values
print(ANA_vaud_forecast)

# Optionally, if you want to see a detailed DataFrame of the forecast:
ANA_vaud_df <- as.data.frame(ANA_vaud_fit)
print(ANA_vaud_df)

# Plot the forecast for visual inspection
autoplot(ts_vaud) +
  labs(title = "15-Month Forecast for Vaud",
       x = "Month",
       y = "Predicted Values") +
  theme_minimal()

# Residuals of the model
vaud_fit |> gg_tsresiduals(lag_max = "3 years")

# Check the accuracy of the model
accuracy(AAA_vaud_fit)

check_residuals(AAA_vaud_fit)

#################
#ETS Grid Search#
#################
# Assuming 'ts_vaud' is your tsibble prepared as before
# Specify the combinations of error, trend, and seasonal components you want to test
components <- expand.grid(error = c("A", "M"), 
                          trend = c("A", "M", "N"), 
                          season = c("A", "M", "N"))

# Convert the grid components explicitly to characters
components$error <- as.character(components$error)
components$trend <- as.character(components$trend)
components$season <- as.character(components$season)

# Fit models for each combination using pmap
models <- components |>
  mutate(model = pmap(list(error, trend, season), function(e, t, s) {
    cat("Fitting model with error:", e, "trend:", t, "season:", s, "\n")  # Debug output
    ts_vaud |> 
      model(ETS(value ~ error(e) + trend(t) + season(s)))
  }))

# Extract and bind the model summaries (including AIC for comparison)
model_summaries <- models |>
  mutate(summary = map(model, report)) |>
  unnest(summary)

model_summaries <- models |>
  mutate(aic = map_dbl(model, ~ glance(.x)$AIC))

# Assuming 'models' contains the ETS models and 'model_summaries' includes AIC values
# Re-extract best model using direct indexing for clarity
best_model_index <- which.min(model_summaries$aic)  # Find index of the minimum AIC
best_model <- model_summaries$model[[best_model_index]]  # Extract model using the index

# Print the best model
print(best_model)
# Now forecast using the best model
best_forecast <- forecast(best_model, h = 12)# Forecast with the best model
best_forecast <- best_model |>
  forecast(h = 12)

# Plot the forecast
autoplot(best_forecast) +
  labs(title = "Best ETS Model Forecast for Vaud", x = "Month", y = "Predicted Values") +
  theme_minimal()
