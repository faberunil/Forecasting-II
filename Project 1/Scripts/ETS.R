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
## ETS ANA (best)###
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
accuracy(ANA_vaud_fit)

summary_ANA_vaud <- glance(ANA_vaud_fit)
print(summary_ANA_vaud)

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
models_vd <- components |>
  mutate(model = pmap(list(error, trend, season), function(e, t, s) {
    cat("Fitting model with error:", e, "trend:", t, "season:", s, "\n")  # Debug output
    ts_vaud |> 
      model(ETS(value ~ error(e) + trend(t) + season(s)))
  }))

# Extract and bind the model summaries (including AIC for comparison)
model_summaries_vd <- models_vd |>
  mutate(summary = map(model, report)) |>
  unnest(summary)

model_summaries_vd <- models_vd |>
  mutate(aic = map_dbl(model, ~ glance(.x)$AIC))

# Assuming 'models' contains the ETS models and 'model_summaries' includes AIC values
# Re-extract best model using direct indexing for clarity
best_model_index_vd <- which.min(model_summaries_vd$aic)  # Find index of the minimum AIC
best_model_vd <- model_summaries_vd$model[[best_model_index_vd]]  # Extract model using the index

# Print the best model
print(best_model_vd)

# Now forecast using the best model
best_forecast_vd <- forecast(best_model_vd, h = 12)# Forecast with the best model
best_forecast_vd <- best_model_vd |>
  forecast(h = 12)

# Plot the forecast
autoplot(best_forecast_vd) +
  labs(title = "Best ETS Model Forecast for Vaud", x = "Month", y = "Predicted Values") +
  theme_minimal()

#################
#### Luzern ####
#################

# Filter data for Vaud
Luzern_data <- data |>
  filter(Kanton == "Luzern", Herkunftsland == "Japan")

ts_luz <- Luzern_data |>
  mutate(Monat = month_conversion[Monat]) |> # Convert month names to numbers
  mutate(Date = yearmonth(paste(Jahr, Monat, sep = "-"))) |>
  as_tsibble(index = Date)


#################
#### ETS MNM ####
#################

# Fitting the ETS model
lz_fit <- ts_luz |> model(ETS(value))
MNM_lz_fit <- ts_luz |> model(ETS(value ~ error("M") + trend("N") + season("M")))

report(MNM_lz_fit)

# Plot the forecast for the next 15 months
MNM_lz_fit |> forecast(h = 15) |> autoplot(ts_luz)

MNM_lz_forecast <- MNM_lz_fit |> forecast(h = 15)
# Print the forecasted values
print(MNM_lz_forecast)

# Optionally, if you want to see a detailed DataFrame of the forecast:
MNM_lz_df <- as.data.frame(MNM_lz_fit)
print(MNM_lz_df)

# Plot the forecast for visual inspection
autoplot(ts_luz) +
  labs(title = "15-Month Forecast for Luzern",
       x = "Month",
       y = "Predicted Values") +
  theme_minimal()

# Residuals of the model
lz_fit |> gg_tsresiduals(lag_max = "3 years")

# Check the accuracy of the model
accuracy(MNM_lz_fit)

##################################################################################
#### metrics ######################################################################
##################################################################################

# Calculate AIC, ME, and MASE for the best ETS model for Vaud
accuracy_vdETS <- accuracy(ANA_vaud_fit)
aic_vdETS <- summary_ANA_vaud$AIC
mae_vdETS <- accuracy_vdETS$MAE
mase_vdETS <- accuracy_vdETS$MASE

# Calculate AIC, ME, and MASE for the best ETS model for Luzern
summary_MNM_lz <- glance(MNM_lz_fit)
accuracy_lzETS <- accuracy(MNM_lz_fit)
aic_lzETS <- summary_MNM_lz$AIC
mae_lzETS <- accuracy_lzETS$MAE
mase_lzETS <- accuracy_lzETS$MASE


# Create a summary table for the metrics
metricsETS_df <- data.frame(
  Model = c("ETS Vaud", "ETS Luzern"),
  AIC = c(aic_vdETS, aic_lzETS),
  MAE = c(mae_vdETS, mae_lzETS),
  MASE = c(mase_vdETS, mase_lzETS)
)

print(metricsETS_df)

# save the metrics to a rdata df
save(metricsETS_df, file = "Project 1/Data/metricsETS_df.rdata")
