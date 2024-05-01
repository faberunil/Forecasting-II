# Load the packages
library(here)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

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

#### ETS Forecast

# Fitting the ETS model
vaud_fit <- ts_vaud |> model(ETS(value))
AAA_vaud_fit <- ts_vaud |> model(ETS(value ~ error("A") + trend("A") + season("A")))

report(AAA_vaud_fit)

# Plot the forecast for the next N year
AAA_vaud_fit |> forecast(h = 12) |> autoplot(ts_vaud)

# Residuals of the model
vaud_fit |> gg_tsresiduals(lag_max = "3 years")
