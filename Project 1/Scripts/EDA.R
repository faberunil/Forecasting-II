# Load the packages
library(here)
library(readxl)
library(dplyr)
library(ggplot2)

# Read the data using a path relative to the project root including the 'Project 2' folder
data <- read_excel(here("Project 1", "Data", "Dataset_tourism.xlsx"))

##############
#### Vaud ####
##############

# Filter data for Vaud
vaud_data <- data |>
  filter(Kanton == "Vaud", Herkunftsland == "Herkunftsland - Total")

# Convert Monat and Jahr to a Date format for time series analysis
vaud_data <- vaud_data |>
  mutate(Date = as.Date(paste(Jahr, Monat, "1", sep="-"), "%Y-%B-%d")) |>
  arrange(Date)  # Ensure data is in chronological order

# Plotting the data to visualize trends and seasonality
ggplot(vaud_data, aes(x = Date, y = value)) +
  geom_line() +
  labs(title = "Monthly Overnight Stays in Vaud",
       x = "Date",
       y = "Number of Overnight Stays")

# Calculate the distribution

################
#### Luzern ####
################

# Filter data for Luzern
luzern_data <- data |>
  filter(Kanton == "Luzern", Herkunftsland == "Japan")

# Convert Monat and Jahr to a Date format for time series analysis
luzern_data <- luzern_data |>
  mutate(Date = as.Date(paste(Jahr, Monat, "1", sep="-"), "%Y-%B-%d")) |>
  arrange(Date)  # Ensure data is in chronological order

# Plotting the data to visualize trends and seasonality
ggplot(luzern_data, aes(x = Date, y = value)) +
  geom_line() +
  labs(title = "Monthly Overnight Stays in Luzern by Japanese Tourists",
       x = "Date",
       y = "Number of Overnight Stays")

# Calculate the distribution
