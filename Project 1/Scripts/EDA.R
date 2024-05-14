# Load the packages
library(here)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tsibble)

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
  arrange(Date)

# Create a tsibble
month_conversion <- c("Januar" = "1", "Februar" = "2", "März" = "3", "April" = "4", 
                      "Mai" = "5", "Juni" = "6", "Juli" = "7", "August" = "8",
                      "September" = "9", "Oktober" = "10", "November" = "11", "Dezember" = "12")

ts_vaud <- vaud_data |>
  mutate(Monat = month_conversion[Monat]) |> # Convert month names to numbers
  mutate(Month = yearmonth(paste(Jahr, Monat, sep = "-"))) |>
  as_tsibble(index = Month)


# Plotting the data to visualize trends and seasonality
ggplot(ts_vaud, aes(x = Date, y = value)) +
  geom_line() +
  labs(title = "Monthly Overnight Stays in Vaud",
       x = "Date",
       y = "Number of Overnight Stays")

# Calculate the distribution
vaud_distribution <- vaud_data |>
  summarise(mean = mean(value, na.rm = TRUE), 
            median = median(value, na.rm = TRUE), 
            sd = sd(value, na.rm = TRUE)) |>
  pivot_longer(cols = everything(), names_to = "Statistic", values_to = "Value")

# Plot the distribution
# Density plot
ggplot(vaud_data, aes(x = value)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of Overnight Stays in Vaud",
       x = "Number of Overnight Stays",
       y = "Density")

# Boxplot
ggplot(vaud_data, aes(x = "", y = value)) +
  geom_boxplot(fill = "blue") +
  labs(title = "Boxplot of Overnight Stays in Vaud",
       x = "",
       y = "Number of Overnight Stays")

# Violin plot
ggplot(vaud_data, aes(x = "", y = value)) +
  geom_violin(fill = "blue") +
  labs(title = "Violin Plot of Overnight Stays in Vaud",
       x = "",
       y = "Number of Overnight Stays")

### STL Decomposition ###
#########################

# STL decomp
dcmp <- ts_vaud |> model(STL(value)) 
components(dcmp)

# Plot the trend
ts_vaud |> 
  autoplot(value, color='gray') + autolayer(components(dcmp), trend, color='red') + xlab("Year") + ylab("Temperature (°C)") + ggtitle("Trend")

# Show all compononents of our time serie
components(dcmp) |> autoplot() + xlab("Year")

# Plot season adjusted
ts_vaud |>
  autoplot(value, color='gray') + autolayer(components(dcmp), season_adjust, color='blue') + xlab("Year") + ylab("Value") + ggtitle("Value Seasonally Adjusted")

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
# Density plot
ggplot(luzern_data, aes(x = value)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of Overnight Stays in Luzern by Japanese Tourists",
       x = "Number of Overnight Stays",
       y = "Density")

# Boxplot
ggplot(luzern_data, aes(x = "", y = value)) +
  geom_boxplot(fill = "blue") +
  labs(title = "Boxplot of Overnight Stays in Luzern by Japanese Tourists",
       x = "",
       y = "Number of Overnight Stays")

# Violin plot
ggplot(luzern_data, aes(x = "", y = value)) +
  geom_violin(fill = "blue") +
  labs(title = "Violin Plot of Overnight Stays in Luzern by Japanese Tourists",
       x = "",
       y = "Number of Overnight Stays")