# Load the here library
library(here)

# Read the data using a path relative to the project root including the 'Project 2' folder
data <- read.csv(here("Project 2", "Data", "seatemp.csv"), header = TRUE, skip = 2)

# Check the first few rows to ensure it's loaded correctly
head(data)