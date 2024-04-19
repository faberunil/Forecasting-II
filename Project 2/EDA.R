# Reading the data, skipping the first two lines
data <- read.csv("C:/Users/faber/Documents/Forecasting-II/Project 2/SST_ABSO_002.50E_007.50E_37.50N_42.50N_19820101_20221231_timeseries (1).csv", 
                 header = TRUE, skip = 2)

# Check the first few rows to ensure it's loaded correctly
head(data)
