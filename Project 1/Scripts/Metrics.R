# Load the necessary libraries
library(here)
library(stringr)

# Source from ETS.R, ARIMA.R, and TSLM.R
load(here("Project 1", "Data", "metricsETS_df.rdata"))
load(here("Project 1", "Data", "metricsarima_df.rdata"))
load(here("Project 1", "Data", "metricsTSLM_df.rdata"))

# Combine all metrics into one dataframe
metrics_df <- rbind(metricsETS_df, metricsarima_df, metricsTSLM_df)

# make a df for only vaud and one for Luzern
metrics_vaud <- metrics_df %>% filter(str_detect(Model, "Vaud"))
metrics_luzern <- metrics_df %>% filter(str_detect(Model, "Luzern"))