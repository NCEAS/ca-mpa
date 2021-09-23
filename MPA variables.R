# MPA variables
# ---
# code by Jacob Eurich, Sept. 22 2021

# Initialization ----------------------------------------------------------
data_path <- "/home/shares/ca-mpa/MPA variable data/"
input_file <- "California MPA variables for evaluation analyses.csv"

# Data
mpa_raw <- read.csv(file.path(data_path, input_file), header=TRUE)

# Packages
library(dplyr)
library(ggplot2)

# Clean and explore raw data
mpa <- mpa_raw
colnames(mpa)
str(mpa)
summary_list <- lapply(mpa, summary)
summary(mpa$Size_km2)
