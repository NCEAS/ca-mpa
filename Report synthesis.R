# Report synthesis
# ---
# code by Jacob Eurich, Jan 2022


# Clear workspace
rm(list = ls())

# Initialization ----------------------------------------------------------

# Packages
library(ggplot2)
library(tidyverse)

# Read data - Aurora
data_path <- "/home/shares/ca-mpa/" # [JB, I had to add this in manually via cyberduck, can you get the GD_data to auto sync?]
input_file <- "Technical_report_synthesis_20Jan22.xlsx"
data_orig <- readxl::read_excel(file.path(data_path, input_file), sheet=2, skip = 0, na="NA")

# Read data - JE local drive
# setwd("~/Documents/ACTIVE Research/NCEAS Postdoc/Data/Technical report synthesis")
datadir <- "data"
datafile <- "Technical_report_synthesis_20Jan22.xlsx"
data_orig <- readxl::read_excel(file.path(datadir, datafile), sheet=2, skip = 0, na="NA")

# Format data ----------------------------------------------------------

