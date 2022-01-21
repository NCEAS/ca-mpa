# Report synthesis
# ---
# code by Jacob Eurich, Jan 2022


# Clear workspace
rm(list = ls())

# Initialization ----------------------------------------------------------

# Packages
library(ggplot2)
library(tidyverse)

# Directories
# data_path <- "/home/shares/ca-mpa/GD_data/" [couldn't get this to work Julien]
# input_file <- "Technical_report_synthesis_20Jan22.xlsx"

# Read data
datadir <- "data"
datafile <- "Technical_report_synthesis_20Jan22.xlsx"
data_orig <- readxl::read_excel(file.path(datadir, datafile), sheet=2, skip = 0, na="NA")

# Format data ----------------------------------------------------------

