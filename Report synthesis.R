# Report synthesis
# ---
# code by Jacob Eurich, Jan 2022


# Clear workspace
rm(list = ls())

# Initialization ----------------------------------------------------------

# Packages
library(ggplot2)
library(tidyverse)
# install.packages("janitor")
# library(janitor)

# Directories
datadir <- "case_studies/data"
plotdir <- "case_studies/figures"

data_path <- "/home/shares/ca-mpa/GD_data/"
input_file <- "California MPA variables for evaluation analyses.csv"

# Read data
datafile <- "Eurich_KiribatiClam_Case_Study.xlsx"
data_orig <- readxl::read_excel(file.path(datadir, datafile), sheet=6, skip = 5, na="NA")

# Format data ----------------------------------------------------------

