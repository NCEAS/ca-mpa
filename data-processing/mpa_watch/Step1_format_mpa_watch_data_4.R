

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(rinat)
library(tidyverse)

# Directories
indir <- "data/mpa_watch/raw"
outdir <- "data/mpa_watch/processed"
plotdir <- "data/mpa_watch/figures"

# Read data
data_orig <- read.csv(file.path(indir, "Surveys_2022-05-09_CaliforniaPrograms.csv"), as.is=T)


# Format data
################################################################################

# Format data
data <- data_orig %>%
  # Rename
  janitor::clean_names("snake")

# Inspect
str(data)





