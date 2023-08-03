
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
datadir <- "analyses/1performance_eco/output"

# Read data
data_orig <- readRDS(file.path(datadir, "biomass_with_moderators.Rds"))

