
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)
library(tidycensus)

# Directories
indir <- "data/census_data/raw"
outdir <- "data/census_data/processed"
plotdir <- "data/census_data/figures"

# Package
# https://walker-data.com/tidycensus/articles/basic-usage.html

# Set API key
tidycensus::census_api_key(key="974c34380d8224622c765f34050e53a382511e33", install=T, overwrite = T)

# Download data
vars <- load_variables(year=2010, dataset="sf1", cache = FALSE)

age10 <- get_decennial(geography = "tract",
                       variables = "P013001",
                       state="California",
                       year = 2010)



