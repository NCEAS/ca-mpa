
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
indir <- "data/gis_data/raw"
outdir <- "data/gis_data/processed"
plotdir <- "data/gis_data/figures"

# Projections
moll <- "+proj=moll"

# Read data
mpas <- readRDS(file.path(outdir, "CA_MPA_polygons_smrs_smcas.Rds"))

# Convert to meters
mpas_m <- mpas %>%
  sf::st_transform("+proj=moll")


# Read data
################################################################################

# Buffer MPAs
mpas_10km <- sf::st_buffer(mpas_m, dist=10*1000)

# Plot data
g <- ggplot() +
  geom_sf(data=mpas_m, color="black", fill=NA) +
  geom_sf(data=mpas_10km, color="grey30", fill=NA)
g


