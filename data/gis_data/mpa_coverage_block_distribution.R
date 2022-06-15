## Create Histogram Showing Amount of MPA in Each Block
## Cori Lopazanski; June 13 2022

# Setup ------------------------------------------------------------------------

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive-105151121202188525604/Shared drives/NCEAS MPA network assessment/MPA Network Assessment: Working Group Shared Folder/data/sync-data" # Cori Local
gisdir <- file.path(basedir, "gis_data/processed")
plotdir <- "data/gis_data/figures"

  
# Read Data
block_mpa_key <- read.csv(file.path(gisdir, "CA_blocks_with_mpas.csv"), as.is=T)


# Histograms with Raw Area -----------------------------------------------------

# All Blocks with MPAs
g1 <- ggplot(data = block_mpa_key) +
  geom_histogram(aes(mpa_km2))

## Less than 10 square km of MPA
g2 <- ggplot(data = block_mpa_key %>% 
               filter(mpa_km2 < 10)) +
  geom_histogram(aes(mpa_km2))

## Less than 2.5 square km of MPA
g3 <- ggplot(data = block_mpa_key %>% 
               filter(mpa_km2 < 2.5)) +
  geom_histogram(aes(mpa_km2))

### Merge
g <- gridExtra::grid.arrange(g1, g2, g3, nrow=3)


### Export figure
ggsave(g, filename=file.path(plotdir, "figure_mpa_block_coverage_dist.png"),
       width=6.5, height=3.25, units="in", dpi=600)

# Histograms with Percent Coverage ---------------------------------------------



