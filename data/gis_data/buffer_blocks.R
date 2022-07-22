## Generate Block Buffers
## Cori Lopazanski; June 13 2022

# Setup ------------------------------------------------------------------------

## Clear workspace
rm(list = ls())

##Packages
library(tidyverse)

## Directories
base.dir <- "/Volumes/GoogleDrive-105151121202188525604/Shared drives/NCEAS MPA network assessment/MPA Network Assessment: Working Group Shared Folder/data/sync-data" # Cori Local
gis.dir <- file.path(base.dir, "gis_data/processed")
out.dir <- file.path(gis.dir, "CA_block_polygons_buffers")
plot.dir <- "data/gis_data/figures"


## Read Data --------------------------------------------------------------------

### Read Block Stats
block_stats <- readRDS(file.path(gis.dir,"CA_blocks_stats.Rds"))

# Generate Block Buffers -------------------------------------------------------

# Buffer distances
buffers_km <- c(0.5, 1, 3, 5, 10, 20, 50)

# Loop through buffer distances
for(i in 1:length(buffers_km)){
  
  # Buffer
  buffer_do <- buffers_km[i]
  
  # Buffer MPAs
  block_buffer <- sf::st_buffer(block_stats, dist=buffer_do*1000)
  
  # Export data
  filename <- paste0("CA_block_polygons_", buffer_do, "km_buffer.Rds")
  saveRDS(block_buffer, file.path(out.dir,  filename))
}
