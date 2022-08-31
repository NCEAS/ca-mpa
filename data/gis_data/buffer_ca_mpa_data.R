
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
indir <- file.path(basedir, "gis_data/raw/CA_MPA_boundaries/ds582")
outdir <- file.path(basedir, "gis_data/processed")
plotdir <- "data/gis_data/figures"

# Projections
moll <- "+proj=moll"
wgs84 <- "+proj=longlat +datum=WGS84"

# Read data
mpas <- readRDS(file.path(outdir, "CA_MPA_polygons.Rds"))

# Convert to meters
mpas_m <- mpas %>%
  sf::st_transform("+proj=moll")


# Read data
################################################################################

# Buffer distances
buffers_km <- c(30, 40, 60, 70, 80, 90, 100) #c(0.1, 0.5, 1, 3, 5, 10, 20, 50)

# Loop through buffer distances
for(i in 1:length(buffers_km)){
  
  # Buffer
  buffer_do <- buffers_km[i]
  
  # Buffer MPAs
  mpas_buffer <- sf::st_buffer(mpas_m, dist=buffer_do*1000) %>%
    sf::st_transform(wgs84)
  
  # Plot data
  if(F){
    g <- ggplot() +
      geom_sf(data=mpas, color="black", fill=NA) +
      geom_sf(data=mpas_buffer, color="grey30", fill=NA)
    g
  }
  
  # Export data
  filename <- paste0("CA_MPA_polygons_", buffer_do, "km_buffer.Rds")
  saveRDS(mpas_buffer , file.path(outdir,  filename))
  
}



