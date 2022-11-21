
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
state_orig <- readRDS(file.path(outdir, "CA_state_waters_polygons.Rds")) %>% 
  sf::st_transform(moll)



# Read data
################################################################################

# Buffer distances
buffers_km <- c(5)

# Loop through buffer distances
i <- 1
for(i in 1:length(buffers_km)){
  
  # Buffer
  buffer_do <- buffers_km[i]
  
  # Buffer waters
  state <- sf::st_buffer(state_orig, dist=buffer_do*1000) %>%
    sf::st_transform(wgs84)
  
  # Plot data
  if(F){
    g <- ggplot() +
      geom_sf(data=state_orig %>% sf::st_transform(wgs84), color="black", fill=NA) +
      geom_sf(data=state, color="grey30", fill=NA)
    g
  }
  
  # Export data
  filename <- paste0("CA_state_waters_", buffer_do, "km_buffer.Rds")
  saveRDS(state, file.path(outdir,  filename))
  
}



