
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)
library(tidycensus)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data/"
indir <- file.path(basedir, "census_data/raw")
outdir <- file.path(basedir, "census_data/processed")
plotdir <- "data/census_data/figures"

# Read data
pop <- readRDS(file=file.path(outdir, "CA_2010_census_tot_pop_by_block_raster.Rds"))

# Buffered MPA files
mpa_buffers <- list.files(file.path(basedir, "gis_data/processed"), pattern="km_buffer.Rds")
mpa_buffers_km <- gsub("CA_MPA_polygons_|km_buffer.Rds", "", mpa_buffers) %>% as.numeric()


# Calculate data
################################################################################

# Convert to raster
pop_ras <- pop %>% 
  select(long_dd, lat_dd, people) %>% 
  raster::rasterFromXYZ(crs = "+proj=longlat +datum=WGS84") 

# Loop through files
x <- 1
data_orig <- purrr::map_df(1: length(mpa_buffers), function(x){
  
  # Read data
  mpas <- readRDS(file.path(file.path(basedir, "gis_data/processed", mpa_buffers[x])))
  
  # Sum npeople within X km of MPA
  fdata <- raster::extract(x=pop_ras, y=mpas, method="simple", fun=sum, na.rm=T)
  
  # Convert data to dataframe
  fdata_df <- fdata %>% 
    as.data.frame() %>% 
    rename("npeople"="V1")
  
  # Add data to MPAs
  fdata_out <- mpas %>% 
    sf::st_drop_geometry() %>% 
    select(name, long_dd, lat_dd) %>% 
    mutate(npeople=fdata_df$npeople,
           buffer_km=mpa_buffers_km[x]) %>% 
    select(buffer_km, name, long_dd, lat_dd, npeople) %>% 
    rename(mpa=name)
  
  # # Plot quickly
  # ggplot(fdata_out, aes(x=long_dd, y=lat_dd, size=npeople)) +
  #   geom_point()
  
  
})

# Export
saveRDS(data_orig, file=file.path(outdir, "MPA_population_within_multiple_buffers.Rds"))
  







