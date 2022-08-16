
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

# Read MPAs
mpas_50km <- readRDS(file.path(basedir, "gis_data/processed", "CA_MPA_polygons_50km_buffer.Rds"))

# Calculate data
################################################################################

# Convert to raster
pop_ras <- pop %>% 
  select(long_dd, lat_dd, people) %>% 
  raster::rasterFromXYZ(crs = "+proj=longlat +datum=WGS84") 

# Sum npeople within X km of MPA
data <- raster::extract(x=pop_ras, y=mpas_50km, method="simple", fun=sum, na.rm=T)

# Convert data to dataframe
data_df <- data %>% 
  as.data.frame() %>% 
  rename("npeople_50km"="V1")

# Add data to MPAs
data_out <- mpas_50km %>% 
  sf::st_drop_geometry() %>% 
  select(name, long_dd, lat_dd) %>% 
  mutate(npeople_50km=data_df$npeople_50km)

# Plot quickly
ggplot(data_out, aes(x=long_dd, y=lat_dd, size=npeople_50km)) +
  geom_point()

# Export
saveRDS(data_out, file=file.path(outdir, "MPA_population_within_50km.Rds"))
  







