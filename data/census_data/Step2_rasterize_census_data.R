
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

# Projections
wgs84 <- sf::st_crs("+proj=longlat +datum=WGS84")
utm10 <- sf::st_crs(" +proj=utm +zone=10 +datum=NAD83 +units=m +no_defs")

# Tract-level data
################################################################################

# Read data
tract_data <- readRDS(file.path(outdir, "CA_2010_census_tot_pop_by_tract.Rds"))

# Project to meters
tract_data_m <- tract_data %>% 
  sf::st_transform(utm10)

# Template raster
template_raster <- fasterize::raster(tract_data_m, res = 500)

# Rasterize data
tract_data_ras_m <- fasterize::fasterize(sf=tract_data_m, raster=template_raster, field="people_sqkm", fun="first", background = NA)

# Plot raster quickly
raster::plot(tract_data_ras_m)

# Convert to lat/long
tract_data_ras <- tract_data_ras_m %>% 
  raster::projectRaster(crs="+proj=longlat +datum=WGS84")

# Plot raster slowly
tract_data_ras_df <- tract_data_ras %>% 
  raster::as.data.frame(xy=T) %>% 
  rename(long_dd=x, lat_dd=y, people_sqkm=layer) %>% 
  mutate(people=people_sqkm*(0.5*0.5))
tract_data_ras_df_sample <- tract_data_ras_df %>% 
  sample_frac(size=0.1)
ggplot(tract_data_ras_df, aes(x=long_dd, y=lat_dd, fill=people_sqkm)) +
  geom_tile(color=NA) +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_fill_gradientn(name="Population density\n(people/sqkm)", colors=RColorBrewer::brewer.pal(9, "YlOrRd"), trans="log10", na.value = NA) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw()

# Check population
sum(tract_data_ras_df$people, na.rm=T) / 1e6

# Export data
saveRDS(tract_data_ras_df, file=file.path(outdir, "CA_2010_census_tot_pop_by_tract_raster.Rds"))


# Block-level data
################################################################################

# Read data
block_data <- readRDS(file.path(outdir, "CA_2010_census_tot_pop_by_block.Rds"))

# Project to meters
block_data_m <- block_data %>% 
  sf::st_transform(utm10)

# Template raster
template_raster <- fasterize::raster(block_data_m, res = 500)

# Rasterize data
block_data_ras_m <- fasterize::fasterize(sf=block_data_m, raster=template_raster, field="people_sqkm", fun="first", background = NA)

# Plot raster quickly
raster::plot(block_data_ras_m)

# Convert to lat/long
block_data_ras <- block_data_ras_m %>% 
  raster::projectRaster(crs="+proj=longlat +datum=WGS84")

# Plot raster slowly
block_data_ras_df <- block_data_ras %>% 
  raster::as.data.frame(xy=T) %>% 
  rename(long_dd=x, lat_dd=y, people_sqkm=layer) %>% 
  mutate(people=people_sqkm*(0.5*0.5))
block_data_ras_df_sample <- block_data_ras_df %>% 
  sample_frac(size=0.1)
ggplot(block_data_ras_df, aes(x=long_dd, y=lat_dd, fill=people_sqkm)) +
  geom_tile(color=NA) +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_fill_gradientn(name="Population density\n(people/sqkm)", colors=RColorBrewer::brewer.pal(9, "YlOrRd"), trans="log10", na.value = NA) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw()

# Check population
sum(block_data_ras_df$people, na.rm=T) / 1e6

# Export data
saveRDS(block_data_ras_df, file=file.path(outdir, "CA_2010_census_tot_pop_by_block_raster.Rds"))




