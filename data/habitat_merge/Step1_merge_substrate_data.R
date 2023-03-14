

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
datadir1 <- file.path(basedir, "habitat_cdfw/processed")
datadir2 <- file.path(basedir, "habitat_anita/raw")
plotdir <- "data/habitat_cdfw/figures"

# Read data
cdfw_orig <- terra::rast(file.path(datadir1, "CA_bottom_substrate_10m.tiff")) 
ci_orig <-  terra::rast(file.path(datadir2, "CIN_rock/All_CI_N_rock_2m.tif"))


# Build data
################################################################################

# Reproject Channel Islands raster
ci <- ci_orig %>% 
  terra::project(y=cdfw_orig)
terra::plot(ci_orig)
terra::plot(ci)

# Recode non-Channel Islands raster
# 1-4 = soft; 5-8 = hard
cdfw <- cdfw_orig %>% 
  terra::classify(rcl=matrix(c(1, 0,
                               2, 0, 
                               3, 0,
                               4, 0,
                               5, 1,
                               6, 1, 
                               7, 1, 
                               8, 1), ncol=2, byrow=T))
plot(cdfw)

# Merge rasters

ci_df <- ci %>% 
  as.data.frame()


# Plot islands
################################################################################

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf") %>% 
  sf::st_transform(crs=crs(ci))
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf") %>% 
  sf::st_transform(crs=crs(ci))

# Plot data
g <- ggplot() +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot raster
  # Crop
  # terra::ext(ci)
  coord_sf(datum=crs(usa),
           xlim=c(-100000, 100000),
           ylim=c(-500000, -400000)) +
  # Theme
  theme_bw()
g




# Inspect Channel Islands
################################################################################

cdfw_check <- cdfw_orig %>%
  raster::projectRaster(crs=raster::crs(ci_orig)) %>% 
  raster::crop(y=raster::extent(ci_orig))

# Anita
plot(ci_orig)


