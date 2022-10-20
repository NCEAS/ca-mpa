

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
indir <- file.path(basedir, "habitat_cdfw/raw")
outdir <- file.path(basedir, "habitat_cdfw/processed")
plotdir <- "data/habitat_cdfw/figures"

# Read data
# wetlands_orig <- sf::st_read(file.path(indir, "Wetlands", "HAB_CA_Wetlands.shp"))
# shoretypes_orig <- sf::st_read(file.path(indir, "Shoretypes", "HAB_CA_ShoreTypes.shp"))
# art_reefs_orig <- sf::st_read(file.path(indir, "HAB_SCSR_ArtificialReefs_Cen", "HAB_SCSR_ArtificialReefs_Cen.shp"))
# eelgrass_orig <- sf::st_read(file.path(indir, "HAB_CA_Eelgrass", "HAB_CA_Eelgrass_161128.shp"))
# estuaries_orig <- sf::st_read(file.path(indir, "Estuaries", "HAB_CA_Estuaries.shp"))
# marsh_orig <- sf::st_read(file.path(indir, "CoastalMarsh", "HAB_CA_coastalmarsh.shp"))
substrate_orig <- sf::st_read(file.path(indir, "HAB_CA_PredictedSubstrate_WZ", "HAB_CA_PredictedSubstrate_WZ.shp"))

# Projections
wgs84 <- sf::st_crs("+proj=longlat +datum=WGS84")

# NAD83 / California Albers = EPSG:3310
sf::st_crs(substrate_orig)


# Rasterize substrate
################################################################################

# Format substrate
substrate <- substrate_orig %>% 
  select(Sub_Depth, geometry) %>% 
  rename(substrate=Sub_Depth) %>% 
  mutate(substrate=recode(substrate,
                          "Soft 0 - 30m"="1", 
                          "Soft 30 - 100m"="2",
                          "Soft 100 - 200m"="3",
                          "Soft 200 - 3000m"="4",
                          "Hard 0 - 30m"="5",
                          "Hard 30 - 100m"="6",
                          "Hard 100 - 200m"="7",
                          "Hard 200 - 3000m"="8") %>% as.numeric())

unique(substrate$substrate)

# Bounding box
bbox <- sf::st_bbox(substrate_orig)

# Template raster
temp_ras <- raster::raster(res=10, 
                           xmn=bbox$xmin,
                           xmx=bbox$xmax,
                           ymn=bbox$ymin,
                           ymx=bbox$ymax, 
                           crs='+init=EPSG:3310')

# Rasterize substrate polygon
substrate_ras <- fasterize::fasterize(sf=substrate,
                                      raster=temp_ras,
                                      field="substrate",
                                      fun="first",
                                      background=NA)

# Plot 
# raster::plot(substrate_ras)

# Export
raster::writeRaster(substrate_ras, filename = file.path(outdir, "CA_bottom_substrate_10m.tiff"), overwrite=T)






