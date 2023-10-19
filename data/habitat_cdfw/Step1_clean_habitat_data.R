

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Users/cfree/Library/CloudStorage/GoogleDrive-cfree@ucsb.edu/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data" #Chris
indir <- file.path(basedir, "habitat_cdfw/raw")
outdir <- file.path(basedir, "habitat_cdfw/processed")
plotdir <- "data/habitat_cdfw/figures"

# Read data
data_orig <- sf::st_read(file.path(indir, "HAB_CA_PredictedSubstrate_WZ", "HAB_CA_PredictedSubstrate_WZ.shp"))

# Read Stanford data to get its useful projection
# stanford_orig <- sf::st_read(file.path(basedir, "habitat_stanford/raw", "HAB_SCSR_PredictedSubstrateWDepth.shp"))

# Projections
wgs84 <- sf::st_crs("+proj=longlat +datum=WGS84")

# NAD83 / California Albers = EPSG:3310
sf::st_crs(data_orig)



# Format shapefile
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>%
  # Simplify
  select(sub, zone) %>% 
  setNames(c("substrate", "depth", "geometry")) %>% # rename is very slow
  # Add numeric substrate
  mutate(substrate_code=recode(substrate,
                               "Hard"="1",
                               "Soft"="0") %>% as.numeric()) %>% 
  # Select
  select(substrate, substrate_code, depth, geometry)


# Inspect
str(data)
freeR::complete(data)
table(data$substrate)
table(data$depth)
table(data$substrate_code)

# Export
saveRDS(data, file=file.path(outdir, "CA_substrate_polygon_cdfw.Rds"))
sf::st_write(data, dsn=file.path("~/Desktop", "CA_substrate_polygon_cdfw.shp")) # sf won'twri


# Rasterize shapefile
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






