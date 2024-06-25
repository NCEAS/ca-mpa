# Process WCDSCI Bathymetry Layer
# Cori Lopazanski
# January 2024

# Setup ---------------------------------------------------------------------
library(terra)
library(sf)

sync.dir <- "/home/shares/ca-mpa/data/sync-data"

# Read Data ---------------------------------------------------------------------

# WCDSCI bathymetry
bathy_200m <- rast(file.path(sync.dir, "wcdsci-bathymetry/raw/WCDSCI_EXPRESS_200m_0_1200mWD.tif"))

# PMEP nearshore zones shapefiles
zones_ca <- read_sf(dsn = file.path(sync.dir, "habitat_pmep/raw/PMEP_Nearshore_Zones_and_Habitat.gdb"), 
                    layer = 'West_Coast_USA_Nearshore_Zones') %>% 
  st_zm() %>% # convert to xy
  filter(State == "CA") %>% 
  mutate(PMEP_Zone = as.numeric(PMEP_Zone))

# Get CRS of the nearshore habitat data
zones_crs <- crs(zones_ca)

# Build -----------------------------------------------------------
# Reproject 200m version
bathy_200m_projected <- project(bathy_200m, zones_crs)

ext(bathy_200m)
ext(bathy_200m_projected)


# Export to processed folder
writeRaster(bathy_200m_projected, file.path(sync.dir, "wcdsci-bathymetry/processed/wcdsci_200m_projected.tif"))

# Reproject 25m version
bathy_25m <- rast(file.path(sync.dir, "wcdsci-bathymetry/raw/WCDSCI_EXPRESS_25m_0_1200mWD.tif"))

bathy_25m_projected <- project(bathy_25m, zones_crs)

# Export to processed folder
writeRaster(bathy_25m_projected, file.path(sync.dir, "wcdsci-bathymetry/processed/wcdsci_25m_projected.tif"))
