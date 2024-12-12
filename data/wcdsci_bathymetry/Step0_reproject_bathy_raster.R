# Step 0. Intersect Bathymetry Raster with Sites
# Cori Lopazanski lopazanski@bren.ucsb.edu
# Dec 2024


library(sf)
library(terra)

# Read the bathymetry raster
bathy_25m <- rast("/home/shares/ca-mpa/data/sync-data/wcdsci-bathymetry/raw/WCDSCI_EXPRESS_25m_0_1200mWD.tif")

# Read the LTM sites shapefiles
sites_raw <- read_sf("/home/shares/ca-mpa/data/sync-data/monitoring/ltm_sites_1000m_merged/ltm_sites_1000m_merged.shp") 

# The bathy raster CRS is not completely defined (missing a few details)
# These are consistent with the Hotine Oblique Mercator (Variant A) 
crs(bathy_25m) <- "+proj=omerc +lat_0=39 +lonc=-125 +alpha=75 +k=0.9996 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# Reproject bathymetry raster to match sites
bathy_25m_projected <- project(bathy_25m, crs(sites_raw))
writeRaster(bathy_25m_projected, "/home/shares/ca-mpa/data/sync-data/wcdsci-bathymetry/processed/wcdsci_25m_projected.tif", overwrite = T)

# Verify alignment
#intersect(ext(bathy_25m_projected), ext(sites_raw))
# didn't work, they don't overlap.

# # Intersect the raster with the LTM sites
# sites_projected <- st_transform(sites_raw, crs(bathy_25m))
# ext(sites_projected)
# ext(bathy_25m)
# intersect(ext(sites_projected), ext(bathy_25m))
