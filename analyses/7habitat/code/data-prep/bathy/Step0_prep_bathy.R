# Process bathymetry datasets
# Cori Lopazanski (lopazanski@bren.ucsb.edu)
# December 2024

library(tidyverse)
library(sf)
library(terra)

rm(list = ls())
gc()

wcds.dir <- "/home/shares/ca-mpa/data/sync-data/wcdsci-bathymetry"
ltm.dir  <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"
mbes.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_anita/raw/depth_MBES_CA"
caba.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_anita/raw/CA_Bathy_30m"
proc.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_anita/processed"


# Prep the rasters by removing >= 0 depth, matching resolutions ------------------------------------------

# The CSMP has the best coverage for the sites and has the benefit of being interpolated
# and closely examined for MPA purposes. For these reasons it will likely be our "default" layer.

# 1. Remove land from CSMP layer  ------------------------------------------------------

# Get files for the 30m CSMP layer
bathy_30m <- rast(file.path(caba.dir, "depth_30m_all_CA.tif"))

# Remove land (>= 0 depth) from CSMP layer and write to proc.dir
bathy_30m_masked <- app(bathy_30m, fun = function(x) ifelse(x >= 0, NA, x))
writeRaster(bathy_30m_masked, file.path(proc.dir, "csmp_30m_bathy.tif"), overwrite=TRUE)

rm(bathy_30m, bathy_30m_masked) # clean up

# Read the processed file as a template
template <- rast(file.path(proc.dir, "csmp_30m_bathy.tif"))

# 2. Upsample and mask CDFW layers + resample to CSMP -----------------------------------

# CDFW data is at 2m resolution, and in South spans 2-5m resolution across different areas. 
# It covers a wider range, but the higher resolution means that some sites have "better" 
# data than others. To keep consistent, we will upsample/aggregate to 30m resolution 
# to match the other datasets. 

# For each region (each file is provided separately for each region, we will 
# create and save a 30m version of the higher resolution raster. 

# Get files for 2m CDFW layers 
bathy_2m_rasters <- data.frame(region = list.files(mbes.dir, pattern = "2m_bathy\\.tif$", full.names = FALSE)) %>%
  mutate(region = str_remove(region, "_2m_bathy.tif$")) %>% 
  filter(!region %in% c("bat_scsr_is", "bat_scsr")) # handle south separately

# Read each region (except south), aggregate to 30m resolution, mask, and export
prep_rasters_30m <- function(regions, mbes.dir, proc.dir, template) {
  walk(bathy_2m_rasters$region, function(region_name) {
    orig <- rast(file.path(mbes.dir, paste0(region_name, "_2m_bathy.tif")))
    agg_30m  <- aggregate(orig, fact = 15, fun = mean, na.rm = TRUE)
    masked <- ifel(agg_30m >= 0, NA, agg_30m)
    writeRaster(masked, file.path(proc.dir, paste0(region_name, "_30m_bathy.tif")), overwrite = TRUE)
    }
  )
}

prep_rasters_30m(regions, mbes.dir, proc.dir, template)

# For the mainland south coast, rasters are provided at varying resolutions.
# Use same approach as above, but with different conversion factor for different regions.
scsr_rasters <- data.frame(region = list.files(mbes.dir, pattern = "scsr", full.names = FALSE))%>% 
  filter(str_detect(region, "bathy\\.tif$")) %>% 
  mutate(region = str_remove(region, "_bathy.tif$")) %>% 
  mutate(conversion = case_when(str_detect(region, "2m") ~ 15,
                                str_detect(region, "3m") ~ 10,
                                str_detect(region, "4m") ~ 7, # err on side of lower; only accepts integer
                                str_detect(region, "5m") ~ 6))
  
prep_rasters_scsr <- function(scsr_rasters, mbes.dir, proc.dir) {
  pmap(scsr_rasters, function(region, conversion, ...) {
    orig <- rast(file.path(mbes.dir, paste0(region, "_bathy.tif")))
    agg_30m <- aggregate(orig, fact = conversion, fun = mean, na.rm = TRUE)
    mask <- ifel(agg_30m >= 0, NA, agg_30m)
    
    if (str_detect(region, "4m")){
      target <- rast(ext = ext(mask), resolution = c(30, 30), crs = crs(mask)) # snap to 30x30
      mask <- resample(mask, target)
      
    }
    writeRaster(mask, file.path(proc.dir, paste0(region, "_30m_bathy.tif")),  overwrite = TRUE)
  })
}

prep_rasters_scsr(scsr_rasters, mbes.dir, proc.dir)

# Mosaic the south coast 30m rasters into a single raster: 
# The parts of south coast have different extents, so want to create a template
# matching the crs and res but with maximum extent (and resample to same origin)

# Pull the various south coast 30m files
scsr_files <- list.files(proc.dir, pattern = "bat_scsr", full.names = TRUE)
scsr_list <- lapply(scsr_files, rast)

# Union the extents
u <- ext(scsr_list[[1]])

for (i in 2:length(scsr_list)) {
  u <- union(u, ext(scsr_list[[i]]))
}

# Check
plot(u)
plot(ext(scsr_list[[1]]), add = T)
plot(ext(scsr_list[[2]]), add = T)
plot(ext(scsr_list[[3]]), add = T)
plot(ext(scsr_list[[4]]), add = T)
plot(ext(scsr_list[[5]]), add = T)

# Create template raster to resample
template_scsr <- rast(ext = u,
                      resolution = 30,
                      crs = crs(scsr_list[[1]]))

# Resample each to have same origin
scsr_aligned <- lapply(scsr_list, function(r) {
  resample(r, template_scsr, method = "bilinear")
})

# Check
compareGeom(scsr_aligned[[1]], scsr_aligned[[2]])
compareGeom(scsr_aligned[[2]], scsr_aligned[[3]])
compareGeom(scsr_aligned[[3]], scsr_aligned[[4]])
compareGeom(scsr_aligned[[4]], scsr_aligned[[5]])

# Create spat raster collection to mosaic the south coast rasters
scsr_sprc <- sprc(scsr_aligned)
scsr_mosaic <- mosaic(scsr_sprc)
plot(scsr_mosaic) # worked

# Project the scsr raster to match the other regions
scsr_mosaic <- project(scsr_mosaic, "EPSG:26910")

# Pull the additional regional rasters into a list
regional_files <- data.frame(region = list.files(proc.dir, pattern = "bat_", full.names = TRUE)) %>% 
  filter(!str_detect(region, "scsr")) %>% 
  pull(region)

regional_list  <- lapply(regional_files, rast) 

# Add the scsr_mosaic to the list
regional_list <- c(regional_list, scsr_mosaic)

# Union the extents of the regional + south coasts
u <- ext(regional_list[[1]])

for (i in 2:length(regional_list)) {
  u <- union(u, ext(regional_list[[i]]))
}

plot(u)
plot(ext(scsr_mosaic), add = T)
plot(ext(regional_list[[1]]), add = T)
plot(ext(regional_list[[2]]), add = T)
plot(ext(regional_list[[3]]), add = T)
plot(ext(regional_list[[4]]), add = T)
plot(ext(regional_list[[5]]), add = T)

# Create template raster to resample
template_regional <- rast(ext = u,
                      resolution = 30,
                      crs = crs(regional_list[[1]]))

# Resample each to have same origin
regional_aligned <- lapply(regional_list, function(r) {
  resample(r, template_regional, method = "bilinear")
})

compareGeom(regional_aligned[[1]], regional_aligned[[2]])
compareGeom(regional_aligned[[2]], regional_aligned[[3]])
compareGeom(regional_aligned[[3]], regional_aligned[[4]])
compareGeom(regional_aligned[[4]], regional_aligned[[5]])


# Create spat raster collection to mosaic the regional rasters
reg_sprc <- sprc(regional_aligned)
reg_mosaic <- mosaic(reg_sprc)
plot(reg_mosaic) # worked

writeRaster(reg_mosaic, file.path(proc.dir, "cdfw_30m_bathy.tif"), overwrite = T)


# 3. Remove land from WCDSCI layer and resample to match CSMP --------------------------

# Get files for the 25m WCDSCI layer
bathy_25m <- rast(file.path(wcds.dir, "raw/WCDSCI_EXPRESS_25m_0_1200mWD.tif"))
crs(bathy_25m) <- "+proj=omerc +lat_0=39 +lonc=-125 +alpha=75 +k=0.9996 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# Remove land (>= 0 depth) from WCDSCI layer and write to proc.dir
bathy_25m_masked <- ifel(bathy_25m >= 0, NA, bathy_25m)
writeRaster(bathy_25m_masked, file.path(proc.dir, "wcdsci_25m_bathy.tif"), overwrite=TRUE)

# Reproject to match CSMP crs
bathy_25m <- rast(file.path(proc.dir, "wcdsci_25m_bathy.tif"))
bathy_25m <- project(bathy_25m, crs(template), method = "bilinear")

# Create a target raster based on union extents
# (25m extends farther north/south than CSMP template)
u <- union(ext(bathy_25m), ext(template)) # union the extents

# Target raster should have union extent but CRS and resolution of CSMP template
target <- rast(ext = u, resolution = res(template), crs = crs(template)) 
target <- extend(target, target, snap = "out") # snaps to entire outer edge of cells

# Resample the 25m wcdsci layer to the target
bathy_25m_target <- resample(bathy_25m, target, method = "bilinear")

writeRaster(bathy_25m_target, file.path(proc.dir, "wcdsci_30m_bathy.tif"), overwrite=TRUE)


# Read the processed bathymetry datasets
csmp <- rast(file.path(proc.dir, "csmp_30m_bathy.tif"))
cdfw <- rast(file.path(proc.dir, "cdfw_30m_bathy.tif"))
wcds <- rast(file.path(proc.dir, "wcdsci_30m_bathy.tif"))

# Create a target raster for uniform origin, extent, crs, etc.
u <- union(ext(csmp), ext(wcds))
u <- union(u, ext(cdfw)) # this is identical to above; keep for clarity

target <- rast(ext = u, resolution = c(30, 30), crs = crs(csmp))

# 1. CSMP: Since target has same crs as CSMP, just need to resample:
crs(csmp) == crs(target) # true
csmp <- resample(csmp, target)

# Preserve in case of crash:
writeRaster(csmp, file.path(proc.dir, "csmp_30m_bathy_res.tif"), overwrite=TRUE)

# 2. CDFW: Project + Resample
crs(cdfw) == crs(target) # false
cdfw <- project(cdfw, target)
cdfw <- resample(cdfw, target)

# Preserve in case of crash:
writeRaster(cdfw, file.path(proc.dir, "cdfw_30m_bathy_res.tif"), overwrite=TRUE)

# 3. WCDSCI: 
crs(wcds) == crs(target)
ext(wcds) == ext(target)
wcds <- resample(wcds, target)

# Preserve in case of crash:
writeRaster(wcds, file.path(proc.dir, "wcds_30m_bathy_res.tif"), overwrite=TRUE)

