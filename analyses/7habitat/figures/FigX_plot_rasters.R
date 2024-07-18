# Explore & Process PMEP Habitat Data
# Cori Lopazanski
# November 2023


# Setup --------------------------------------------------------------------------------
# Packages
library(tidyverse)
library(sf)
library(terra)
library(tmap)

# Directories
sync.dir <- "/home/shares/ca-mpa/data/sync-data"
sub.dir  <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/substrate/substrate_rasters"
bio.dir  <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/biotic/biotic_rasters"
  
# Read Data ----------------------------------------------------------------------------
# Zones
zones_ca <- read_sf(dsn = file.path(sync.dir, "habitat_pmep/PMEP_Nearshore_Zones_and_Habitat.gdb"), 
                    layer = 'West_Coast_USA_Nearshore_Zones') %>% 
  st_zm() %>% # convert to xy
  filter(State == "CA")

# MPA polygons
mpas <- readRDS(file.path(sync.dir, "/gis_data/processed/CA_MPA_polygons.Rds")) %>% 
  st_transform(., st_crs(zones_ca))  # transform to match CRS of substrate data

# Get land
#ca <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf") %>% 
#  filter(name == "California")
#foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")


# Process ------------------------------------------------------------------------------

# Read the habitat section
section1.1  <- rast(file.path(sub.dir, "PMEP_Substrate_1.1_24mRes_Section40.tif"))
section1.2  <- rast(file.path(sub.dir, "PMEP_Substrate_1.2_24mRes_Section40.tif"))
section1.2.1 <- rast(file.path(sub.dir, "PMEP_Substrate_1.2.1_24mRes_Section40.tif"))
section1.2.2 <- rast(file.path(sub.dir, "PMEP_Substrate_1.2.2_24mRes_Section40.tif"))

section1.1 <- subst(section1.1, 1, 1.10)
section1.2 <- subst(section1.2, 1, 1.20)
section1.2.1 <- subst(section1.2.1, 1, 1.21)
section1.2.2 <- subst(section1.2.2, 1, 1.22)

# Crop the zones to that section
zones_section <- zones_ca %>% filter(PMEP_Section == 40)

# Subset MPAs to that section
mpa_section <- st_crop(mpas, zones_section)

# Section bbox
#section_bbox <- st_bbox(zones_section)

plot(section1.1, col = "tan4")
plot(section1.2, col = "burlywood2", add = T)
plot(section1.2.1, col = "burlywood4", add = T)
plot(section1.2.2, col = "burlywood3", add = T)
plot(mpa_section$geometry, add = T)

bbox.campus <- st_bbox(st_buffer(mpas$geometry[mpas$name == "Campus Point SMCA (No-Take)"], 5000))

section1.1 <- crop(section1.1, bbox.campus)
section1.2 <- crop(section1.2, bbox.campus)
section1.2.1 <- crop(section1.2.1, bbox.campus)
section1.2.2 <- crop(section1.2.2, bbox.campus)

tmap_mode("view")
tm_shape(zones_section, bbox = bbox.campus) +
  tm_borders(col = "gray50") +
tm_shape(mpa_section, bbox = bbox.campus) +
  tm_borders(col = "black") +
tm_shape(section1.1) +
  tm_raster(palette = "tan4") +
tm_shape(section1.2) +
  tm_raster(palette = "burlywood2") +
tm_shape(section1.2.1) +
  tm_raster(palette = "burlywood4") + 
tm_shape(section1.2.2) +
  tm_raster(palette = "burlywood3") 


habitat_representation <- function(layer_name){
  # Read the section habitat raster
  sub_section <- rast(file_name)
  
  # Find the subset intersecting with MPAs
  sub_intersect <- intersect(mpas, sub_section)
  
  # Calculate the total area and store
  
  # Calculate the area within MPAs and store
  
}


# List of filenames for the substrate raster sections
filenames <- list.files(sub.dir, pattern="*.tif", full.names=TRUE)

# Read each of the raster sections
hard_sections <- lapply(filenames, rast)

# Create spat raster collection
hard_sections <- sprc(hard_sections) 





