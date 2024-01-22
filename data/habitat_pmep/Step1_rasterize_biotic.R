# Explore & Process PMEP Habitat Data (Biotic Component)
# Cori Lopazanski
# January 2024

# About --------------------------------------------------------------------------------
# Read and clean the CA subset of the Pacific Marine and Estuary Partnership data


# Setup --------------------------------------------------------------------------------
rm(list=ls())

# Load required packages
library(tidyverse)
library(janitor)
library(sf)
library(terra)
library(raster)
library(fasterize)
library(purrr)
library(gdalUtils)

# Directories
gdb.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/PMEP_Nearshore_Zones_and_Habitat.gdb" # Aurora
bio.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/biotic"
fig.dir <- "~/ca-mpa/analyses/7habitat/figures" 

# Read ---------------------------------------------------------------------------------
# Zones
zones_ca <- read_sf(dsn = gdb.dir, layer = "West_Coast_USA_Nearshore_Zones") %>% 
  st_zm() %>% 
  filter(State == "CA")

# Shapefiles
biotic <- read_sf(dsn = file.path(bio.dir, "biotic_ca"), 
                  layer = 'West_Coast_USA_Nearshore_CMECS_Biotic_Habitat') 

# Attributes
attribute <- readRDS(file.path(bio.dir, "West_Coast_USA_Nearshore_CMECS_Biotic_Habitat_Attributes.Rds")) %>% 
  filter(State == "CA") %>% 
  filter(!(CMECS_BC_Code == '9.9.9.9.9')) %>% # drop unclassified
  filter(!(CMECS_BC_Code == '1.2')) # drop floating plants

# Rasterize -----------------------------------------------------------------------------

# List of habitat types
hab_types <- attribute %>% 
  dplyr::select(CMECS_BC_Category_Code, CMECS_BC_Code, CMECS_BC_Category, CMECS_BC_Name,
           CMECS_BC_Cartography, CMECS_BC_Cartography_Detail, 
           FaunalBed, AquaticVegetationBed, BenthicMacroalgae, Kelp, OtherMacroalgae, 
           Seagrass, AquaticVascularVegetation, FloatingSuspendedBiota) %>% 
  distinct()

# Kelp NS_PolyIDs
kelp <- attribute %>% filter(Kelp == "Yes") 


# Rasterize by section @ 24m -------------------------------------------------------

## 1. Create empty 24m grid ----
# Create empty raster grid at 24m resolution across entire extent of CA, use
# zones layer to ensure grid is maximum extent and consistent
raster <- raster::raster(zones_ca, resolution = 24, crs = st_crs(zones_ca))

# 2. Create function to rasterize section by section
rasterize_by_section <- function(section, layer_name){
  print(paste("Section:", section))
  print(paste("Layer:", layer_name))
  
  # Select section and layer to rasterize
  att_subset <- attribute %>% 
    filter(.data[[layer_name]] == "Yes") %>% 
    filter(PMEP_Section == section)
  
  print(paste("Subset Obs:", nrow(att_subset)))
  
  geo_subset <- biotic %>% 
    filter(NS_PolyID %in% att_subset$NS_PolyID) %>% 
    st_zm() %>% 
    st_transform(crs = st_crs(zones_ca))
  
  print(paste("Geo Obs:", nrow(geo_subset)))
  
  zone_section <- zones_ca %>% 
    filter(PMEP_Section == section)
  
  # Crop empty raster to zone section extent
  raster_crop <- crop(raster, zone_section)
  
  print(extent(raster_crop))
  print(extent(geo_subset))
  
  # Rasterize selected layer and section
  habitat_raster <- fasterize::fasterize(sf = geo_subset, raster = raster_crop)
  
  # Compare to polygon area
  area_poly <- sum(st_area(geo_subset)) %>% as.vector()
  area_rast <- habitat_raster %>% 
    as.data.frame() %>% 
    filter(!is.na(layer)) %>% 
    summarize(area = n() * 24 * 24)
  
  print(area_rast/area_poly)
  
  # Convert to SpatRaster 
  habitat_raster <- rast(habitat_raster)
  
  # Write raster to file
  terra::writeRaster(habitat_raster, file.path(bio.dir, paste("PMEP_Biotic_", layer_name, "_24mRes_Section", section, ".tif", sep = "")), 
                     overwrite = T)
  
}

# 3. Apply function to sections
## Drop Z dimension
#biotic_xy <- st_zm(biotic)

## a. Kelp ----
sections <- c(23, 30, 31, 32, 33, 40, 41) # 
layer_name <- "Kelp"
map2(sections, layer_name, rasterize_by_section) 

## b. Faunal Bed ----

## c. Aquatic vegetation ----





kelp_23 <- rast(file.path(bio.dir, "PMEP_Biotic_Kelp_24mRes_Section23.tif"))
kelp_23_att <- attribute %>% 
  filter(PMEP_Section == 23 & Kelp == "Yes")

ggplot() + 
  geom_sf(data = biotic %>% filter(NS_PolyID %in% kelp_23_att$NS_PolyID) %>% 
            st_zm() %>% st_transform(crs = st_crs(zones_ca)),
          mapping = aes(), color = "black", fill = "transparent") +
  geom_tile(kelp_23 %>% as.data.frame(xy = T), 
            mapping = aes(x = x, y = y, fill = "green", alpha = 0.5, color = "green"), show.legend = F) +
  scale_y_continuous(limits = c(1092000, 1094000)) +
  scale_x_continuous(limits = c(168000, 170000))

