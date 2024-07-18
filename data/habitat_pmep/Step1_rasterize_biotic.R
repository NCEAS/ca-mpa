# Explore & Process PMEP Habitat Data (Biotic Component)
# Cori Lopazanski
# January 2024

# About --------------------------------------------------------------------------------
# Read and clean the CA subset of the Pacific Marine and Estuary Partnership data


# Setup --------------------------------------------------------------------------------
#rm(list=ls())

# Load required packages
library(tidyverse)
library(janitor)
library(sf)
library(terra)
library(raster)
library(fasterize)
library(purrr)
#library(gdalUtils) # NCEAS UPDATED R WITH NO NOTICE FML

# Directories
gdb.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/PMEP_Nearshore_Zones_and_Habitat.gdb" # Aurora
bio.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/biotic"
fig.dir <- "~/ca-mpa/analyses/7habitat/figures" 

# Read ---------------------------------------------------------------------------------
# Zones
zones_ca <- read_sf(dsn = gdb.dir, layer = "West_Coast_USA_Nearshore_Zones") %>% 
  st_zm() %>% # convert to xy
  filter(State == "CA")

# Shapefiles
biotic <- read_sf(dsn = file.path(bio.dir, "biotic_ca"), 
                  layer = 'West_Coast_USA_Nearshore_CMECS_Biotic_Habitat') 

# Attributes
attribute <- readRDS(file.path(bio.dir, "West_Coast_USA_Nearshore_CMECS_Biotic_Habitat_Attributes.Rds")) %>% 
  filter(State == "CA") %>% 
  filter(!(CMECS_BC_Code == '9.9.9.9.9')) %>% # drop unclassified
  filter(!(CMECS_BC_Code == '1.2')) # drop floating plants

# Explore -----------------------------------------------------------------------------

# List of habitat types
hab_types <- attribute %>% 
  dplyr::select(CMECS_BC_Category_Code, CMECS_BC_Category, 
           FaunalBed, AquaticVegetationBed, BenthicMacroalgae, Kelp, OtherMacroalgae, 
           Seagrass, AquaticVascularVegetation) %>% 
  distinct() %>% 
  arrange(CMECS_BC_Category_Code, CMECS_BC_Category,
          FaunalBed, AquaticVegetationBed, BenthicMacroalgae, Kelp, OtherMacroalgae, 
          Seagrass, AquaticVascularVegetation)

# Check geometries - all are multipolygon
geom_types <- st_geometry_type(biotic, by_geometry = TRUE) %>% as.data.frame()

# Check CRS - biotic is WGS 84 Pseudo Mercator, transform to match zones
st_crs(zones_ca)
st_crs(biotic)

# Sensitivity analysis for raster resolution ----------------------------------------------------------
# Conduct sensitivity analysis for rasterizing at different resolutions - use kelp raster as a
# proof of concept for the vegetation layers
# 1. Create a Function to Filter and Crop Subsets: This function will take 
# PMEP_Section and bounding box as inputs and return a cropped subset.
# 2. Iterate Over PMEP_Sections: Calculate the area for each subset using 
# the function, collect results from each iteration for comparison.

## a. Setup ------------------------------------------------------------
# Drop Z dimension, transform to match substrate
biotic <- st_zm(biotic) %>% 
  st_transform(crs = st_crs(zones_ca))

## b. Define functions and inputs ---------------------------------------------
# Select a few subsets within each PMEP Section
# (Hard to automate because not all areas contain the habitat)
bbox1 <- st_bbox(c(xmin = 170000, xmax = 174000, ymin = 1088000, ymax = 1093000), crs = st_crs(biotic)) # 23 - Cape Mendocino to Cape Blanco
bbox2 <- st_bbox(c(xmin = 197000, xmax = 199000, ymin = 822000, ymax = 829000), crs = st_crs(biotic)) # 30 - Cape Mendocino to Point Reyes 
bbox3 <- st_bbox(c(xmin = 355000, xmax = 360000, ymin = 510000, ymax = 515000), crs = st_crs(biotic)) # 31 - Point Reyes to Point Sur
bbox4 <- st_bbox(c(xmin = 458000, xmax = 462000, ymin = 355000, ymax = 360000), crs = st_crs(biotic)) # 32 - Point Sur to Point Arguello including Davidson Se*
bbox5 <- st_bbox(c(xmin = 532000, xmax = 538000, ymin = 216000, ymax = 221000), crs = st_crs(biotic)) # 33 - Point Arguello South including San Juan Seamount
bbox6 <- st_bbox(c(xmin = 547000, xmax = 552000, ymin = 270000, ymax = 275000), crs = st_crs(biotic)) # 40 - Point Conception to Palos Verdes
bbox7 <- st_bbox(c(xmin = 795000, xmax = 800000, ymin = 90000, ymax = 95000), crs = st_crs(biotic)) # 41 - Palos Verdes to US-Mex Border

# Code for making sure that the bounding box covers kelp, using the rasters
# to explore because is way faster than dealing with original shapefiles
# kelp <- rast(file.path(bio.dir, "PMEP_Biotic_Kelp_24mRes_Section23.tif"))
# ext(kelp)
# ggplot() +
#   geom_tile(kelp %>% as.data.frame(xy = T), 
#             mapping = aes(x = x, y = y), fill = "red", alpha = 0.5, show.legend = F) +
#   scale_x_continuous(limits = c(170000,  174000)) +
#   scale_y_continuous(limits = c(1088000,  1093000))

# Define function for rasterizing at given res and calculating area 
subset_area <- function(section, resolution, bbox){
  print(paste("Section:", section))
  print(paste("Resolution:", resolution))
  
  # Subset polygon to layer
  subset <- attribute %>% 
    filter(PMEP_Section == section) %>% 
    filter(.data[[layer_name]] == "Yes") %>% 
    left_join(., biotic) %>% 
    st_as_sf()
  
  print(paste("Obs:", nrow(subset)))
  
  # Crop subset to bounding box (for kelp)
  subset <- st_crop(subset, bbox)
  
  # Rasterize
  raster <- raster::raster(subset, resolution = resolution)
  habitat_raster <- fasterize::fasterize(sf = subset, raster = raster)
  
  # Calculate polygon area
  poly_area <- subset %>% 
    st_union() %>% 
    st_area() %>% 
    as.vector()
  
  print(paste("Polygon area:", poly_area))
  
  
  # Calculate area
  area <- habitat_raster %>% 
    as.data.frame() %>% 
    filter(!is.na(layer)) %>% 
    summarize(area = n() * resolution * resolution) %>% 
    mutate(res = resolution) %>% 
    mutate(section = section) %>% 
    mutate(poly_area = poly_area)
  
  return(area)
}

## c. Params for each habitat -------------------------------------------------
# Kelp 
# Must use bounding boxes for kelp; otherwise st_join will crash the system
sections <- rep(c(23, 30,  31, 32, 33, 40, 41), times = 7) #
bboxes <- rep(list(bbox1, bbox2, bbox3, bbox4, bbox5, bbox6, bbox7), times = 7) # 
resolutions <- rep(c(500, 250, 100, 50, 24, 10, 5), each = 7) # 
layer_name <- "Kelp"
all_results <- pmap(list(sections, resolutions, bboxes), subset_area) 

# Faunal Bed 
sections <- c(rep(c(30, 31, 32, 33, 40, 41), times = 7)) # omit 23, no fb
resolutions <- c(rep(c(200, 100, 50, 24, 15, 10, 5), each = 6))
layer_name <- "FaunalBed"
all_results <- map2(sections, resolutions, subset_area) 

# Aquatic Vegetation 
sections <- c(rep(c(23, 30,  31, 32, 33, 40, 41), times = 7)) # 
bboxes <- rep(list(bbox1, bbox2, bbox3, bbox4, bbox5, bbox6, bbox7), times = 7) # 
resolutions <- c(rep(c(200, 100, 50, 24, 15, 10, 5), each = 7)) #
layer_name <- "AquaticVegetationBed"
all_results <- pmap(list(sections, resolutions, bboxes), subset_area) 

## d. Results -----------------------------------------------------------------
# Create dataframe of results 
area_compare <- bind_rows(all_results) %>% 
  mutate(#res = as.factor(res),
    section = as.factor(section),
    diff = area/poly_area) %>%  
  mutate(diff_abs = abs(1-area/poly_area)) 

#saveRDS(area_compare, file.path(bio.dir, "resolution-sensitivity-results-kelp.Rds"))
#saveRDS(area_compare, file.path(bio.dir, "resolution-sensitivity-results-faunalbed.Rds"))
#saveRDS(area_compare, file.path(bio.dir, "resolution-sensitivity-results-aquaticveg.Rds"))

# Average difference for each resolution (across different bboxes)
average <- area_compare %>% 
  group_by(res) %>% 
  summarize(res_mean = mean(diff_abs),
            res_sd = sd(diff_abs))


## d. Plot differences --------------------------------------------------
# Plot of differences
ggplot(data = area_compare) + 
  geom_point(aes(x = res, y = diff_abs, color = section))+
  theme_minimal()+
  labs(x = "Resolution",
       y = "Diff from polygon area",
       color = "Subset")
#ggsave(file.path(fig.dir, "Step0_pmep_biotic_kelp_rasterize_resolutions.png"))
#ggsave(file.path(fig.dir, "Step0_pmep_biotic_fanalbed_rasterize_resolutions.png"))
#ggsave(file.path(fig.dir, "Step0_pmep_biotic_aquaticveg_rasterize_resolutions.png"))

ggplot(data = average, aes(x = res, y = res_mean)) + 
  geom_col() +
  geom_errorbar(aes(ymin = res_mean, ymax = res_mean + res_sd)) +
  theme_minimal() +
  labs(x = "Resolution", y = "Average proportional difference from polygon area")
#ggsave(file.path(fig.dir, "Step0_pmep_biotic_kelp_rasterize_avg_resolutions.png"))
#ggsave(file.path(fig.dir, "Step0_pmep_biotic_fanalbed_rasterize_avg_resolutions.png"))
#ggsave(file.path(fig.dir, "Step0_pmep_biotic_aquaticveg_rasterize_avg_resolutions.png"))


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
  subset <- attribute %>% 
    filter(.data[[layer_name]] == "Yes") %>% 
    filter(PMEP_Section == section) %>% 
    left_join(., biotic) %>% 
    st_as_sf()
  
  print(paste("Subset Obs:", nrow(subset)))

  # Crop empty raster to zone section extent
  zone_section <- zones_ca %>% 
    filter(PMEP_Section == section)
  
  raster_crop <- crop(raster, zone_section)
  
  # Rasterize selected layer and section
  habitat_raster <- fasterize::fasterize(sf = subset, raster = raster_crop)
  
  # Convert to SpatRaster 
  habitat_raster <- rast(habitat_raster)
  
  # Write raster to file
  terra::writeRaster(habitat_raster, file.path(bio.dir, "biotic_rasters", paste("PMEP_Biotic_", layer_name, "_24mRes_Section", section, ".tif", sep = "")), 
                     overwrite = T)
  
}

# 3. Apply function to sections
# Make sure that biotic dataset is converted to XY and
# transformed to match the substrate CRS

## a. Kelp ----
#sections <- c(23, 30, 31, 32, 33, 40, 41)  
#layer_name <- "Kelp"
#map2(sections, layer_name, rasterize_by_section) 

# Test plot output
kelp <- rast(file.path(bio.dir, "biotic_rasters", "PMEP_Biotic_Kelp_24mRes_Section33.tif"))
plot(kelp, col = "blue")

## b. Faunal Bed ----
#sections <- c(30, 31, 32, 33, 40, 41)  # none for 23
#layer_name <- "FaunalBed"
#map2(sections, layer_name, rasterize_by_section) 

## c. Aquatic vegetation ----
sections <- c(23, 30, 31, 32, 33, 40, 41)  
layer_name <- "AquaticVegetationBed"
map2(sections, layer_name, rasterize_by_section) 


# Combine into a single raster -----------------------
raster.dir <- file.path(bio.dir, "biotic_rasters")

combine_sections <- function(layer_name){
  # List filenames
  filenames <- list.files(raster.dir, pattern = paste("*", layer_name, sep = ""), 
                          full.names = T)
  
  # Read each of the sections
  sections <- lapply(filenames, rast)
  
  # Create spatraster collection
  sections <- sprc(sections)
  
  # Merge into single raster
  merged <- terra::merge(sections)
  
  # Write to file
  terra::writeRaster(merged, file.path(raster.dir, paste("PMEP_Biotic_", layer_name, "_24mRes_Full.tif", sep = "")), overwrite = T)
}

# Combine rasters and export
#combine_sections(layer_name = "Kelp")
#combine_sections(layer_name = "FaunalBed")
#combine_sections(layer_name = "AquaticVegetationBed")

