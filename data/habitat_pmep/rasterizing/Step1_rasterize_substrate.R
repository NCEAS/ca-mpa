# Explore & Process PMEP Habitat Data (Substrate Component)
# Cori Lopazanski
# November 2023

# About --------------------------------------------------------------------------------
# Read and clean the Pacific Marine and Estuary Partnership data
# 1. Fix multisurface geometries 
# 2. Sensitivity analysis for rasterizing hard substrate layer: compare the area of hard
# substrate from the original multipolygons to rasters created at different resolutions
# 3. Rasterize substrate section-by-section at 24m resolution for hard and soft
# 4. Combine section rasters into one statewide raster

# NOTE: This rasterizing step does *not* include the estuary portions of the 
# substrate data, which are captured in Sections 50, 52, 53. The extent of these
# sections are also not incorporated in the "Nearshore Zones" layer.

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


# Directories
#gdb.dir <- "/Users/lopazanski/Documents/habitat/PMEP/PMEP_Nearshore_Zones_and_Habitat.gdb" # Local
sync.dir <- "/home/shares/ca-mpa/data/sync-data"
out.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed"
gdb.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/PMEP_Nearshore_Zones_and_Habitat.gdb" # Aurora
fig.dir <- "~/ca-mpa/analyses/7habitat/figures" 
sub.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/substrate"

# Read Data ----------------------------------------------------------------------------
# Check layer names
st_layers(dsn=gdb.dir)

# Zones
zones_ca <- read_sf(dsn = gdb.dir, layer = "West_Coast_USA_Nearshore_Zones") %>% 
  st_zm() %>% # convert to xy
  filter(State == "CA")

# Shape files
substrate <- read_sf(dsn = file.path(sub.dir, "substrate_ca"), 
                  layer = 'West_Coast_USA_Nearshore_CMECS_Substrate_Habitat') 
"/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/substrate"

# Attributes
attribute <- readRDS(file.path(sub.dir, "West_Coast_USA_Nearshore_CMECS_Substrate_Habitat_Attributes.Rds")) %>% 
  filter(State == "CA") 

# Process multisurface geometries ----------------------------------
# This only necessary with original multipolygon data from .gdb; no longer
# multisurface within the exported shapefiles - only errors are within the
# estuary zones (50, 52...)

# There is an issue with several multisurface geometries - R cannot handle
# many spatial operations with these geometries.
geom_types <- st_geometry_type(substrate, by_geometry = TRUE) %>% as.data.frame()

# Get each of those multisurface observations
multisurf <- substrate_ca %>% slice(38, 46, 48, 49, 50, 698, 765)

# Try extracting geometries
multisurf_geoms <- lapply(multisurf$Shape, `[`)

# Cast to multipolygon
multisurf_mp <- lapply(multisurf_geoms, function(x) sf::st_multipolygon( x = x ) )

# Create simple dataframe with original metadata
multisurf_simple <- multisurf %>% st_drop_geometry()

# Create sf object
multisurf_sf <- sf::st_as_sfc(multisurf_mp) %>% st_sf() %>% 
  st_set_crs(st_crs(substrate_ca)) %>% 
  st_set_geometry("Shape") # rename geometry to match the substrate

# Join the sf object to the original dataframe
multisurf_corrected <- cbind(multisurf_simple, multisurf_sf) %>% 
  st_as_sf()

# Drop those multisurface polygons from the original dataset (still retained above)
substrate_ca <- substrate_ca %>% filter(!(NS_PolyID %in% multisurf$NS_PolyID)) 

# Check classess of two dfs
class(substrate_ca)
class(multisurf_corrected)

# Add the corrected geometries
substrate_ca <- rbind(multisurf_corrected, substrate_ca) 
class(substrate_ca) # check class

# Check geometry types to make sure no multisurface geometries remain
geom_types <- st_geometry_type(substrate_ca, by_geometry = TRUE) %>% as.data.frame() # works!

# Remove some of the extra intermediate dfs
rm(multisurf, multisurf_corrected, multisurf_geoms, multisurf_mp, multisurf_sf, multisurf_simple, geom_types)
gc()

# Sensitivity analysis for raster resolution ----------------------------------------------------------
# Conduct sensitivity analysis for rasterizing at different resolutions
# 1. Create a Function to Filter and Crop Subsets: This function will take 
# PMEP_Section and bounding box as inputs and return a cropped subset.
# 2. Iterate Over PMEP_Sections: Calculate the area for each subset using 
# the function, collect results from each iteration for comparison.

## a. Setup ------------------------------------------------------------
# Drop Z dimension 
substrate <- st_zm(substrate)

# Filter to rock only
substrate_rock <- substrate_xy %>% 
  filter(CMECS_SC_Category_Code == "1.1") %>% 
  filter(PMEP_Zone >= 2 & PMEP_Zone <= 5)

# Overwrite
#substrate_xy <- substrate_xy_rock

## b. Define functions and inputs ---------------------------------------------
# Select a few subsets within each PMEP Section
# (Hard to automate because not all areas contain rock)
bbox1 <- st_bbox(c(xmin = 172000, xmax = 177000, ymin = 1012000, ymax = 1017000), crs = st_crs(substrate)) # 23 - Cape Mendocino to Cape Blanco
bbox2 <- st_bbox(c(xmin = 191000, xmax = 199000, ymin = 822000, ymax = 829000), crs = st_crs(substrate)) # 30 - Cape Mendocino to Point Reyes 
bbox3 <- st_bbox(c(xmin = 354000, xmax = 357000, ymin = 510000, ymax = 514000), crs = st_crs(substrate)) # 31 - Point Reyes to Point Sur
bbox4 <- st_bbox(c(xmin = 460000, xmax = 465000, ymin = 350000, ymax = 355000), crs = st_crs(substrate)) # 32 - Point Sur to Point Arguello including Davidson Se*
bbox5 <- st_bbox(c(xmin = 529000, xmax = 534000, ymin = 230000, ymax = 235000), crs = st_crs(substrate)) # 33 - Point Arguello South including San Juan Seamount
bbox6 <- st_bbox(c(xmin = 547000, xmax = 552000, ymin = 270000, ymax = 275000), crs = st_crs(substrate)) # 40 - Point Conception to Palos Verdes
bbox7 <- st_bbox(c(xmin = 795000, xmax = 800000, ymin = 90000, ymax = 94000), crs = st_crs(substrate)) # 41 - Palos Verdes to US-Mex Border

# Define function for rasterizing at given res and calculating area 
subset_area <- function(section, resolution, bbox){
  # Print the pieces
  print(paste("Section:", section))
  print(paste("Resolution:", resolution))
  
  # Filter to given section
  subset <- attribute %>% 
    filter(PMEP_Section == section) %>% 
    filter(CMECS_SC_Category_Code == cat_code) %>% 
    left_join(., substrate) %>% 
    st_as_sf()
  
  print(paste("Obs:", nrow(subset)))
  
  # Crop substrate to area
  subset <- st_crop(subset, bbox)
  
  print(paste("BBox Obs:", nrow(subset)))
  
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

# Apply function across original boxes 
cat_code <- "1.1"
sections <- c(rep(c(41), times = 5)) 
bboxes <- c(rep(list(bbox7), times = 5))  
resolutions <- c(rep(c(250, 100, 50, 24, 10), each = 1)) # , 100, 50, 30, 25, 15, 10, 5, 2

sections <- c(rep(c(23, 30, 31, 32, 33, 40, 41), times = 5)) 
bboxes <- c(rep(list(bbox1, bbox2, bbox3, bbox4, bbox5, bbox6, bbox7), times = 5))  
resolutions <- c(rep(c(250, 100, 50, 24, 10), each = 7)) # , 100, 50, 30, 25, 15, 10, 5, 2

all_results <- pmap(list(sections, resolutions,  bboxes), subset_area) 

## c. Results  -------------------------------------------------
# Create dataframe of results 
area_compare <- bind_rows(all_results) %>% 
  mutate(#res = as.factor(res),
         section = as.factor(section),
         diff = area/poly_area) %>%  
  mutate(diff_abs = abs(1-area/poly_area)) 

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
#ggsave(file.path(fig.dir, "Step0_pmep_substrate_1.1_rasterize_resolutions.png"))

ggplot(data = average) + 
  geom_point(aes(x = res, y = res_mean)) +
  theme_minimal() +
  labs(x = "Resolution", y = "Average proportional difference from polygon area")
#ggsave(file.path(fig.dir, "Step0_pmep_substrate_1.1_rasterize_avg_resolutions.png"))

# Chat with Anita on 11 Jan 2024 - Could look at when the difference becomes
# statistically significant. Visually, it seems reasonable to choose the 24-30m 
# range, which also lines up nicely with the kelp forest landsat data, which has
# minimum 24m resolution from satellite imagery.

# Rasterize hard substrate @ 24m -------------------------------------------------------
## 1. Create empty raster grid at 24m resolution across entire extent of CA, use
# zones layer to ensure grid is maximum extent and consistent
raster <- raster::raster(zones_ca, resolution = 24, crs = st_crs(zones_ca))

# 2. Create function to rasterize section by section
rasterize_by_section <- function(section, substrate_code){
  print(paste("Section:", section))
  print(paste("Sub code:", substrate_code))
  
  # Select section and layer to rasterize
  subset <- attribute %>% 
    filter(PMEP_Section == section) %>% 
    filter(CMECS_SC_Category_Code == substrate_code) %>% 
    left_join(., substrate) %>% 
    st_as_sf()
  
  print(paste("Subset:", nrow(subset)))
  
  zone_section <- zones_ca %>% 
    filter(PMEP_Section == section)
  
  # Crop empty raster to zone section extent
  raster_crop <- crop(raster, zone_section)
  
  # Rasterize
  rock_raster <- fasterize::fasterize(sf = subset, raster = raster_crop)
  
  # Convert to SpatRaster 
  rock_raster <- rast(rock_raster)
  
  # Write raster to file
  terra::writeRaster(rock_raster, file.path(sub.dir, paste("PMEP_Substrate_", substrate_code, "_24mRes_Section", section, ".tif", sep = "")), 
                     overwrite = T)
  
}

# 3. Apply function to sections
## Drop Z dimension
substrate <- st_zm(substrate)
gc()

## a. Hard substrate ----
sections <- c(23, 30, 31, 32, 33, 40, 41)
substrate_code <- "1.1"

map2(sections, substrate_code, rasterize_by_section) 

## b. Soft substrate ----
## Unconsolidated mineral substrate
sections <- c(23, 30, 31, 32, 33, 40, 41)
substrate_code <- "1.2"
map2(sections, substrate_code, rasterize_by_section) 

## Coarse unconsolidated substrate
# NO OBS FOR SECTIONS: 23, 32
sections <- c(30, 31, 33, 40, 41)
substrate_code <- "1.2.1"
map2(sections, substrate_code, rasterize_by_section) 

## Fine unconsolidated substrate
# NO OBS FOR SECTIONS: 32
sections <- c(23, 30, 31, 33, 40, 41) # # complete; run one by one to avoid crash
substrate_code <- "1.2.2"
map2(sections, substrate_code, rasterize_by_section) 

# Combine into a single raster -----------------------
raster.dir <- file.path(sub.dir, "substrate_rasters")

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
  terra::writeRaster(merged, file.path(raster.dir, paste("PMEP_Substrate_", layer_name, "_24mRes_Full.tif", sep = "")), overwrite = T)
}

# Combine rasters and export (run gc between to save memory space)
combine_sections(layer_name = "1.1")
combine_sections(layer_name = "1.2")
combine_sections(layer_name = "1.2.1")
combine_sections(layer_name = "1.2.2")



