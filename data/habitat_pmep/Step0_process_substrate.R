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

# Read Data ----------------------------------------------------------------------------
# Check layer names
st_layers(dsn=gdb.dir)

# Read zones
zones <- read_sf(dsn = gdb.dir, layer = "West_Coast_USA_Nearshore_Zones")

# Read substrate component from CA
substrate_ca <- read_sf(dsn = gdb.dir, 
                        query = "SELECT * FROM West_Coast_USA_Nearshore_CMECS_Substrate_Habitat WHERE State = 'CA'")

# Read MPA polygons
mpas <- readRDS(file.path(sync.dir, "/gis_data/processed/CA_MPA_polygons.Rds")) %>% 
  # Transform to match CRS of substrate data
  st_transform(., st_crs(substrate_ca))

one_nsid <- read_sf(dsn = gdb.dir,
                    query = "SELECT * From West_Coast_USA_Nearshore_CMECS_Substrate_Habitat WHERE NS_PolyID = '12189'")


## Create simple dfs  -----------------------------------------------
zones_simple <- zones %>% 
  st_drop_geometry() %>% 
  filter(State == "CA")

substrate_ca_simple <- substrate_ca %>% st_drop_geometry()

mpas_simple <- mpas %>% st_drop_geometry()

# Process multisurface geometries ----------------------------------
# There is an issue with several multisurface geometries - R cannot handle
# many spatial operations with these geometries.
geom_types <- st_geometry_type(substrate_ca, by_geometry = TRUE) %>% as.data.frame()

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
substrate_xy <- st_zm(substrate_ca)

# Filter to rock only
substrate_xy_rock <- substrate_xy %>% 
  filter(CMECS_SC_Category_Code == "1.1") %>% 
  filter(PMEP_Zone >= 2 & PMEP_Zone <= 5)

# Overwrite
#substrate_xy <- substrate_xy_rock

## b. Define functions and inputs ---------------------------------------------
# Select a few subsets within each PMEP Section
# (Hard to automate because not all areas contain rock)
bbox1 <- st_bbox(c(xmin = 172000, xmax = 177000, ymin = 1012000, ymax = 1017000), crs = st_crs(substrate_xy)) # 23 - Cape Mendocino to Cape Blanco
bbox2 <- st_bbox(c(xmin = 191000, xmax = 199000, ymin = 822000, ymax = 829000), crs = st_crs(substrate_xy)) # 30 - Cape Mendocino to Point Reyes 
bbox3 <- st_bbox(c(xmin = 354000, xmax = 357000, ymin = 510000, ymax = 514000), crs = st_crs(substrate_xy)) # 31 - Point Reyes to Point Sur
bbox4 <- st_bbox(c(xmin = 460000, xmax = 465000, ymin = 350000, ymax = 355000), crs = st_crs(substrate_xy)) # 32 - Point Sur to Point Arguello including Davidson Se*
bbox5 <- st_bbox(c(xmin = 529000, xmax = 534000, ymin = 230000, ymax = 235000), crs = st_crs(substrate_xy)) # 33 - Point Arguello South including San Juan Seamount
bbox6 <- st_bbox(c(xmin = 547000, xmax = 552000, ymin = 270000, ymax = 275000), crs = st_crs(substrate_xy)) # 40 - Point Conception to Palos Verdes
bbox7 <- st_bbox(c(xmin = 795000, xmax = 800000, ymin = 90000, ymax = 95000), crs = st_crs(substrate_xy)) # 41 - Palos Verdes to US-Mex Border

# Define function for rasterizing at given res and calculating area 
subset_area <- function(section, bbox, resolution){
  # Print the pieces
  print(paste("Section:", section))
  print(paste("Resolution:", resolution))
  
  # Filter to given section
  subset <- substrate_xy %>% 
    filter(PMEP_Section == section)

  # Crop substrate to area
  subset_crop <- st_crop(subset, bbox)
  
  # Calculate polygon area
  poly_area <- sum(st_area(subset_crop)) %>%  as.vector() 
  print(paste("Polygon area:", poly_area))
  
  # Rasterize
  substrate_raster <- raster::raster(subset_crop, resolution = resolution)
  rock_raster <- fasterize::fasterize(sf = subset_crop, raster = substrate_raster)
  
  # Calculate area
  area <- rock_raster %>% 
    as.data.frame() %>% 
    filter(!is.na(layer)) %>% 
    summarize(area = n() * resolution * resolution) %>% 
    mutate(res = resolution) %>% 
    mutate(section = section) %>% 
    mutate(poly_area = poly_area)
  
  return(area)
}

# Apply function across original boxes (all but box #2, gives error)
sections <- c(rep(c(23, 31, 32, 33, 40, 41), times = 10)) # 30 omitted
bboxes <- c(rep(list(bbox1, bbox3, bbox4, bbox5, bbox6, bbox7), times = 10)) # bbox2 omitted
resolutions <- c(rep(c(500, 250, 100, 50, 30, 25, 15, 10, 5, 2), each = 6))

all_results <- pmap(list(sections, bboxes, resolutions), subset_area) 

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
## By section ----
## 1. Create empty raster grid at 24m resolution across entire extent of CA, use
# zones layer to ensure grid is maximum extent and consistent
zones_simple <- zones %>% st_drop_geometry()
zones_ca <- st_zm(zones) %>% 
  filter(State == "CA")

raster <- raster::raster(zones_ca, resolution = 24, crs = st_crs(zones_ca))

# 2. Create function to rasterize section by section
rasterize_by_section <- function(section, substrate_code){
  print(paste("Section:", section))
  print(paste("Sub code:", substrate_code))
  
  # Select section to rasterize
  subset <- substrate_xy %>% 
    filter(PMEP_Section == section) %>% 
    filter(CMECS_SC_Category_Code == substrate_code)
  
  zone_section <- zones_ca %>% 
    filter(PMEP_Section == section)
  
  # Crop empty raster to zone section extent
  raster_crop <- crop(raster, zone_section)
  
  # Rasterize
  rock_raster <- fasterize::fasterize(sf = subset, raster = raster_crop)
  
  # Plot rasterized section
  # raster_plot <- ggplot() +
  #   geom_sf(data = subset, mapping = aes(), color = "black", fill = "black") +
  #   geom_tile(rock_raster %>% as.data.frame(xy = T) %>% filter(!is.na(layer)), mapping = aes(x = x, y = y, fill = layer), show.legend = F) +
  #   labs(title = paste("Section:", section),
  #        x = NULL,
  #        y = NULL)
  # print(raster_plot)
  
  # Compare to polygon area
  area_poly <- sum(st_area(subset)) %>% as.vector()
  area_rast <- rock_raster %>% 
    as.data.frame() %>% 
    filter(!is.na(layer)) %>% 
    summarize(area = n() * 24 * 24)
  
  print(area_rast/area_poly)
  
  # Convert to SpatRaster 
  rock_raster <- rast(rock_raster)
  
  # Write raster to file
  terra::writeRaster(rock_raster, file.path(out.dir, paste("PMEP_Substrate_", substrate_code, "_24mRes_Section", section, ".tif", sep = "")), 
                     overwrite = T)
  
}

# 3. Apply function to sections
## Drop Z dimension
substrate_xy <- st_zm(substrate_ca)

## a. Hard substrate ----
sections <- c(23, 30, 31, 32, 33, 40, 41)
substrate_code <- "1.1"

map2(sections, substrate_code, rasterize_by_section) 

## b. Soft substrate ----
substrate_code <- "1.2"
map2(sections, substrate_code, rasterize_by_section) 



# Combine into a single raster -----------------------
# List of filenames for the substrate raster sections
filenames <- list.files(out.dir, pattern="*.tif", full.names=TRUE)

# Read each of the raster sections
hard_sections <- lapply(filenames, rast)

# Create spat raster collection
hard_sections <- sprc(hard_sections) 

# Merge into a single raster
hard_raster <- terra::merge(hard_sections)

# Write to file
terra::writeRaster(hard_raster, file.path(out.dir, "PMEP_Substrate_1.1_24mRes_Full.tif"), overwrite = T)


# Process full substrate component --------------
substrate_processed <- substrate_ca %>% 
  filter(CMECS_SC_Category_Code < 3) %>%  # drop anthropogenic and unclassified polygons
  dplyr::select(PMEP_Region:CMECS_SC_Category_Code, PMEP_NSID, NS_PolyID, Shape_Length, Shape_Area)

substrate_processed_simple <- substrate_processed %>% 
  st_drop_geometry()

# Plots --------
ggplot() +
  geom_sf(data = zone_section, mapping = aes(), color = "black", fill = "transparent") +
  geom_tile(raster_crop %>% as.data.frame(xy = T), 
            mapping = aes(x = x, y = y, fill = "blue", alpha = 0.5, color = "blue"), show.legend = F)

ggplot() + 
  geom_sf(data = zone_section, mapping = aes(), color = "black", fill = "transparent") +
  geom_sf(data = subset, mapping = aes(), color = "blue", fill = "blue", alpha = 0.5)

ggplot() + 
  geom_sf(data = subset, mapping = aes(), color = "black", fill = "black") +
  geom_tile(rock_raster %>% as.data.frame(xy = T) %>% filter(!is.na(layer)), 
            mapping = aes(x = x, y = y, fill = layer), show.legend = F) +
  labs(x = NULL,
       y = NULL)



# Subset to only the data within MPAs ------------------------------------------

## Create raster object of the substrate layer
substrate_raster <- raster::raster(substrate_ca)

# Use st_intersection to create a subset of only those data within MPAs
mpa_intersect <- st_intersection(substrate_ca, mpas)
saveRDS(mpa_intersect, file.path(sync.dir, "habitat_pmep", "mpa_substrate_intersection.Rds"))

# This approach ends up splits polygons based on MPA borders (e.g. greater number 
# of observations because adjacent MPAs will split one poly into two)
mpa_intersect_simple <- mpa_intersect %>% st_drop_geometry()

obs_per_mpa <- mpa_intersect_simple %>% 
  group_by(name) %>% 
  summarize(n_obs = n())

no_substrate <- mpas_simple %>% 
  filter(!(name %in% obs_per_mpa$name))


# Calculate area of each polygon within MPAs
mpa_intersect$mpa_area <- st_area(mpa_intersect)

mpa_intersect_simple <- mpa_intersect %>% st_drop_geometry()

mpa_totals <- mpa_intersect_simple %>% 
  mutate(CMECS_SC_Broad = if_else(CMECS_SC_Category_Code < 1.5,
                                  str_extract(CMECS_SC_Category_Code, "^.{3}"), CMECS_SC_Category_Code)) %>% 
  group_by(CMECS_SC_Broad) %>% 
  summarize(mpa_area = sum(mpa_area),
            mpa_area_km = round(mpa_area/(1*10^6), 3))

state_totals <- substrate_ca_simple %>% 
  mutate(CMECS_SC_Broad = if_else(CMECS_SC_Category_Code < 1.5, 
                                  str_extract(CMECS_SC_Category_Code, "^.{3}"), CMECS_SC_Category_Code)) %>% 
  group_by(CMECS_SC_Broad) %>% 
  summarize(state_area = sum(Shape_Area),
            state_area_km = round(state_area/(1e6), 3))

representation <- full_join(mpa_totals, state_totals) %>% 
  mutate(proportion = mpa_area/state_area) 


library(RColorBrewer)
hab_colors <- c("red4", # anthro
                "burlywood3", "burlywood2", #coarse, fine
                "tan4", #rock
                "white", #unclassified"
                "burlywood1") #unconsolidated





