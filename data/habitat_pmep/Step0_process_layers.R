# Explore & Process PMEP Habitat Data (Substrate + Biotic Component)
# Cori Lopazanski
# November 2023

# About --------------------------------------------------------------------------------
# Read and clean the Pacific Marine and Estuary Partnership data


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
gdb.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/PMEP_Nearshore_Zones_and_Habitat.gdb" # Aurora
fig.dir <- "~/ca-mpa/analyses/7habitat/figures" 

# Read Data ----------------------------------------------------------------------------
# Check layer names
st_layers(dsn=gdb.dir)

# Read zones
zones <- read_sf(dsn = gdb.dir, layer = "West_Coast_USA_Nearshore_Zones")
zones_simple <- zones %>% 
  st_drop_geometry() %>% 
  filter(State == "CA")

# Read substrate component from CA
substrate_ca <- read_sf(dsn = gdb.dir, 
                        query = "SELECT * FROM West_Coast_USA_Nearshore_CMECS_Substrate_Habitat WHERE State = 'CA'")

# Create simple version without spatial data
substrate_ca_simple <- substrate_ca %>% st_drop_geometry()

# Read MPA polygons
mpas <- readRDS(file.path(sync.dir, "/gis_data/processed/CA_MPA_polygons.Rds")) %>% 
  # Transform to match CRS of substrate data
  st_transform(., st_crs(substrate_ca))

mpas_simple <- mpas %>% st_drop_geometry()

# Process multisurface geometries -------------------------------------------------------------------
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

# Remove some of the extra for now
rm(multisurf, multisurf_corrected, multisurf_geoms, multisurf_mp, multisurf_sf, multisurf_simple, geom_types)
rm(substrate_ca)

# Rasterize substrate layer -------------------------------------------------------------------
# Drop Z dimension 
substrate_xy <- st_zm(substrate_ca)

# Filter to rock only
substrate_xy <- substrate_xy %>% 
  filter(CMECS_SC_Category_Code == "1.1") %>% 
  filter(PMEP_Zone >= 2 & PMEP_Zone <= 5)


# Conduct sensitivity analysis for rasterizing at different resolutions
# 1. Create a Function to Filter and Crop Subsets: This function will take a PMEP_Section, 
# PMEP_Zone range, and bounding box dimensions as inputs and return a cropped subset.
# 2. Iterate Over PMEP_Sections and Zones: Use nested loops to iterate over the different 
# PMEP_Sections and PMEP_Zone groups.
# 3. Calculate Areas for Each Subset: Within the loop, calculate the area for each subset 
# using the function, collect results from each iteration for comparison.

# Select a few subsets - hard to automate because not all areas contain rock
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

# Plots ----
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

# Rasterize full hard substrate layer --------------------------------------------
# 1. Create empty raster grid at 24m resolution across entire extent of CA
# 2. Select section for rasterizing
# 3. Crop empty raster to section extent
# 4. Rasterize section

# Troubleshooting code for error geom/res ----
section <- 30
resolution <- 24
bbox <- bbox2

# Filter to given section
subset <- substrate_xy %>% 
  filter(PMEP_Section == section)

# Crop substrate to area
subset_crop <- st_crop(subset, bbox)

# Plot the cropped subset
ggplot() + geom_sf(data = subset_crop, mapping = aes(), color = "black", fill = "black") 


# Calculate polygon area
poly_area <- sum(st_area(subset_crop)) %>%  as.vector() 
print(paste("Polygon area:", poly_area))

# Create empty raster
substrate_raster <- raster::raster(subset_crop, resolution = resolution)

# Rasterize - this is what fails sometimes
rock_raster <- fasterize::fasterize(sf = subset_crop, raster = substrate_raster)

ggplot() + 
  geom_sf(data = subset_crop, mapping = aes(), color = "black", fill = "black") +
  geom_tile(rock_raster %>% as.data.frame(xy = T) %>% filter(!is.na(layer)), mapping = aes(x = x, y = y, fill = layer), show.legend = F) +
  labs(x = NULL,
       y = NULL)


# Get geometry types
geom_types <- st_geometry_type(subset_crop, by_geometry = TRUE) %>% as.data.frame()

# Calculate area
area <- rock_raster %>% 
  as.data.frame() %>% 
  filter(!is.na(layer)) %>% 
  summarize(area = n() * resolution * resolution) %>% 
  mutate(res = resolution) %>% 
  mutate(section = section) %>% 
  mutate(poly_area = poly_area)


# Plots
ggplot() +
  geom_sf(data = subset_crop, mapping = aes(), color = "black", fill = "transparent") +
  geom_tile(rock_raster %>% as.data.frame(xy = T) %>% filter(!is.na(layer)), mapping = aes(x = x, y = y, fill = layer)) 

ggplot() +
  geom_sf(data = subset_crop, mapping = aes(), color = "black", fill = "transparent") +
  geom_tile(rock_raster_4 %>% as.data.frame(xy = T) %>% filter(!is.na(layer)), mapping = aes(x = x, y = y, fill = layer)) 












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

### LESS OLD BUT STILL OLD ------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------

# Test with one MPA
carr <- mpas %>% filter(name == "Carrington Point SMR")

# Transform to CRS of substrate data
carr_transform <- st_transform(carr, st_crs(substrate_ca))
st_crs(carr_transform)

# Create 2D version of the substrate data (error with handling the Z dimension potentially)
# no longer necessary
#substrate_ca_2d <- st_zm(substrate_ca, drop = T, what = "ZM")
carr_substrate <- st_intersection(substrate_ca, carr_transform)

library(RColorBrewer)
hab_colors <- c("red4", # anthro
                "burlywood3", "burlywood2", #coarse, fine
                "tan4", #rock
                "white", #unclassified"
                "burlywood1") #unconsolidated


ggplot(carr_substrate)+
  geom_sf(aes(fill = CMECS_SC_Category, color = CMECS_SC_Category)) +
  scale_fill_manual(name = "Habitat Type", 
                    values = hab_colors) +
  scale_color_manual(name = "Habitat Type", 
                    values = hab_colors) 
  #scale_colour_brewer(palette = "BrBG")+
 # scale_fill_brewer(palette = "BrBG")

# multisurface <- substrate_ca %>% 
#   filter(st_geometry_type(., by_geometry = TRUE) == "MULTISURFACE")

# Fix geometries
substrate_ca_2d <- st_make_valid(substrate_ca_2d)

carr_substrate <- st_intersection(substrate_ca_2d, carr_transform)

test <- st_geometry_type(substrate_ca) %>% as.data.frame()



