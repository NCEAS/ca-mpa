# Plot site and buffer
# Cori Lopazanski (lopazanski@bren.ucsb.edu)
# December 2024

library(tidyverse)
library(sf)
library(terra)
library(tmap)

rm(list = ls())
gc()

ltm.dir  <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"
proc.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_anita/processed"
fig.dir <- "~/ca-mpa/analyses/7habitat/figures/site-plots/bathy"

# General Data Prep  -----------------------------------------------------------

# Read the combined bathymetry raster (use CA version)
bathy_30m <- rast(file.path(proc.dir, "combined_30m_bathy_ca.tif"))

# Read the slope and TRI data (calculated from the combined bathy raster)
slope <- rast(file.path(proc.dir, "combined_30m_bathy_ca_slope.tif"))
tri   <- rast(file.path(proc.dir, "combined_30m_bathy_ca_tri.tif"))

# Read the LTM sites
sites <- readRDS(file.path("/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024",
                           "site_locations_corrected.Rds")) %>%
  arrange(site) %>%
  mutate(site_id = row_number()) # unique site ID for joining later

# Project sites to a linear CRS
sites_proj <- st_transform(sites, crs = crs(bathy_30m))
sites_vect <- vect(sites_proj)

# Read the depth data
depth_metrics <-  readRDS(file.path(ltm.dir, "site_depth_agg.Rds"))

# Load the coastline
coast <- sf::st_read(file.path("/home/shares/ca-mpa/data/sync-data/gis_data/raw", "Coastn83", "coastn83.shp")) %>% 
  st_union() %>% 
  st_transform(., crs = 26910)

# Define function to calculate buffers  ----------------------------------------

# Create terrain stack
terrain <- c(bathy_30m, slope, tri)
names(terrain) <- c("depth", "slope", "tri")


# Examine and plot sites ---------------

# Create subset to process in batches
#focal_sites <- sites %>% filter(habitat == "Kelp forest" & !is.na(site_type))
#focal_sites <- sites %>% filter(habitat == "Rocky reef")
focal_sites <- sites %>% filter(habitat == "Surf zone")

# Figure out what's up with PC
focal_sites <-  sites %>% filter(str_detect(site, "CABRILLO|CASPAR")) %>% pull(site)

# Set tmap specifications
tmap_mode("plot")
tmap_options(component.autoscale = F)

# To check just one site:
my_site <- sites %>% filter(str_detect(site, "Strand")) %>% pull(site)
my_site <- focal_sites[[6]]



# Process each site
for (my_site in unique(focal_sites$site)) {
  
  # Filter for the current site
  site_point <- sites %>% filter(site == !!my_site)
  
  # Calculate buffer to find raster in proxmity of the site
  site_500 <- st_buffer(site_point, dist = 500)
  site_100 <- st_buffer(site_point, dist = 100)
  site_25 <- st_buffer(site_point, dist = 25)
  
  # Convert to vector object and reproject to raster CRS
  sites_vect <- vect(site_100)
  sites_vect <- project(sites_vect, crs(bathy_30m))
  
  # Crop raster to the site
  raster_site <- crop(terrain, terra::buffer(sites_vect, 30))
  
  # Plot
  plot <-
    tm_shape(raster_site) +
    tm_raster(col.scale = tm_scale_continuous(values = "-blues")) +
    tm_shape(site_25) +
    tm_borders(col = "red") +
    tm_shape(site_100) +
    tm_borders(col = "red") +
    tm_shape(site_500) +
    tm_borders(col = "red") +
    tm_title(paste(my_site), position = tm_pos_on_top(), frame = F, size = 1) +
    tm_layout(
      outer.margins = c(0.01, 0.01, 0.01, 0.01),
      frame = FALSE,
      legend.outside = FALSE,
      legend.frame = FALSE,
      legend.text.size = 0.5,
      panel.show = F
    )
  
  plot
  tmap_save(tm = plot,
            filename = file.path(fig.dir, paste0(site_point$habitat, " - ", site_point$site, ".jpg")),
            width = 5, height = 8, units = "in", dpi = 300)
}


# Plot the depth profiles -----------------------------------------------------------------------------------

# Simplify coastline for a smoother edge
coast_simple <- st_simplify(coast, dTolerance = 50) %>% smoothr::smooth() %>% 
  st_cast(., "LINESTRING") 

coast_simple$length <- as.numeric(st_length(coast_simple))
coast_simple <- coast_simple[coast_simple$length >= 100]
coast_simple <- st_union(coast_simple)

# Pick a site
my_site <- sites$site[[49]]

# Filter for the current site
site_point <- sites %>% filter(site == !!my_site)

# Calculate buffer to find raster in proxmity of the site
site_500 <- st_buffer(site_point, dist = 500)

# Nearest point on coast to the site (point on coast)
coast_point <- st_nearest_points(coast_simple, site_point) %>%   # LINESTRING: coast -> site
  st_cast(., "POINT")[1] %>% st_as_sf() # Get just the point

# Create direction vector from coast -> site 
site_xy <- st_coordinates(site_point)[1, 1:2]
v <- (site_xy - st_coordinates(coast_point)[1, 1:2])
u <- v / sqrt(sum(v^2))

# Build transect from those points
transect <- st_sfc(st_linestring(rbind(site_xy - u * 500, 
                                       site_xy + u * 500)), 
                   crs = st_crs(site_point)) %>% 
  st_transform(., crs = crs(terrain)) # transform to match raster 

# Quick plot check
plot(st_geometry(site_500), col = NA, border = "grey")
plot(st_geometry(coast), add = TRUE)
plot(st_geometry(coast_simple), col = "blue", add = TRUE)
plot(st_geometry(site_point), add = TRUE, pch = 16, col = "red")
plot(st_geometry(transect), add = TRUE, lwd = 2, col = "black")


# Sample along transect
sample_pts <- st_line_sample(transect, n = 500, type = "regular") %>% st_cast("POINT") %>% st_sf()
sample_xy <- st_coordinates(sample_pts)

sample_pts$dist_m <- c(0, cumsum(sqrt(rowSums((sample_xy[-1,1:2] - sample_xy[-nrow(sample_xy),1:2])^2)))) # distances along transect
sample_pts$depth_m <- terra::extract(terrain, vect(sample_pts)) %>% as.data.frame() %>% pull(depth) # extract depth values

# Midpoint will be closest to site - reset as zero
sample_pts$dist_m <- sample_pts$dist_m - sample_pts$dist_m[length(sample_pts$dist_m)/2]

sample_pts <- sample_pts %>% 
  mutate(depth_m = depth_m * -1) %>% 
  filter(!is.na(depth_m))

# Plot
site_depth <- sample_pts %>% filter(dist_m == 0) 

ggplot(sample_pts, aes(x = dist_m, y = depth_m)) +
  geom_smooth(method = "loess", se = FALSE, span = 0.085, col = "black", linewidth = 0.8) + 
  geom_point(data = site_depth, aes(x = dist_m, y = depth_m), size = 2) +
  scale_y_reverse() +
  theme_classic() +
  labs(x = "Distance from site (m)", y = "Depth (m)",
       title = paste(my_site))



sample_transects <- function(my_site, sites_sf, coast_sf, terrain_raster) {
  
  # Filter for the current site
  site_point <- sites %>% filter(site == !!my_site)
  
  # Calculate buffer to find raster in proxmity of the site
  site_500 <- st_buffer(site_point, dist = 500)
  
  # Nearest point on coast to the site (point on coast)
  coast_point <- st_nearest_points(coast_simple, site_point) %>%   # LINESTRING: coast -> site
    st_cast(., "POINT")[1] %>% st_as_sf() # Get just the point
  
  # Create direction vector from coast -> site 
  site_xy <- st_coordinates(site_point)[1, 1:2]
  v <- (site_xy - st_coordinates(coast_point)[1, 1:2])
  u <- v / sqrt(sum(v^2))
  
  # Build transect from those points
  transect <- st_sfc(st_linestring(rbind(site_xy - u * 500, 
                                         site_xy + u * 500)), 
                     crs = st_crs(site_point)) %>% 
    st_transform(., crs = crs(terrain_raster)) # transform to match raster 
  
  # Sample along transect
  sample_pts <- st_line_sample(transect, n = 500, type = "regular") %>% st_cast("POINT") %>% st_sf()
  sample_xy <- st_coordinates(sample_pts)
  sample_pts$dist_m <- c(0, cumsum(sqrt(rowSums((sample_xy[-1,1:2] - sample_xy[-nrow(sample_xy),1:2])^2)))) # distances along transect
  sample_pts$depth_m <- terra::extract(terrain, vect(sample_pts)) %>% as.data.frame() %>% pull(depth) # extract depth values
  
  # Midpoint will be closest to site - reset as zero
  sample_pts$dist_m <- sample_pts$dist_m - sample_pts$dist_m[length(sample_pts$dist_m)/2]
  
  sample_pts <- sample_pts %>% 
    mutate(depth_m = depth_m * -1,
           site = paste(my_site)) %>% 
    filter(!is.na(depth_m))
  
  # Return the object
  sample_pts
  
}

my_sites <- sites$site[sites$habitat == "Surf zone"]  
my_sites <- sites$site[sites$habitat == "Kelp forest"]
my_sites <- sites$site[sites$habitat == "Rocky reef"]

my_sites <- sites %>% 
  filter(habitat == "Rocky reef") %>% 
#  filter(site %in% data_kelp$site) %>% 
  sample_n(42) %>% 
  pull(site)

sample_depths <- map_dfr(my_sites, ~ sample_transects(.x, sites, coast_simple, terrain))

sample_metrics <- depth_metrics %>% 
  filter(site %in% sample_depths$site)

sample_depths2 <- sample_depths %>% 
  mutate(dist_m = case_when(site %in% c("Mad River Reference",
                                        "Samoa MPA",
                                        "South Campus MPA",
                                        "Spanish Bay MPA",
                                        "Whalers Cove MPA") ~ dist_m*-1,
                            T~dist_m)) %>% 
  left_join(sample_metrics) %>% 
  mutate(site = fct_reorder(site, relief_500))

ggplot(sample_depths2 %>% 
         filter(between(dist_m, -250, 250)), aes(x = dist_m, y = depth_m)) +
  geom_smooth(method = "loess", se = FALSE, span = 0.085, linewidth = 0.8, color = "black") +
  geom_point(data = filter(sample_depths2, dist_m == 0), aes(x = dist_m, y = depth_m), size = 2) +
  scale_y_reverse() +
  theme_minimal() +
  labs(x = "Distance from site (m)", y = "Depth (m)") +
  facet_wrap(~ site, scales = "free_x")


