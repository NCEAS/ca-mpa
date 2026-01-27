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

# # Explore what's wrong with PC04
# # Point conception site is wrong - let them know but move on since we don't include...
# pc_sites <- sites %>% filter(str_detect(site, "PC"))
# 
# # Filter for the current site
# site_point <- sites %>% filter(site %in% pc_sites$site)
# 
# # Calculate buffer to find raster in proxmity of the site
# site_500 <- st_buffer(site_point, dist = 500)
# site_25 <- st_buffer(site_point, dist = 25)
# 
# # Convert to vector object and reproject to raster CRS
# sites_vect <- vect(site_500)
# sites_vect <- project(sites_vect, crs(bathy_30m))
# 
# # Crop raster to the site
# raster_site <- crop(terrain, sites_vect)
# 
# # Plot
# tmap_mode("view")
# plot <-
#   tm_shape(raster_site) +
#   tm_raster(col.scale = tm_scale_continuous(values = "-blues"),
#             col.legend = tm_legend(title = "")) +
#   tm_shape(site_25) +
#   tm_borders(col = "red") +
#   tm_shape(site_500) +
#   tm_borders(col = "red") +
#   tm_layout(
#     outer.margins = c(0.001, 0.001, 0.001, 0.001),
#     frame = FALSE,
#     legend.outside = FALSE,
#     legend.frame = FALSE,
#     legend.text.size = 1
#   )
# plot



# Load the coastline
coast <- sf::st_read(file.path("/home/shares/ca-mpa/data/sync-data/gis_data/raw", "Coastn83", "coastn83.shp")) %>% 
  st_union() %>% 
  st_transform(., crs = 26910)

my_site <- sites$site[531]

# Filter for the current site
site_point <- sites %>% filter(site == !!my_site)

# Calculate buffer to find raster in proxmity of the site
site_500 <- st_buffer(site_point, dist = 500)

# assume coast and site_point exist and share or can be transformed to a projected CRS
target_crs <- 26910
coast_u <- st_transform(st_union(coast), target_crs)
site_pt <- st_transform(site_point, target_crs)

# nearest point on coast to the site (point on coast)
np_line <- st_nearest_points(coast_u, site_pt)      # LINESTRING: coast -> site
coast_pt <- st_cast(np_line, "POINT")[1]

# coordinates (projected, meters)
site_xy  <- st_coordinates(site_pt)[1, ]
coast_xy <- st_coordinates(coast_pt)[1, ]

# direction vector from coast -> site, unitized
v <- site_xy - coast_xy
u <- v / sqrt(sum(v^2))

# endpoints 500 m each side of site along that direction
half <- 500
p1 <- site_xy - u * half
p2 <- site_xy + u * half

# transect
transect <- st_sfc(st_linestring(rbind(p1, p2)), crs = st_crs(site_pt))
transect <- st_sf(geometry = transect)

# quick plot check
plot(st_geometry(site_500), col = NA, border = "grey")
plot(st_geometry(coast_u), add = TRUE)
plot(st_geometry(site_pt), add = TRUE, pch = 16, col = "red")
plot(st_geometry(transect), add = TRUE, lwd = 2, col = "black")

# sample regular points along the transect
n_samples <- 100
pts <- st_line_sample(transect, n = n_samples, type = "regular") %>% st_cast("POINT")
pts_sf <- st_sf(geometry = pts)

# extract bathymetry values at sample points
vals_df <- terra::extract(terrain$depth, vect(pts_sf)) %>% as.data.frame()
# terra::extract returns first column ID; next column(s) are raster values — pick the first raster layer

val_col <- names(vals_df)[2]
pts_sf$depth_raw <- vals_df[[val_col]]

# drop NA samples (e.g., outside raster extent)
pts_sf <- pts_sf %>% filter(!is.na(depth_raw))

# compute cumulative distance along transect (projected CRS -> meters)
coords <- st_coordinates(pts_sf)
dists_m <- c(0, cumsum(sqrt(rowSums((coords[-1,1:2] - coords[-nrow(coords),1:2])^2))))
pts_sf$dist_m <- dists_m

# normalize depth sign so positive = depth (meters below sea surface)
# heuristic: if median depth is negative, flip sign
if (median(pts_sf$depth_raw, na.rm = TRUE) < 0) {
  pts_sf$depth <- -pts_sf$depth_raw
} else {
  pts_sf$depth <- pts_sf$depth_raw
}

# simple ggplot depth profile (distance in meters)
ggplot(pts_sf, aes(x = dist_m, y = depth)) +
  geom_line() +
  scale_y_reverse() +
  labs(x = "Distance (m)", y = "Depth (m, positive downward)") +
  theme_minimal()


