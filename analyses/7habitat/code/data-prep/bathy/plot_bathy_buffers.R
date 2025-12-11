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
# slope <- rast(file.path(proc.dir, "combined_30m_bathy_ca_slope.tif"))
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
terrain <- c(bathy_30m, tri)
names(terrain) <- c("depth", "tri")


# Examine and plot sites ---------------

# Create subset to process in batches
#focal_sites <- sites %>% filter(habitat == "Kelp forest" & !is.na(site_type))
#focal_sites <- sites %>% filter(habitat == "Rocky reef")
focal_sites <- sites %>% filter(habitat == "Surf zone")

# Set tmap specifications
tmap_mode("plot")
tmap_options(component.autoscale = F)

# To check just one site:
my_site <- sites %>% filter(str_detect(site, "Strand")) %>% pull(site)

# Process each site
for (my_site in unique(focal_sites$site)) {
  
  # Filter for the current site
  site_point <- sites %>% filter(site == !!my_site)
  
  # Calculate buffer to find raster in proxmity of the site
  site_500 <- st_buffer(site_point, dist = 500)
  site_25 <- st_buffer(site_point, dist = 25)
  
  # Convert to vector object and reproject to raster CRS
  sites_vect <- vect(site_500)
  sites_vect <- project(sites_vect, crs(bathy_30m))
  
  # Crop raster to the site
  raster_site <- crop(terrain, buffer(sites_vect, 30))
  
  # Plot
  plot <-
    tm_shape(raster_site) +
    tm_raster(col.scale = tm_scale_continuous(values = "-blues")) +
    tm_shape(site_25) +
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
