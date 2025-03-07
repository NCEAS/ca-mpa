# Clean Monitoring Sites
# Cori Lopazanski
# Dec 2024

# About: This script was adapted from an archived script in the original processing code.

# FYI This script is recursive because of earlier errors in the code, but is only important
# for plotting the habitat and the new site locations! Habitat is only needed for the surf zone 
# sites at the end.

# Setup -----------------------------------------------------------------------------------
library(tidyverse)
library(patchwork)
library(sf)

rm(list = ls())

hab.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed_v2"
gis.dir <- "/home/shares/ca-mpa/data/sync-data/gis_data/processed"

# The smaller sized buffers are excluded (expect b/c no overlap due to the given
# lat/lon of the site) so will examine the smallest one available
habitat <- readRDS(file.path(hab.dir, "combined/buffers", paste0("habitat_buffers_", 25, "m.Rds"))) %>% 
  filter(depth_zone != "landward")

# Load the cleaned monitoring site table
sites <- readRDS(file.path("/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024", "site_locations.Rds")) %>% 
  st_as_sf(., coords = c("lon_dd", "lat_dd"), remove = T, crs = 4326) %>% 
  st_transform(., crs = 26910) 

# Load the state waters polygon
state_waters_poly <- readRDS(file.path(gis.dir, "CA_state_waters_polygons.Rds")) %>% 
  st_transform(., crs = 26910) %>% 
  st_buffer(., dist = -5) # shrink to ensure sites line up with habitat edges

# Load the site table with the errors
# sites_review <- readRDS(file.path(hab.dir, "review", "sites_review.Rds"))
coast <- sf::st_read(file.path("/home/shares/ca-mpa/data/sync-data/gis_data/raw", "Coastn83", "coastn83.shp")) %>% 
  st_union() %>% 
  st_transform(., crs = 26910)

# Check which site points are not within state waters (on land)
inside_indices <- st_within(sites, state_waters_poly)  
inside_logical <- lengths(inside_indices) > 0  
sites_outside <- sites[!inside_logical, ] %>% 
  filter(!habitat %in% c("Deep reef", "Rocky reef", "Kelp forest")) # these are outside state waters but still in the actual water


# Fix Surf Sites ----------------------------------------------------------------------------

## 1. Sites that are too far offshore --------------------------------------------------------
sites_surf_offshore <- sites %>% 
  filter(habitat == "Surf zone" & !(site %in% sites_outside$site))

# Create a list to store plots and a dataframe to store corrected points
plots <- list()
corrected_points_list <- list()
my_site <- sites_surf_offshore$site[7]

# Process each site
for (my_site in unique(sites_surf_offshore$site)) {
  
  # Filter for the current site
  site_point <- sites %>% filter(site == !!my_site)
  
  # Filter for habitat in proximity of that site
  habitat_df <- habitat %>% filter(site == !!my_site)
  habitat_poly <- st_union(habitat_df)  # Ensure a single polygon
  
  # Get the boundary (edges) of the water polygon
  water_boundary <- st_cast(state_waters_poly, "MULTILINESTRING")
  
  # Move to the nearest point 5m from MHW line
  nearest_lines <- st_nearest_points(site_point, water_boundary)
  
  # Extract the nearest edge point
  nearest_edge_point <- st_cast(nearest_lines, "POINT")[2]
  
  # Update site_point with the nearest edge geometry
  site_point_corrected <- site_point %>%
    mutate(geometry = nearest_edge_point)
  
  # Append corrected points to the list
  corrected_points_list[[my_site]] <- site_point_corrected
  
  # Create a bounding box around the site and corrected points
  site_bbox <- st_bbox(st_buffer(st_union(st_geometry(habitat_df), st_geometry(site_point_corrected)), dist = 100)) 
  
  # Create a buffer of the site to plot at 25m
  site_25 <- st_buffer(site_point, dist = 25)
  
  # Create the plot
  plot <- ggplot() +
    geom_sf(data = coast, color = "blue", lwd = 0.6) +
    geom_sf(data = state_waters_poly, fill = "lightblue", alpha = 0.5) +
    geom_sf(data = habitat_df, aes(fill = habitat_class)) +
    geom_sf(data = site_25, color = "green") +
    geom_sf(data = site_point_corrected, color = "red", size = 2) +
    geom_sf(data = site_point, color = "black", size = 2) +
    coord_sf(
      xlim = c(site_bbox["xmin"], site_bbox["xmax"]),
      ylim = c(site_bbox["ymin"], site_bbox["ymax"])
    ) +
    labs(fill = NULL) +
    ggtitle(paste("Site:", my_site)) +
    theme_minimal() +
    scale_fill_manual(
      values = c(
        "Soft Bottom" = "tan",
        "Hard Bottom" = "saddlebrown",
        "Aquatic Vegetation Bed" = "lightgreen",
        "Aquatic Vascular Vegetation" = "darkgreen",
        "Seagrass" = "darkgreen"
      )
    )
  
  # Store the plot
  plots[[my_site]] <- plot
}

wrap_plots(plots, ncol = 4)

# Combine all corrected points into a single dataframe
corrected_points_df <- bind_rows(corrected_points_list)

# Merge corrected points back into the original sites dataframe
sites_updated <- sites %>%
  filter(!site %in% sites_surf_offshore$site) %>% 
  bind_rows(corrected_points_df)


## 2. Handle the surf sites on land ------ 
# Create a list to store plots and a dataframe to store corrected points
plots <- list()
corrected_points_list <- list()
my_site <- sites_outside$site[5]

# Process each site
for (my_site in unique(sites_outside$site)) {
  
  # Filter for the current site
  site_point <- sites_outside %>% filter(site == !!my_site)
  
  # Filter for habitat in proximity of that site
  habitat_df <- habitat %>% filter(site == !!my_site)
  habitat_poly <- st_union(habitat_df)
  
  # Get the boundary (edges) of the water polygon
  water_boundary <- st_cast(state_waters_poly, "MULTILINESTRING")
  
  ## Check if habitat_poly is completely within water_boundary
  if (st_within(habitat_poly, state_waters_poly, sparse = FALSE)[1, 1]) {
    # Habitat is inside the water boundary → Find nearest point on habitat
    nearest_lines <- st_nearest_points(site_point, habitat_poly)
  } else {
    # Habitat extends beyond the water boundary → Find nearest point on water_boundary
    nearest_lines <- st_nearest_points(site_point, water_boundary)
  }
  
  # Find the nearest line from each point to habitat edge
  #nearest_lines <- st_nearest_points(site_point, habitat_poly)
  nearest_edge_point <- st_cast(nearest_lines, "POINT")[2]
  
  # Update site_point with the nearest edge geometry
  site_point_corrected <- site_point %>%
    mutate(geometry = nearest_edge_point)
  
  # Append corrected points to the list
  corrected_points_list[[my_site]] <- site_point_corrected
  
  # Create a bounding box around the site and corrected points
  site_bbox <- st_bbox(st_buffer(st_union(st_geometry(habitat_df), st_geometry(site_point)), dist = 100)) 
  
  # Create a buffer of the site to plot at 25m
  site_25 <- st_buffer(site_point, dist = 25)
  
  # Create the plot
  plot <- ggplot() +
    geom_sf(data = coast, color = "blue", lwd = 0.6) +
    geom_sf(data = state_waters_poly, fill = "lightblue", alpha = 0.5) +
    geom_sf(data = habitat_df, aes(fill = habitat_class)) +
    geom_sf(data = site_25, color = "green") +
    geom_sf(data = site_point_corrected, color = "red", size = 2) +
    geom_sf(data = site_point, color = "black", size = 2) +
    coord_sf(
      xlim = c(site_bbox["xmin"], site_bbox["xmax"]),
      ylim = c(site_bbox["ymin"], site_bbox["ymax"])
    ) +
    labs(fill = NULL) +
    ggtitle(paste("Site:", my_site)) +
    theme_minimal() +
    scale_fill_manual(
      values = c("Soft Bottom" = "tan","Hard Bottom" = "saddlebrown", "Aquatic Vegetation Bed" = "lightgreen",
                 "Aquatic Vascular Vegetation" = "darkgreen", "Seagrass" = "darkgreen"))
  
  # Store the plot
  plots[[my_site]] <- plot
}

# Combine all corrected points into a single dataframe
corrected_points_df <- bind_rows(corrected_points_list)

wrap_plots(plots, ncol = 5) # 15 sites moved from land

# Merge corrected points back into the original sites dataframe
sites_updated <- sites_updated %>%
  filter(!site %in% sites_outside$site) %>% 
  bind_rows(corrected_points_df)

# Export the points
saveRDS(sites_updated,
        file.path("/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024", "site_locations_corrected.Rds")) 


