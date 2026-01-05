# Create plots of each of the habitats 

# Setup -----------------------------------------------------------------------------------

library(tidyverse)
library(patchwork)
library(sf)

rm(list = ls()); gc()

hab.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed_v2"
gis.dir <- "/home/shares/ca-mpa/data/sync-data/gis_data/processed"
fig.dir <- "~/ca-mpa/analyses/7habitat/figures/site-plots/habitat"

# The smaller sized buffers are excluded (expect b/c no overlap due to the given
# lat/lon of the site) so will examine the smallest one available
habitat <- readRDS(file.path(hab.dir, "combined/buffers", paste0("habitat_buffers_", 500, "m.Rds"))) %>% 
  filter(depth_zone != "landward") %>% 
  mutate(habitat_class = factor(habitat_class, levels = c("Soft Bottom", "Hard Bottom", "Aquatic Vegetation Bed", "Aquatic Vascular Vegetation", "Seagrass")))

# Get data for MPA status 
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"
data_rock <- readRDS(file.path(ltm.dir, "2025/rock_biomass_subset.Rds")) %>% distinct(site, site_type, affiliated_mpa) %>% mutate(habitat = "Shallow reef")
data_kelp <- readRDS(file.path(ltm.dir, "2025/kelp_biomass_subset.Rds")) %>% distinct(site, site_type, affiliated_mpa) %>% mutate(habitat = "Kelp forest")
data_surf <- readRDS(file.path(ltm.dir, "2025/surf_biomass_subset.Rds")) %>% distinct(site, site_type, affiliated_mpa) %>% mutate(habitat = "Surf zone")

# Bind together to get which site is associated with each MPA
site_mpa <- bind_rows(data_rock, data_kelp) %>% bind_rows(., data_surf)
rm(data_rock, data_kelp, data_surf)

# Load the cleaned monitoring site table
sites <-  readRDS(file.path("/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024", "site_locations_corrected.Rds")) %>% 
  st_as_sf(., coords = c("lon_dd", "lat_dd"), remove = F, crs = 4326) %>% 
  st_transform(., crs = 26910) %>% 
  mutate(habitat = if_else(habitat == "Rocky reef", "Shallow reef", habitat)) %>% 
  left_join(., site_mpa) %>% 
  filter(!is.na(affiliated_mpa))

# Load the state waters polygon
state_waters_poly <- readRDS(file.path(gis.dir, "CA_state_waters_polygons.Rds")) %>% 
  st_transform(., crs = 26910) 

# Load the coastline
coast <- sf::st_read(file.path("/home/shares/ca-mpa/data/sync-data/gis_data/raw", "Coastn83", "coastn83.shp")) %>% 
  st_union() %>% 
  st_transform(., crs = 26910)


# Function to plot each site  -----------------------------------------------------

make_site_plot <- function(my_site) {
  print(my_site)
  site_point <- sites %>% filter(site == !!my_site)
  habitat_df <- habitat %>% filter(site == !!my_site)
  habitat_poly <- st_union(habitat_df)
  
  site_bbox <- st_bbox(st_buffer(site_point, dist = 550))
  
  site_buffer_25 <- st_buffer(site_point, dist = 25)
  
  ggplot() +
    geom_sf(data = coast, color = "blue", lwd = 0.6) +
    geom_sf(data = state_waters_poly, fill = "lightblue", alpha = 0.5) +
    geom_sf(data = habitat_df, aes(fill = habitat_class), show.legend = FALSE) +
    geom_sf(data = site_point, color = "black", size = 1) +
    geom_sf(data = site_buffer_25, color = "red", fill = NA) +
    coord_sf(
      xlim = c(site_bbox["xmin"], site_bbox["xmax"]),
      ylim = c(site_bbox["ymin"], site_bbox["ymax"])
    ) +
    labs(fill = NULL, title = my_site) +
    theme_minimal() +
    scale_fill_manual(values = c(
      "Soft Bottom" = "tan",
      "Hard Bottom" = "saddlebrown",
      "Aquatic Vegetation Bed" = "lightgreen",
      "Aquatic Vascular Vegetation" = "darkgreen",
      "Seagrass" = "darkgreen"
    )) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(size = 4),
      axis.text = element_text(size = 4)
    )
}

mpa_groups <- sites %>%
  select(habitat, site, affiliated_mpa, site_type) %>%
  arrange(habitat, affiliated_mpa, site_type, site) %>% 
  group_split(habitat, affiliated_mpa, site_type)


for (group in mpa_groups) {
  mpa <- unique(group$affiliated_mpa) %>% str_to_title() %>% str_replace_all("Smca", "SMCA") %>% str_replace_all("Smr", "SMR")
  habitat_name <- unique(group$habitat)
  site_type <- unique(group$site_type)
  group_sites <- group %>% pull(site)
  
  plots <- lapply(group_sites, make_site_plot)
  ht <- if (length(plots) > 3) ceiling(length(plots)/3) * 3 + 0.5 else 3.5
  
  grid <- wrap_plots(plots, ncol = 3) + 
    plot_annotation(title = paste(mpa, " - ", site_type)) +
    theme(title = element_text(size = 8))
    
  ggsave(file.path(fig.dir, paste0(habitat_name, " - ", mpa, " - ", site_type, ".png")), plot = grid, width = 9, height = ht, dpi = 300)
  
}


