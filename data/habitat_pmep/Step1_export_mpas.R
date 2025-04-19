# Export the habitat data within proximity of the monitoring sites AND their corresponding MPAs 
# Primarily used for plotting
# Cori Lopazanski
# June 2024


# Setup   ----------------------------------------------------------------------
library(tidyverse)
library(sf)

bio.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/biotic"
sub.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed_v2/substrate"
gdb.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Nearshore_Zones_and_Habitat.gdb" 
com.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/combined"
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"


# Read substrate attributes and CRS --------------------------------------------
att <- readRDS(file.path(sub.dir, "West_Coast_USA_Nearshore_CMECS_Substrate_Habitat_Attributes.Rds")) %>% 
  filter(State == "CA")

crs_sub <- st_crs(st_read(file.path(sub.dir, "substrate_ca/sections/substrate_section_23.gpkg"), quiet = TRUE))
crs_bio <- st_crs(st_read(file.path(bio.dir, "biotic_ca/sections/biotic_section_23.gpkg"), quiet = TRUE))

# Build 500m site buffers  ----------------------------------------------------
# Filter to the sites that are included in the analayses
site_cols <- c("year", "site", "site_type", "bioregion", "region4", "affiliated_mpa")
included_kelp <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/data", "kelp_filtered_targeted_msy_data.rds")) %>% distinct(across(all_of(site_cols))) %>% mutate(habitat = "Kelp forest")
included_rock <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/data", "rock_filtered_targeted_rmsy_data.rds")) %>% distinct(across(all_of(site_cols))) %>% mutate(habitat = "Shallow reef")
included_surf <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/data", "surf_filtered_targeted_m_data.rds")) %>% distinct(across(all_of(site_cols))) %>% mutate(habitat = "Surf zone")

included_sites <- bind_rows(included_kelp, included_rock, included_surf)

sites <- readRDS(file.path(ltm.dir, "site_locations_corrected.Rds")) %>% 
  filter(!habitat == "Deep reef") %>% 
  filter(!is.na(habitat)) %>% 
  mutate(habitat = if_else(habitat == "Rocky reef", "Shallow reef", habitat)) %>% 
  filter(site %in% included_sites$site) %>% 
  mutate(habitat = factor(habitat, levels = c("Shallow reef", "Kelp forest", "Surf zone")))

# Project CRS to use meters
sites <- st_transform(sites, crs = 26910)  

# Create buffer polygons around each point, e.g., 1000 meters radius
sites <- st_buffer(sites, dist = 500)


# Get shapefiles for MPAs and filter to MPAs included in the analyses
mpa_poly <- readRDS(file.path("/home/shares/ca-mpa/data/sync-data/gis_data/processed", "CA_MPA_polygons.Rds"))  %>% 
  mutate(affiliated_mpa = str_remove_all(name, regex("\\(No-Take\\)")) %>% 
           str_to_lower() %>% str_trim()) %>% 
  filter(affiliated_mpa %in% included_sites$affiliated_mpa)

mpa_poly <- st_transform(mpa_poly, crs = 26910)

sites <- left_join(sites, included_sites %>% distinct(habitat, site, site_type, affiliated_mpa))

mpa_list <- mpa_poly %>% group_split(affiliated_mpa)

mpa_bbox <- map(mpa_list, function(mpa_row) {
  buffer_km <- 3000  # 3 km in meters
  id <- mpa_row$affiliated_mpa[1]
  mpa_geom <- st_geometry(mpa_row)
  
  site_geom <- sites %>%
    filter(affiliated_mpa == id) %>%
    st_geometry() %>%
    st_union()
  
  combo <- st_union(mpa_geom, site_geom)
  bbox <- st_as_sfc(st_bbox(combo)) %>% st_buffer(buffer_km) %>% 
    st_bbox() %>%
    st_as_sfc()
  
  tibble(affiliated_mpa = id, geometry = bbox)
})

mpa_bbox <- bind_rows(mpa_bbox) %>% st_as_sf()


# Intersect MPA footprints and substrate --------------------------------

# For substrate:
intersect_substrate <- function(section){
  print(paste0("Section: ", section))
  
  sect <- st_read(file.path(sub.dir, "substrate_ca/sections", paste0("substrate_section_", section, ".gpkg")))
  print("Read complete")
  
  section_intersect <- st_intersection(sect, mpa_bbox)
  print("Intersection complete")
  
  saveRDS(section_intersect, file.path(sub.dir, "substrate_mpas", paste0("substrate_mpas_section_", section, ".Rds")))
  print("Save complete")
}

# Apply
# Transform bboxes to match substrate
mpa_bbox <- st_transform(mpa_bbox, crs = crs_sub)
sections <- c("23", "30", "31", "32", "33", "40", "41")
lapply(sections, intersect_substrate)

# For biotic:
intersect_biotic <- function(section){
  print(paste0("Section: ", section))
  sect <- st_read(file.path(bio.dir, "biotic_ca/sections", paste0("biotic_section_", section, ".gpkg")))
  print("Read complete")
  
  section_intersect <- st_intersection(sect, mpa_bbox)
  print("Intersection complete")
  
  saveRDS(section_intersect, file.path(bio.dir, "biotic_mpas", paste0("biotic_mpas_section_", section, ".Rds")))
  print("Save complete")
}


# Apply to biotic:
mpa_bbox <- st_transform(mpa_bbox, crs = crs_bio)
lapply(sections, intersect_biotic)

