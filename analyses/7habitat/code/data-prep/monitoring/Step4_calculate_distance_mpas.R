# Calculate site distance to nearest MPA border
# Cori Lopazanski
# March 2025

library(sf)
library(tmap)

# Load site locations 
gis.dir <- "/home/shares/ca-mpa/data/sync-data/gis_data/processed"

sites <- readRDS(file.path("/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024", "site_locations_corrected.Rds")) %>% 
  dplyr::select(site, site_type, geometry)

# Get data for MPA status 
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"
data_rock <- readRDS(file.path(ltm.dir, "combine_tables/ccfrp_full.Rds")) %>% distinct(site, site_type, affiliated_mpa)
data_kelp <- readRDS(file.path(ltm.dir, "combine_tables/kelp_full.Rds")) %>% distinct(site, site_type, affiliated_mpa)
data_surf <- readRDS(file.path(ltm.dir, "combine_tables/surf_full.Rds")) %>% distinct(site, site_type, affiliated_mpa)

# Bind together to get which site is associated with each MPA
data <- bind_rows(data_rock, data_kelp) %>% bind_rows(., data_surf)
rm(data_rock, data_kelp, data_surf)

# Get MPA polygons and general metadata
mpas <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_general.Rds") %>% 
  mutate(implementation_year = as.numeric(format(implementation_date, '%Y'))) %>% 
  filter(affiliated_mpa %in% data$affiliated_mpa)

mpa_poly <- readRDS(file.path("/home/shares/ca-mpa/data/sync-data/gis_data/processed", "CA_MPA_polygons.Rds")) %>%
  st_transform(., crs = 26910) %>% 
  mutate(name = str_remove_all(name, stringr::fixed(" (No-Take)"))) %>% 
  filter(name %in% mpas$name)

# Load state waters polygon
state_waters_poly <- readRDS(file.path(gis.dir, "CA_state_waters_polygons.Rds")) %>% 
  st_transform(., crs = 26910) %>% 
  st_buffer(., dist = -50) # shrink to ensure sites are a litle ofshore/line up with habitat edges

coast <- sf::st_read(file.path("/home/shares/ca-mpa/data/sync-data/gis_data/raw", "Coastn83", "coastn83.shp")) %>% 
  st_union() %>% 
  st_transform(., crs = 26910)

# First expand the MPA polygons to extract the right portions of the coastline
mpas_expanded <- st_buffer(mpa_poly, dist = 500)

# Extract the portion of the coastline that are within these expanded boundaries
coast_extract <- st_intersection(mpas_expanded, coast)

# Buffer the coastline 
coast_extract <- st_simplify(coast_extract)
coast_buffer <- st_buffer(coast_extract, dist = 100)
coast_buffer <- st_simplify(coast_buffer)
coast_buffer_poly <- st_cast(coast_buffer, "MULTIPOLYGON")

# Extract MPA boundaries
mpa_boundaries <- st_boundary(mpa_poly)

# Remove MPA boundary segments that are adjacent to land
mpa_boundaries_clean <- st_difference(mpa_boundaries, coast_buffer_poly)

# Create the map
tm_shape(state_waters_poly) +
  tm_borders(col = "lightblue", fill = "lightblue")+
  tm_shape(mpa_boundaries_clean) +
  tm_lines(col = "type") +
  tm_shape(coast_buffer_poly) +
  tm_borders(col = "black")





# Compute distances to cleaned MPA boundaries
sites <- sites %>%
  mutate(
    dist_to_mpa = st_distance(geometry, mpa_boundaries_clean) %>% as.numeric(),
    inside_mpa = st_within(geometry, mpas, sparse = FALSE) %>% rowSums() > 0
  )

# If inside an MPA, make distance negative (to indicate inside distance)
sites <- sites %>%
  mutate(dist_to_mpa = ifelse(inside_mpa, -dist_to_mpa, dist_to_mpa))

# View results
sites %>% dplyr::select(site, dist_to_mpa)










