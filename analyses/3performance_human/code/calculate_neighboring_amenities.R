


# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
gisdir <- file.path(basedir, "gis_data/processed")
outdir <- file.path(basedir, "mpa_traits/processed")

# Read parks
parks <- readRDS(file.path(gisdir, "USA_parks.Rds"))
mpas_1km <- readRDS(file.path(gisdir, "CA_MPA_polygons_1km_buffer.Rds"))
parks_dissolve <- sf::st_read(file.path(gisdir, "USA_parks_clipped_dissolve.shp"))

# Read campgrounds, picnic grounds, parking lots
campgrounds <- readRDS(file.path(gisdir, "CA_state_park_camp_grounds.Rds"))
picnics <- readRDS(file.path(gisdir, "CA_state_park_picnics.Rds"))
parking <- readRDS(file.path(gisdir, "CA_state_park_parking.Rds"))


# Calculate number of parks within 1 km
################################################################################

# Simplify layers
mpas_1km_simple <- mpas_1km %>% 
  select(name, geometry) %>% 
  sf::as_Spatial()
parks_simple <- parks %>% 
  select(park, geometry) %>% 
  mutate(park=make.unique(park)) %>% 
  sf::as_Spatial()

# Intersect MPAs and parks
# mpa_park_intersect <- sf::st_intersection(mpas_1km_simple, parks_simple) # embarrassing how slow sf is
mpa_park_intersect <- rgeos::gIntersects(mpas_1km_simple, parks_simple, byid=TRUE)

# Wrange the intersection to identify which parks intersect which MPAs
mpa_park_intersect1 <- mpa_park_intersect %>% 
  as.data.frame()
colnames(mpa_park_intersect1) <- mpas_1km_simple$name
rownames(mpa_park_intersect1) <- parks_simple$park
mpa_park_intersect2 <- mpa_park_intersect1 %>% 
  rownames_to_column(var="park") %>% 
  select(park, everything()) %>% 
  gather(key="mpa", value="in_park_yn", 2:ncol(.)) %>% 
  filter(in_park_yn==T) %>% 
  select(mpa, park)

# Summarise stats
stats <- mpa_park_intersect2 %>% 
  group_by(mpa) %>% 
  summarize(nparks=n(),
            parks=paste(park, collapse=", "))


# Compute intersection
mpa_park_sf <- sf::st_intersection(mpas_1km, parks_dissolve)
mpa_park_sf_km2 <- mpa_park_sf %>% 
  select(name) %>% 
  mutate(area_m2 = sf::st_area(.) %>% as.numeric(),
         area_km2 = area_m2 / (1000*1000))


# Calculate number of amenities within 1 km
################################################################################

# Campgrounds
camps_1km <- rgeos::gIntersects(mpas_1km_simple, campgrounds %>% sf::as_Spatial(), byid=TRUE)
camps_1km_stats <- camps_1km %>% 
  as.data.frame() %>% 
  setNames(mpas_1km$name) %>% 
  mutate(campground=campgrounds$campground) %>% 
  select(campground, everything()) %>% 
  gather(key="mpa", value="in_mpa_yn", 2:ncol(.)) %>% 
  filter(in_mpa_yn==T) %>% 
  group_by(mpa) %>% 
  summarize(n_camp_grounds=n(),
            camp_grounds=paste(campground, collapse=", ")) %>% 
  ungroup()

# Picnis
picnics_1km <- rgeos::gIntersects(mpas_1km_simple, picnics %>% sf::as_Spatial(), byid=TRUE)
picnics_1km_stats <- picnics_1km %>% 
  as.data.frame() %>% 
  setNames(mpas_1km$name) %>% 
  mutate(picnic_area=picnics$detail) %>% 
  select(picnic_area, everything()) %>% 
  gather(key="mpa", value="in_mpa_yn", 2:ncol(.)) %>% 
  filter(in_mpa_yn==T) %>% 
  group_by(mpa) %>% 
  summarize(n_picnic_areas=n(),
            picnic_areas=paste(picnic_area, collapse=", ")) %>% 
  ungroup()

# Parking lots
parking_1km <- rgeos::gIntersects(mpas_1km_simple, parking %>% sf::as_Spatial(), byid=TRUE)
parking_1km_stats <- parking_1km %>% 
  as.data.frame() %>% 
  setNames(mpas_1km$name) %>% 
  mutate(parking_lot=parking$name) %>% 
  select(parking_lot, everything()) %>% 
  gather(key="mpa", value="in_mpa_yn", 2:ncol(.)) %>% 
  filter(in_mpa_yn==T) %>% 
  group_by(mpa) %>% 
  summarize(n_parking_lots=n(),
            parking_lots=paste(parking_lot, collapse=", ")) %>% 
  ungroup()



# Final stats
################################################################################

# Build final data
data <- mpas_1km %>% 
  # Simplify
  select(name) %>% 
  sf::st_drop_geometry() %>% 
  # Add park stats
  left_join(stats, by=c("name"="mpa")) %>% 
  left_join(mpa_park_sf_km2 %>% sf::st_drop_geometry(), by="name") %>% 
  select(-area_m2) %>% 
  # Add amenities
  left_join(camps_1km_stats, by=c("name"="mpa")) %>% 
  left_join(picnics_1km_stats, by=c("name"="mpa")) %>% 
  left_join(parking_1km_stats, by=c("name"="mpa")) %>% 
  # Add zeros
  mutate(nparks=ifelse(is.na(nparks), 0, nparks),
         area_km2=ifelse(is.na(area_km2), 0, area_km2),
         n_camp_grounds=ifelse(is.na(n_camp_grounds), 0, n_camp_grounds),
         n_picnic_areas=ifelse(is.na(n_picnic_areas), 0, n_picnic_areas),
         n_parking_lots=ifelse(is.na(n_parking_lots), 0, n_parking_lots))

# Export
saveRDS(data, file=file.path(outdir, "mpa_nearby_parks.Rds"))




