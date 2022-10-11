# Compute zonal statistics for all the MPAs

librarian::shelf(sf, terra, tidyverse, cfree14/wcfish,
                 quiet = TRUE)

# path 
base_dir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
gis_dir <- file.path(base_dir, "gis_data/processed")
counterfact_dir <- file.path(base_dir, "counterfactuals")



# load the rasters

filenames  <- list.files(counterfact_dir)
# raster_list <- list.files(counterfact_dir, full.names = TRUE)
# filenames

# bathy
bathy <- rast(file.path(counterfact_dir, "ca_bathymetry_200m_epsg3309.tif"))

# get the mpa shapefile from Chris' package
mpas_stat <- wcfish::mpas_ca %>% 
  sf::st_as_sf() %>% 
  filter(type!="SMP")  %>%
  st_transform(crs = st_crs(bathy)) %>%
  tibble::rowid_to_column("ID")


bathy_mean <- terra::extract(bathy, vect(mpas), mean, na.rm = TRUE)
names(bathy_mean)[ncol(bathy_mean)] <- "bathy_mean"
mpas_stat <- left_join(mpas_stat, bathy_mean, by = "ID")

bathy_min <- terra::extract(bathy, vect(mpas), min, na.rm = TRUE)
names(bathy_min)[ncol(bathy_min)] <- "bathy_mein"
mpas_stat <- left_join(mpas_stat, bathy_min, by = "ID")

bathy_max <- terra::extract(bathy, vect(mpas), max, na.rm = TRUE)
names(bathy_max)[ncol(bathy_max)] <- "bathy_max"
mpas_stat <- left_join(mpas_stat, bathy_max, by = "ID")


# population

pop <- rast(file.path(counterfact_dir, "counterfactuals_population_density_50km.tif"))
# Compute the stats
pop_mean <- terra::extract(pop, vect(mpas), mean, na.rm = TRUE)
names(pop_mean)[ncol(pop_mean)] <- "pop_50km_mean"
mpas_stat <- left_join(mpas_stat, pop_mean, by = "ID")


# Distance to Shore

shore <- rast(file.path(counterfact_dir, "distance_coastline.tif"))
# Compute the stats
shore_mean <- terra::extract(shore, vect(mpas), mean, na.rm = TRUE)
names(shore_mean)[ncol(shore_mean)] <- "dist_shore_mean"
mpas_stat <- left_join(mpas_stat, shore_mean, by = "ID")


# Distance from Park entry points

park <- rast(file.path(counterfact_dir, "distance_park_entry_points.tif"))
# Compute the stats
park_mean <- terra::extract(park, vect(mpas), mean, na.rm = TRUE)
names(park_mean)[ncol(park_mean)] <- "dist_park_mean"
mpas_stat <- left_join(mpas_stat, park_mean, by = "ID")


# Distance from Beach access

beach <- rast(file.path(counterfact_dir, "distance_public_access_points.tif"))
# Compute the stats
beach_mean <- terra::extract(beach, vect(mpas), mean, na.rm = TRUE)
names(beach_mean)[ncol(beach_mean)] <- "dist_beach_mean"
mpas_stat <- left_join(mpas_stat, beach_mean, by = "ID")


# inaturalist count

inat <- rast(file.path(counterfact_dir, "inat_count_raster.tif"))
# Compute the stats
inat_sum <- terra::extract(inat, vect(mpas), sum, na.rm = TRUE)
names(inat_sum)[ncol(inat_sum)] <- "inat_total"
mpas_stat <- left_join(mpas_stat, inat_sum, by = "ID")


## Write file
st_write(mpas_stat, file.path(counterfact_dir, "mpas_counterfactuals_stats_epsg3309.geojson"), driver = "GeoJSON")
