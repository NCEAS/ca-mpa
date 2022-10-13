# Compute zonal statistics for all the MPAs

librarian::shelf(sf, terra, tidyverse, cfree14/wcfish,
                 quiet = TRUE)

# path 
base_dir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
gis_dir <- file.path(base_dir, "gis_data/processed")
counterfact_dir <- file.path(base_dir, "counterfactuals")


#### Load the rasters into a cube ----

# list the filenames
filenames  <- list.files(counterfact_dir, pattern = "tif")
print(filenames)

# load the rasters
raster_list <- list.files(counterfact_dir, pattern = "tif", full.names = TRUE)
raster_cube <- rast(raster_list)

# rename the layers with better names
layer_names <- c("bathymetry", "population_density_50km", "distance_shore", "distance_park", "distance_beach", "inat_count", "park_count_600m", "beach_count_600m")

# check that we have 8 raster files in the folder
if (length(filenames) != 8) {
  stop("8 raster files were expected")
}

# needs improvements
message("check the order matches, if not you will have to change the code")
print(filenames)
print(layer_names)

# Rename layers
names(raster_cube) <- layer_names


#### Compute MPA stats ----

# bathy
bathy <- raster_cube$bathymetry

# get the mpa shapefile from Chris' package
mpas <- wcfish::mpas_ca %>% 
  sf::st_as_sf() %>% 
  filter(type!="SMP")  %>%
  st_transform(crs = st_crs(bathy)) %>%
  tibble::rowid_to_column("ID")


bathy_mean <- terra::extract(bathy, vect(mpas), mean, na.rm = TRUE)
names(bathy_mean)[ncol(bathy_mean)] <- "bathy_mean"
mpas_stat <- left_join(mpas, bathy_mean, by = "ID")

bathy_min <- terra::extract(bathy, vect(mpas), min, na.rm = TRUE)
names(bathy_min)[ncol(bathy_min)] <- "bathy_min"
mpas_stat <- left_join(mpas_stat, bathy_min, by = "ID")

bathy_max <- terra::extract(bathy, vect(mpas), max, na.rm = TRUE)
names(bathy_max)[ncol(bathy_max)] <- "bathy_max"
mpas_stat <- left_join(mpas_stat, bathy_max, by = "ID")


# Population

pop <- raster_cube$population_density_50km

# Compute the stats
pop_mean <- terra::extract(pop, vect(mpas), mean, na.rm = TRUE)
names(pop_mean)[ncol(pop_mean)] <- "pop_50km_mean"
mpas_stat <- left_join(mpas_stat, pop_mean, by = "ID")


# Distance to Shore

shore <- raster_cube$distance_shore

# Compute the stats
shore_mean <- terra::extract(shore, vect(mpas), mean, na.rm = TRUE)
names(shore_mean)[ncol(shore_mean)] <- "dist_shore_mean"
mpas_stat <- left_join(mpas_stat, shore_mean, by = "ID")


# Distance from Park entry points

park <- raster_cube$distance_park

# Compute the stats
park_mean <- terra::extract(park, vect(mpas), mean, na.rm = TRUE)
names(park_mean)[ncol(park_mean)] <- "distance_park_mean"
mpas_stat <- left_join(mpas_stat, park_mean, by = "ID")


# Distance from Beach access

beach <- raster_cube$distance_beach

# Compute the stats
beach_mean <- terra::extract(beach, vect(mpas), mean, na.rm = TRUE)
names(beach_mean)[ncol(beach_mean)] <- "distance_beach_mean"
mpas_stat <- left_join(mpas_stat, beach_mean, by = "ID")


# inaturalist count

inat <- raster_cube$inat_count

# Compute the stats
inat_sum <- terra::extract(inat, vect(mpas), sum, na.rm = TRUE)
names(inat_sum)[ncol(inat_sum)] <- "inat_total"
mpas_stat <- left_join(mpas_stat, inat_sum, by = "ID")


#### Transform raster cube into a dataframe ----

counterfact_df <- terra::as.data.frame(raster_cube, xy=TRUE, cells=TRUE)


#### Export ----

# Write MPA stats as geojson
st_write(mpas_stat, file.path(counterfact_dir, "mpas_counterfactuals_stats_epsg3309.geojson"), driver = "GeoJSON")

# Write raster data frame
saveRDS(counterfact_df, file=file.path(counterfact_dir, "counterfactual_layers_epsg3309.Rds"))



