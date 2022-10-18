# Processing data to develop raster layers needed for the counterfactuals analysis
#
# Julien Brun, NCEAS; brun@nceas.usb.edu

# install.packages("librarian")
librarian::shelf(sf, terra, tidyverse, cfree14/wcfish,
                 quiet = TRUE)


data_path <- "/home/shares/ca-mpa/data/sync-data"
inat_path <- "inaturalist/processed"
counterfact_path <- "counterfactuals"


#### bathymetry ####

# We will use this 200m raster as our reference grid to compute the distance from the other layers.
# Note this specific layer as been exported from the arc format into a geotiff using QGIS

bathy <- rast(file.path(data_path, "bathymetry", "processed", "ca_bathymetry_200m_epsg3309.tif"))
bathy

# set the empty raster grid
aoi <- setValues(bathy, as.numeric(NULL))
aoi


### load CA state shapefile for masking land

ca_state_wgs84 <- st_read(file.path(data_path, "gis_data", "raw", "CA_State","ca_boundary_wgs84.shp")) 

ca_state_epsg3309 <- ca_state_wgs84%>%
  st_transform(crs=3309)


#### Rasterizing the MPA to the grid ####

# read data in
# get the mpa shapefile from Chris' package
mpas <- wcfish::mpas_ca %>% 
  sf::st_as_sf() %>% 
  filter(type!="SMP")  %>%
  st_transform(crs = st_crs(bathy)) %>%
  tibble::rowid_to_column("ID")

mpas_raster_id <- terra::rasterize(vect(mpas), bathy, field="ID",
                                   filename= file.path(data_path, counterfact_path, "mpa_200m_id_epsg3309.tif"),
                                   overwrite = TRUE)
mpas_raster <- terra::rasterize(vect(mpas), bathy,
                                filename= file.path(data_path, counterfact_path, "mpa_200m_epsg3309.tif"),
                                overwrite = TRUE)


#### distance from beach access ####

# read data in
beach_parking <- st_read(file.path(data_path, "access_points", "raw", "public_access_points.geojson"))

glimpse(beach_parking)

# Reproject
beach_parking_epsg3309 <- st_transform(beach_parking, crs=st_crs(bathy))

# compute distance raster from public beach access point
distance_public_access_points <- terra::distance(aoi, vect(beach_parking_epsg3309),
                                                 filename = file.path(data_path, counterfact_path, "distance_public_access_points.tif"),
                                                 overwrite = TRUE)

# mask land
mask(distance_public_access_points, vect(ca_state_epsg3309), inverse = TRUE, touches = FALSE, 
     filename = file.path(data_path, counterfact_path, "distance_public_access_points_masked.tif"),
     overwrite = TRUE)


#### Number of beach access within 500m ####

# compute distance raster from public beach access point
rast_beach <- terra::rasterize(vect(beach_parking_epsg3309), aoi, fun = length)

### set up focal matrix - weight of 1 within 500m, 0 outside -- 200m raster resolution => ~3 pixels
focal_rad <- 3
focal_dia <- focal_rad*2+1
focal_mtx <- matrix(nrow = focal_dia, ncol = focal_dia)
for(i in 1:focal_dia) {
  for(j in 1:focal_dia) {
    ### i <- 1; j <- 1
    r_from_center <- sqrt((i - (focal_rad + 1))^2 + (j - (focal_rad + 1))^2)
    focal_mtx[i, j] <- ifelse(r_from_center < focal_rad, 1, 0)
  }
}

### Calculate focal sum of population (or pop density, for equal area
### there is no meaningful difference when rescaling anyway)
number_public_access_points <- focal(rast_beach, w = focal_mtx, fun = sum, na.rm = TRUE, 
                     progress = 'text',
                     filename = file.path(data_path, counterfact_path, "number_public_access_points_500m.tif"),
                     overwrite = TRUE)

# mask land
mask(number_public_access_points, vect(ca_state_epsg3309), inverse = TRUE, touches = FALSE, 
     filename = file.path(data_path, counterfact_path,"number_public_access_points_500m_masked.tif"),
     overwrite = TRUE)



#### distance from park entry points ####

# read data in
park_entry <- st_read(file.path(data_path, "ca_state_park","entry_points","raw","ParkEntryPoints.geojson"))

glimpse(park_entry)            

# Reproject
park_entry_epsg3309 <- st_transform(park_entry, crs=st_crs(bathy))

# compute distance raster from public beach access point
distance_park_entry_points <- terra::distance(aoi, vect(park_entry_epsg3309),
                                              filename = file.path(data_path, counterfact_path,"distance_park_entry_points.tif"),
                                              overwrite = TRUE)

# mask land
mask(distance_park_entry_points, vect(ca_state_epsg3309), inverse = TRUE, touches = FALSE, 
     filename = file.path(data_path, counterfact_path,"distance_park_entry_points_masked.tif"),
     overwrite = TRUE)


#### Number of park access within 500m ####

# compute distance raster from public beach access point
rast_park <- terra::rasterize(vect(park_entry_epsg3309), aoi, fun = length)

### set up focal matrix - weight of 1 within 500m, 0 outside -- 200m raster resolution => 3 pixels
focal_rad <- 3
focal_dia <- focal_rad*2+1
focal_mtx <- matrix(nrow = focal_dia, ncol = focal_dia)
for(i in 1:focal_dia) {
  for(j in 1:focal_dia) {
    ### i <- 1; j <- 1
    r_from_center <- sqrt((i - (focal_rad + 1))^2 + (j - (focal_rad + 1))^2)
    focal_mtx[i, j] <- ifelse(r_from_center < focal_rad, 1, 0)
  }
}

### Calculate focal sum of population (or pop density, for equal area
### there is no meaningful difference when rescaling anyway)
number_park_access_points <- focal(rast_park, w = focal_mtx, fun = sum, na.rm = TRUE, 
                                     progress = 'text',
                                     filename = file.path(data_path, counterfact_path, "number_park_access_points_500m.tif"),
                                     overwrite = TRUE)

# mask land
mask(number_park_access_points, vect(ca_state_epsg3309), inverse = TRUE, touches = FALSE, 
     filename = file.path(data_path, counterfact_path,"number_park_access_points_500m_masked.tif"),
     overwrite = TRUE)

#### distance from coastline #### Better resolution that global watch
# Read data in
# coastline <- st_read(file.path(data_path,"gis_data/raw/Coastn83/coastn83.shp"))
# 
# # Reproject
# coastline_epsg3309 <- st_transform(coastline, crs=st_crs(bathy))
# 
# # Create a 50km buffer
# coastline_buffer_50km_epsg3309 <- st_buffer(coastline_epsg3309, dist=50000)
# 
# aoi_masked <- mask(aoi, vect(coastline_buffer_50km_epsg3309), inverse = TRUE, touches = FALSE)

# Read global fishery watch distance from shore raster
dist_shore_geotiff <- "global_fishing_watch/distance-from-shore.tif"
dist_shore <- rast(file.path(data_path, dist_shore_geotiff))

# Reproject
dist_shore_ca <- project(dist_shore, aoi,
                         filename = file.path(data_path, counterfact_path,"distance_coastline.tif"),
                         overwrite = TRUE)

# Mask
mask(dist_shore_ca, vect(ca_state_epsg3309), inverse = TRUE, touches = FALSE, 
     filename = file.path(data_path, counterfact_path,"distance_coastline_masked.tif"),
     overwrite = TRUE)


#### inaturalist counts ####

# Read iNaturalist data
data_inat <- readRDS(file=file.path(data_path, inat_path, "2000_2021_inaturalist_data.Rds"))


# Convert to sf
data_inat_sf <- data_inat %>%
  sf::st_as_sf(coords=c("long_dd", "lat_dd"), crs=4326) %>%
  st_transform(crs=st_crs(bathy))

data_inat_sf_obs <- data_inat_sf %>%
  mutate(presence = 1) %>%
  select(presence)

inat_count <- rasterize(vect(data_inat_sf_obs), aoi, fun=sum,
                        filename = file.path(data_path, counterfact_path, "inat_count_raster.tif"),
                        overwrite = TRUE)


# Mask
mask(inat_count, vect(ca_state_epsg3309), inverse = TRUE, touches = FALSE, 
     filename = file.path(data_path, counterfact_path, "inat_count_raster_masked.tif"),
     overwrite = TRUE)


#### Population within 50km ####

# Set the dir
popdir <-  file.path(data_path, "census_data/processed")

# Read the tract level data
tract_data <- readRDS(file.path(popdir, "CA_2010_census_tot_pop_by_tract.Rds")) %>%
  filter(!is.na(npeople)) %>%
  st_transform(crs=st_crs(aoi))

# Compute the number of people per pixel (200m*200m)
tract_data <- tract_data %>% 
  mutate(people_pixel = people_sqkm*0.2*0.2)

# rasterize the polygons to our bathy grid
tract_data_ras <- terra::rasterize(vect(tract_data), aoi, field="people_pixel", fun=mean,
                                   filename = file.path(data_path, counterfact_path, "counterfactuals_population_density_200m.tif"),
                                   overwrite = TRUE)


# Adapted from OHI: https://github.com/mapping-marine-spp-vuln/spp_vuln_mapping/blob/master/1_setup/stressors/7_process_other_stressors.Rmd#L231-L314

### set up focal matrix - weight of 1 within 50 km, 0 outside -- 200m raster resolution => 250 pixels
focal_rad <- 250
focal_dia <- focal_rad*2+1
focal_mtx <- matrix(nrow = focal_dia, ncol = focal_dia)
for(i in 1:focal_dia) {
  for(j in 1:focal_dia) {
    ### i <- 1; j <- 1
    r_from_center <- sqrt((i - (focal_rad + 1))^2 + (j - (focal_rad + 1))^2)
    focal_mtx[i, j] <- ifelse(r_from_center < focal_rad, 1, 0)
  }
}

### Calculate focal sum of population (or pop density, for equal area
### there is no meaningful difference when rescaling anyway)
focal_pop_r <- focal(tract_data_ras, w = focal_mtx, fun = sum, na.rm = TRUE, 
                     progress = 'text',
                     filename = file.path(popdir, "counterfactuals_population_density_50km.tif"),
                     overwrite = TRUE)

# Mask
mask(focal_pop_r, vect(ca_state_epsg3309), inverse = TRUE, touches = FALSE, 
     filename = file.path(popdir, "counterfactuals_population_density_50km_masked.tif"),
     overwrite = TRUE)


