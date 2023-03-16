# This script investigates way to import hdf5 wave energy data from the DOE 
# data source: https://registry.opendata.aws/wpto-pds-us-wave/

# Julien Brun brun@nceas.ucsb.edu


# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("rhdf5")

# Import the necessary libraries
librarian::shelf(tidyverse, janitor, rhdf5, sf)

# Path to the data on Aurora
data_dir <- "/home/shares/ca-mpa/data/wave_energy"

# Period of interest
years <- 2000:2010

#### Check the file content / structure ####
wave_filename_test <- file.path(data_dir, "West_Coast_wave_2000.h5")

# list the content of the first files
h5ls(file=wave_filename_test, s3 = FALSE)

#    group                        name       otype   dclass           dim
# 0      /                 coordinates H5I_DATASET    FLOAT    2 x 699904
# 1      /  directionality_coefficient H5I_DATASET    FLOAT 699904 x 2928
# 2      /               energy_period H5I_DATASET    FLOAT 699904 x 2928
# 3      /    maximum_energy_direction H5I_DATASET    FLOAT 699904 x 2928
# 4      /        mean_absolute_period H5I_DATASET    FLOAT 699904 x 2928
# 5      /         mean_wave_direction H5I_DATASET    FLOAT 699904 x 2928
# 6      /   mean_zero-crossing_period H5I_DATASET    FLOAT 699904 x 2928
# 7      /                        meta H5I_DATASET COMPOUND        699904
# 8      / omni-directional_wave_power H5I_DATASET    FLOAT 699904 x 2928
# 9      /                 peak_period H5I_DATASET    FLOAT 699904 x 2928
# 10     /     significant_wave_height H5I_DATASET    FLOAT 699904 x 2928
# 11     /              spectral_width H5I_DATASET    FLOAT 699904 x 2928
# 12     /                  time_index H5I_DATASET   STRING          2928
# 13     /                 water_depth H5I_DATASET    FLOAT        699904



#### Main loop ####

for (y in years){
  wave_filename <- paste0("West_Coast_wave_", y, ".h5")
  message(sprintf("Processing file %s", wave_filename))
  
  # get the coordinate and create dataframe
  if(y == years[1]) {
    # Get the coordinates
    coord <-  h5read(file.path(data_dir, wave_filename), name = "coordinates")
    
    # Make it a dataframe
    coord_df <- t(coord) %>% as_tibble()
    names(coord_df) <- c("lat", "lon")
  }
  
  # get the wave power values for the year
  wave_power <- h5read(file.path(data_dir, wave_filename), name = "omni-directional_wave_power")
  
  # Compute the year average for each point
  wave_power_mean <- rowMeans(wave_power)
  
  # Add the average values as column (there must be a better way!)
  coord_df[ , ncol(coord_df) + 1] <- wave_power_mean    # Append new column
  colnames(coord_df)[ncol(coord_df)] <- paste0("y", y)  # Rename column name

}


#### subset for our area of interest (ideally would be done earlier) ####

# xmin       ymin       xmax       ymax
# -125.66789   32.33338 -117.09704   41.99988
coord_ca <- coord_df %>%
  filter(lat > 32 & lat < 42) %>%
  filter(lon > -126 & lon < -117)
  
  
#### Compute the all year average ####
wave_power_df <- coord_ca %>%
  mutate(mean_200012 = rowMeans(select(., starts_with("y2")), na.rm = TRUE))

# make it an sf object 
wave_power_sf <- st_as_sf(wave_power_df, coords = c("lon","lat"), remove = FALSE, crs = 4326)


#### write output files ####
st_write(wave_power_sf,
         dsn = file.path(data_dir, "processed", "wave_power_2000_2010.geojson"), 
         layer = "wave_power_2000_2010.geojson",
         delete_dsn = T)

saveRDS(wave_power_sf, file = file.path(data_dir, "processed", "wave_power_2000_2010.rds"))


