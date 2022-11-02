## Import Kelp watch data
# Julien Brun, brun@nceas.ucsb.edu

# Data Source on EDI
# https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-sbc.74.17

# Note:
# the data do not have gridded dimensions as normally in a netcdf file
# the dimensions are time and stations
# The station locations are stored in a different grid
# 
# Here is the workflow attempted here:
# 1. Import the grid as a data frame using `tidync` package; this data frame will have time x stations nrow
# 2. `group_by` station taking the max value since it is what we are interested in (max biomass); this results in a data frame having one row per station
# 3. transform the station location grid into a data frame
# 4. join the two data frame on station
# 5. create a point sf object
# 6. rasterize to any grid


# install.packages("librarian")
librarian::shelf(tidyverse, tidync, sf, stars)

data_dir <- "/home/shares/ca-mpa/data/sync-data/kelp_lter/"

kelpwatch_file <- "LandsatKelpBiomass_2022_Q2_withmetadata.nc"

kelpwatch_raw <- tidync(file.path(data_dir, kelpwatch_file))
kelpwatch_raw # select the biomass grid by default

# kelpwatch_grid <- kelpwatch_raw %>% 
#   hyper_array()

# Transform the biomass grid into a data frame
kelpwatch_df <- kelpwatch_raw %>% 
  hyper_tibble()

kelpwatch_max <- kelpwatch_df %>%
  group_by(station, .drop = FALSE) %>%
  summarise_all(max, na.rm = TRUE)

# Transform the latitude grid into a data frame
kelp_lat <- kelpwatch_raw %>%
  activate("latitude") %>%
  hyper_tibble()

# Transform the longitude grid into a data frame
kelp_lon <- kelpwatch_raw %>%
  activate("longitude") %>%
  hyper_tibble()

# Join the two geo info
kelp_latlon <- left_join(kelp_lat, kelp_lon, by = "station") %>%
  relocate(station, .before=everything())

# Join with the biomass data

kelp_biomass_latlon <- left_join(kelp_latlon, kelpwatch_max, by = "station") %>%
  relocate(station, .before=everything())

# Makes it a sf object
kelp_biomass_sf <- kelp_biomass_latlon %>%
  st_as_sf(coords=c("latitude","longitude"), crs=4326, remove=FALSE)



 
# # Read the data with stars
# kelp_stars <- stars::read_stars(file.path(data_dir, kelpwatch_file)) # warnings because does not recognize dim
# bio <- kelp_stars$biomass # Extract the array for the biomass data
