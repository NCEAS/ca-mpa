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
# 4. join the two data frames on station
# 5. create a point sf object
# 6. rasterize to any grid


# install.packages("librarian")
librarian::shelf(tidyverse, tidync, sf, rnaturalearth)

# paths
data_dir <- "/home/shares/ca-mpa/data/sync-data/kelp_lter"
gisdir <- file.path(data_dir, "gis_data/processed")

kelpwatch_file <- "LandsatKelpBiomass_2022_Q2_withmetadata.nc"


# read the data in
kelpwatch_raw <- tidync(file.path(data_dir, kelpwatch_file))
kelpwatch_raw # select the biomass grid by default

# kelpwatch_grid <- kelpwatch_raw %>% 
#   hyper_array()

# Transform the biomass grid into a data frame
kelpwatch_df <- kelpwatch_raw %>% 
  hyper_tibble(force = TRUE)

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
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=FALSE)

# write as geojson
dir.create(file.path(data_dir, "processed"), showWarnings = FALSE)
st_write(kelp_biomass_sf,
         dsn = file.path(data_dir, "processed/Landsatkelp_max_2022_Q2.geojson"), 
         layer = "Landsatkelp_max_2022_Q2_withmetadata.geojson",
         delete_dsn = T)



#### PLOT ####

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# plot the max Biomass
biog <- ggplot() +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3) +
  # Plot kelp
  geom_sf(data=kelp_biomass_sf, aes(color = biomass), size = 2) +
  # Labels
  labs(x="", y="") +
  # Crop
  coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw()
biog

# Note from metadata: Biomass data (wet weight, kg) are given for individual 30 x 30 meter pixels 
# in the coastal areas extending from near Ano Nuevo, CA through the southern range limit in Baja 
# California (including offshore islands), representing the range where giant kelp is the dominant 
# canopy forming species.


# plot the max area
areag <- ggplot() +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3) +
  # Plot kelp
  geom_sf(data=kelp_biomass_sf, aes(color = area), size = 2) +
  # Labels
  labs(x="", y="") +
  # Crop
  coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw()
areag
