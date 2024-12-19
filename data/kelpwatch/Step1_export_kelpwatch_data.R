## Import Kelp watch data
# Julien Brun, brun@nceas.ucsb.edu
# Updated by Cori Lopazanski, lopazanski@bren.ucsb.edu

# Data Source on EDI
# https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-sbc.74.17
# Updated:
# https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-sbc&identifier=74&revision=26

# Note:
# the data do not have gridded dimensions as normally in a netcdf file
# the dimensions are time and stations
# the station locations are stored in a different grid
# 
# Here is the workflow attempted here:
# 1. Import the grid as a data frame using `tidync` package; this data frame will have time x stations nrow
# 2. `group_by` station taking the max value since it is what we are interested in (max biomass/area); this results in a data frame having one row per station
# 3. transform the station location grid into a data frame
# 4. join the two data frames on station
# 5. create a point sf object
# 6. rasterize to any grid - here is UTM 10N, 30x30 resolution
# 7. export raster to .tif in processed folder


# install.packages("librarian")
librarian::shelf(tidyverse, tidync, sf, rnaturalearth, terra)

# paths
site.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/ltm_sites_1000m_merged"
kelp.dir <- "/home/shares/ca-mpa/data/sync-data/kelpwatch/2024"
ltm.dir  <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"

# read the data in
kelpwatch_file <- "LandsatKelpBiomass_2024_Q3_withmetadata.nc"
kelpwatch_raw <- tidync(file.path(kelp.dir, kelpwatch_file))
kelpwatch_raw # select the biomass grid by default

# Read the LTM sites (these are ones incldued in the habitat analyses)
sites_included <- readRDS(file.path(ltm.dir, "combine_tables/kelp_combine_table.Rds")) %>% distinct(site, site_type) %>% 
  bind_rows(., readRDS(file.path(ltm.dir, "combine_tables/surf_combine_table.Rds")) %>% distinct(site, site_name, site_type)) %>% 
  bind_rows(., readRDS(file.path(ltm.dir, "combine_tables/ccfrp_combine_table.Rds")) %>% distinct(site, site_type))

sites <- readRDS("/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_sites_clean.Rds") %>% 
  filter(site %in% sites_included$site) %>% 
  st_as_sf(., coords = c("long_dd", "lat_dd"), crs = 4326) %>% 
  mutate(site_id = row_number())

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
  relocate(station, .before=everything()) %>% 
  # Make into sf object
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=FALSE) %>% 
  # Reproject to linear units (NAD UTM 10)
  st_transform(crs = 26910)

# Transform sites to match the kelp lat/lon
sites <- st_transform(sites, crs = 26910)

# Buffer the sites to a little over 500m radii (in case cell spans boundary)
sites <- st_buffer(sites, dist = 550)

# Identify the stations that overlap with the sites
site_station_intersect <- st_intersects(kelp_latlon, sites)

# Identify rows where there is at least one intersection
intersect_logical <- lengths(site_station_intersect) > 0

# Filter kelp_latlon for stations that intersect
site_station <- kelp_latlon[intersect_logical, ]

# Transform the biomass grid into a data frame
kelpwatch_df <- kelpwatch_raw %>% 
  hyper_tibble(force = TRUE)

# Transform the time grid into a data frame
kelp_time <- kelpwatch_raw %>% 
  activate("year") %>% 
  hyper_tibble()

# Join the kelp data with the time grid
kelp <- left_join(kelpwatch_df, kelp_time)

# Filter the kelp data for the stations that intersect and years of interest
kelp <- kelp %>% 
  filter(station %in% site_station$station) %>% 
  filter(year >= 2003 & year <= 2023)

# Since we are interested in the max value only, can drop the zeroes 
kelp_present <- kelp %>% 
  filter(!(area == 0)) %>% 
  filter(!is.na(area)) %>% 
  dplyr::select(station, year, area)

# Calculate annual max
kelp_annual <- kelp_present %>%
  arrange(station, year) %>%
  group_by(station, year, .drop = T) %>%
  summarise_all(max, na.rm = T) 

# Create vector object from sites to align with the terra
sites_vect <- vect(sites)

# Create raster template for 30x30m grid
raster_template <- terra::rast(extent = sites_vect,
                               crs = st_crs(kelp_latlon)$wkt,
                               resolution = 30)

# Extends by two cell size in case there are points on the edges
raster_template <- terra::extend(raster_template, 60) 

years <- unique(kelp_annual$year)

for (yr in years) {
  yearly_data <- kelp_annual %>% 
    filter(year == yr) %>% 
    left_join(., kelp_latlon) %>% 
    st_as_sf()
  
  raster <- terra::rasterize(
    yearly_data,
    raster_template,
    field = "area", # The kelp canopy area value
    fun = "sum",    # Aggregates in case of overlaps
    background = NA  # Assign NA to areas without kelp
  )
  
  # Save raster to disk
  raster_path <- file.path(kelp.dir, "processed", paste0("kelp_canopy_", yr, ".tif"))
  terra::writeRaster(raster, raster_path, overwrite = TRUE)
  
}



# Inspect --------------------------------------------------------------------------------

# Load raster
raster_2004 <- terra::rast(file.path(kelp.dir, "processed", "kelp_canopy_2004.tif"))

# Summary of the raster
print(raster_2004)

plot(raster_2004, main = "Kelp Canopy 2004")
plot(st_geometry(sites), add = TRUE, border = "blue")

# Compare input data and rasterized values
input_sum <- sum(kelp_annual %>% filter(year == 2004) %>% pull(area), na.rm = TRUE)
raster_sum <- global(raster_2004, "sum", na.rm = TRUE)

# Compare the two sums
print(input_sum)
print(raster_sum)

