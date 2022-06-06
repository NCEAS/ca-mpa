


# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
indir <- file.path(basedir, "gis_data/raw")
outdir <- file.path(basedir, "gis_data/processed")
plotdir <- "data/gis_data/figures"

# Read data
list.files(indir)
launches_orig <- sf::st_read(file.path(indir, "BoatLaunchSites", "CUL_CA_BoatLaunchSites_DBW.shp"))
divesites_orig <- sf::st_read(file.path(indir, "DiveSites", "CUL_CA_DiveSites.shp"))
piers_orig <- sf::st_read(file.path(indir, "FishingPiers", "FishingPiers.shp"))
ports_orig <- sf::st_read(file.path(indir, "Ports", "CUL_CA_Ports.shp"))
marinas_orig <- sf::st_read(file.path(indir, "Marinas", "CUL_CA_Marinas_DBW.shp"))

# Projections
wgs84 <- sf::st_crs("+proj=longlat +datum=WGS84")

# Source
# https://filelib.wildlife.ca.gov/Public/R7_MR/CULTURAL/


# Format data
################################################################################

# Format dive sites
divesites <- divesites_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(site=site_name) %>% 
  # Reproject
  sf::st_transform(wgs84) %>% 
  # Add XY
  mutate(long_dd = sf::st_coordinates(.)[,1],
         lat_dd = sf::st_coordinates(.)[,2]) %>% 
  # Arrange
  select(site, source, long_dd, lat_dd)

# Export data
saveRDS(divesites, file.path(outdir, "CA_dive_sites.Rds"))


# Format boat launches
launches <- launches_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(region_code=mlpa_sr) %>% 
  # Reproject
  sf::st_transform(wgs84) %>% 
  # Add XY
  mutate(long_dd = sf::st_coordinates(.)[,1],
         lat_dd = sf::st_coordinates(.)[,2]) %>% 
  # Arrange
  select(name, type, county, region_code, long_dd, lat_dd, everything())

# Export data
saveRDS(launches, file.path(outdir, "CA_boat_launches.Rds"))

# Format piers
piers <- piers_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(name=pnt_name) %>% 
  # Reproject
  sf::st_transform(wgs84) %>% 
  # Add XY
  mutate(long_dd = sf::st_coordinates(.)[,1],
         lat_dd = sf::st_coordinates(.)[,2]) %>% 
  # Arrange
  select(-c(objectid, lat_ddm, lon_ddm)) %>% 
  select(name, long_dd, lat_dd, everything())

# Export data
saveRDS(piers, file.path(outdir, "CA_piers.Rds"))

# Format ports
ports <- ports_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(port=feat_name, port_code=portcode, county=p_county) %>% 
  # Reproject
  sf::st_transform(wgs84) %>% 
  # Add XY
  mutate(long_dd = sf::st_coordinates(.)[,1],
         lat_dd = sf::st_coordinates(.)[,2]) %>% 
  # Arrange
  select(port, port_code, county, long_dd, lat_dd, geometry)

# Export data
saveRDS(ports, file.path(outdir, "CA_ports.Rds"))

# Format marinas
marinas <- marinas_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(marina=name, region_code=mlpa_sr, harbor_code=port_harbo, harbor=port_har_1) %>% 
  # Reproject
  sf::st_transform(wgs84) %>% 
  # Add XY
  mutate(long_dd = sf::st_coordinates(.)[,1],
         lat_dd = sf::st_coordinates(.)[,2]) %>% 
  # Arrange
  select(region_code, county, harbor, harbor_code, marina, long_dd, lat_dd, everything())

# Export data
saveRDS(marinas, file.path(outdir, "CA_marinas.Rds"))


# Merge data
################################################################################

piers1 <- piers %>% 
  mutate(dataset="Piers") %>% 
  select(dataset, name, long_dd, lat_dd, geometry)

marinas1 <- marinas %>%
  rename(name=marina) %>% 
  mutate(dataset="Marinas") %>% 
  select(dataset, name, long_dd, lat_dd, geometry)

divesites1 <- divesites %>%
  rename(name=site) %>% 
  mutate(dataset="Dive sites") %>% 
  select(dataset, name, long_dd, lat_dd, geometry)

ports1 <- ports %>%
  rename(name=port) %>% 
  mutate(dataset="Ports") %>% 
  select(dataset, name, long_dd, lat_dd, geometry)

launches1 <- launches %>%
  mutate(dataset="Boat launches") %>% 
  select(dataset, name, long_dd, lat_dd, geometry)

data <- bind_rows(piers1, marinas1, divesites1, ports1, launches1)


# Plot data
################################################################################

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Plot data
g <- ggplot() +
  facet_wrap(~dataset, ncol=3) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3) +
  # Plot data
  geom_sf(data=data, mapping=aes(color=dataset)) +
  # Labels
  labs(x="", y="") +
  # Crop
  coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() +
  theme(legend.position = "none")
g




