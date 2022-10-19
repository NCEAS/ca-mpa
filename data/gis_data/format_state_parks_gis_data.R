


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
camps_orig <- sf::st_read(file.path(indir, "state_parks/Campgrounds", "Campgrounds.shp"))
parking_orig <- sf::st_read(file.path(indir, "state_parks/ParkingPoints", "ParkingPoints.shp"))
picnic_orig <- sf::st_read(file.path(indir, "state_parks/PicnicGrounds", "PicnicGrounds.shp"))
camps_orig <- sf::st_read(file.path(indir, "state_parks/USA_Parks", "park_dtl.gdb"))

# Read parks
rgdal::ogrListLayers(file.path(indir, "state_parks/USA_Parks/v10", "park_dtl.gdb"))
parks_orig <- rgdal::readOGR(file.path(indir, "state_parks/USA_Parks/v10", "park_dtl.gdb"), "park_dtl")

# Projections
wgs84 <- sf::st_crs("+proj=longlat +datum=WGS84")

# Source
# State: https://www.parks.ca.gov/?page_id=29682
# Parks: https://www.arcgis.com/home/item.html?id=578968f975774d3fab79fe56c8c90941


# Format data
################################################################################

# Format camps
camps <- camps_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  # Reproject
  sf::st_transform(wgs84) %>% 
  # Add XY
  mutate(long_dd = sf::st_coordinates(.)[,1],
         lat_dd = sf::st_coordinates(.)[,2])

# Export data
saveRDS(camps, file.path(outdir, "CA_state_park_camp_grounds.Rds"))
write.csv(camps %>% sf::st_drop_geometry(), file.path(outdir, "CA_state_park_camp_grounds.csv"), row.names = F)

# Format parking
parking <- parking_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  # Reproject
  sf::st_transform(wgs84) %>% 
  # Add XY
  mutate(long_dd = sf::st_coordinates(.)[,1],
         lat_dd = sf::st_coordinates(.)[,2])

# Export data
saveRDS(parking, file.path(outdir, "CA_state_park_parking.Rds"))

# Format picnics
picnics <- picnic_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  # Reproject
  sf::st_transform(wgs84) %>% 
  # Add XY
  mutate(long_dd = sf::st_coordinates(.)[,1],
         lat_dd = sf::st_coordinates(.)[,2])

# Export data
saveRDS(picnics, file.path(outdir, "CA_state_park_picnics.Rds"))

# Format parks
parks <- parks_orig %>% 
  # Convert to sf
  sf::st_as_sf() %>% 
  # Reproject
  sf::st_transform(wgs84) %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(park=name, park_type=feattype) %>% 
  select(park, park_type, geometry)

# Export data
saveRDS(parks, file.path(outdir, "USA_parks.Rds"))
sf::st_write(parks, file.path("~/Desktop", "USA_parks.shp")) # because you can't write shps to google drive apparently



# Plot data
################################################################################

# Theme
my_theme <- theme(axis.text=element_text(size=6),
                  axis.title=element_blank(),
                  axis.text.y = element_text(angle = 90, hjust = 0.5),
                  legend.text=element_text(size=6),
                  legend.title=element_text(size=8),
                  strip.text=element_text(size=8),
                  plot.title=element_text(size=10),
                  # Gridlines
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"),
                  # Legend
                  legend.background = element_rect(fill=alpha('blue', 0)))

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
  scale_x_continuous(breaks=seq(-124,-116,  2)) +
  # Crop
  coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g

# Export
ggsave(g, filename=file.path(plotdir, "CA_coastal_cultural_sites.png"), 
       width=6.5, height=7.25, units="in", dpi=600)



