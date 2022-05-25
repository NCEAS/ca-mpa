


# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
indir <- file.path(basedir, "gis_data/raw/StateWaterJurisdiction")
outdir <- file.path(basedir, "gis_data/processed")
plotdir <- "data/gis_data/figures"

# Read data
line_orig <- sf::st_read(file.path(indir, "MAN_CA_StateWaterLine.shp"))
poly_orig <- sf::st_read(file.path(indir, "MAN_CA_StateWater.shp"))

# Projections
wgs84 <- sf::st_crs("+proj=longlat +datum=WGS84")


# Format data
################################################################################

# Inspect attributes
poly_df_orig <- poly_orig %>%
  sf::st_drop_geometry()

# Format data
data <- poly_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  # Add area
  mutate(area_sqkm=sf::st_area(.) %>% as.numeric() / (1000*1000) ) %>%
  # Simplify
  select(id, area_sqkm) %>%
  # Reproject
  sf::st_transform(wgs84)

# Inspect
sum(data$area_sqkm)

# Export data
saveRDS(data, file.path(outdir, "CA_state_waters_polygons.Rds"))


# Plot data
################################################################################

# Get blocks
blocks <- wcfish::blocks

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Plot data
g <- ggplot() +
  # Plot blocks
  geom_sf(data=blocks, fill=NA, color="grey40", lwd=0.1) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3) +
  # Plot state waters
  geom_sf(data=data, fill="red", color=NA) +
  # Labels
  labs(x="", y="") +
  # Crop
  coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw()




