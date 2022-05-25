


# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
indir <- file.path(basedir, "gis_data/raw/CA_MPA_boundaries/ds582")
outdir <- file.path(basedir, "gis_data/processed")
plotdir <- "data/gis_data/figures"

# Read data
data_orig <- sf::st_read(file.path(indir, "ds582.shp"))

# Projections
wgs84 <- sf::st_crs("+proj=longlat +datum=WGS84")


# Format data
################################################################################

# Inspect attributes
data_df_orig <- data_orig %>%
  sf::st_drop_geometry()

# Compute centroids
centroids <- data_orig %>%
  sf::st_transform(crs=wgs84) %>%
  sf::st_centroid(data_orig) %>%
  sf::st_coordinates() %>%
  as.data.frame() %>%
  rename(long_dd=X, lat_dd=Y)

# Format data
data <- data_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(area_sqmi=area_sq_mi,
         region=study_regi,
         name_full=fullname,
         url=dfg_url,
         name_short=shortname,
         area_a=acres,
         area_ha=hectares) %>%
  # Add area (sqkm)
  mutate(area_sqkm=measurements::conv_unit(area_sqmi, "mi2", "km2")) %>%
  # Add centroids
  bind_cols(centroids) %>%
  # Arrange
  select(name, name_full, name_short, region, type, ccr, ccr_int,
         area_sqkm, area_sqmi, area_a, area_ha, long_dd, lat_dd, everything()) %>%
  select(-objectid) %>%
  # Reproject
  sf::st_transform(wgs84)

# Inspect
table(data$region)
table(data$type)
table(data$ccr)
table(data$ccr_int)
head(data)

# Reduce to SMRs and SMCAs
mpas_use <- data %>%
  filter(type %in% c("SMR", "SMCA"))

# Export data
saveRDS(data, file.path(outdir, "CA_MPA_polygons.Rds"))
saveRDS(mpas_use, file.path(outdir, "CA_MPA_polygons_smrs_smcas.Rds"))


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
  # Plot MPAs
  geom_sf(data=data, fill="red", color=NA) +
  # Labels
  labs(x="", y="") +
  # Crop
  coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw()
g




