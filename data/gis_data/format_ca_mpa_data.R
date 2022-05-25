


# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
indir <- "data/gis_data/raw"
outdir <- "data/gis_data/processed"
plotdir <- "data/gis_data/figures"

# Read data
data_orig <- sf::st_read(file.path(indir, "ca_mpas_cdfw", "ds582.shp"))

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

# Read landings
landings <- read.csv(file=file.path(outdir, "2016_2020_average_catch_by_port.csv"), as.is=T)

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Sonoma-Mendocino county line
son_mend_county <- 38+46.125/60

# Setup theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_blank(),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = c(0.8, 0.8),
                   legend.key = element_rect(fill=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot() +
  # Plot blocks
  geom_sf(data=blocks, fill=NA, color="grey40", lwd=0.1) +
  # Sononoma-Mendocino county line
  geom_hline(yintercept=son_mend_county) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3) +
  # Plot ports
  geom_point(data=landings, aes(x=long_dd, y=lat_dd, size=value_usd/1e6), alpha=0.3) +
  # Plot MPAs
  geom_sf(data=data, fill="red", color=NA) +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_size_continuous(name="Average annual revenues\nfrom 2016-2020\n(USD millions)") +
  # Crop
  coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + my_theme

# Export
ggsave(g, filename=file.path(plotdir, "CA_mpa_coverage_w_fisheries_areas.png"),
       width=4.5, height=6.5, units="in", dpi=600)




