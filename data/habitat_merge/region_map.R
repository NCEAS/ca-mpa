
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Users/cfree/Library/CloudStorage/GoogleDrive-cfree@ucsb.edu/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data/"
datadir <- file.path(basedir, "habitat_merge")

# Read data
data_orig <- raster::raster(file.path(datadir, "CA_bottom_substrate_10m_wgs84.tif"))

# Projections
wgs84 <- sf::st_crs("+proj=longlat +datum=WGS84")

# Read state water
state_waters <- readRDS(file.path(basedir, "gis_data/processed/CA_state_waters_polyline.Rds"))


# Build example raster
################################################################################

# If building example raster
if(F){
  # Crop
  bbox <- raster::extent(c(-121, -117, 32.5, 34.5))
  raster_crop <- raster::crop(x=data_orig,
                              y=bbox)
  raster_xy <- raster_crop %>% 
    raster::as.data.frame(xy=T, na.rm=T)
  raster_xy_df <- raster_xy %>% 
    setNames(c("substrate", "long_dd", "lat_dd")) %>% 
    select(long_dd, lat_dd, substrate)
  saveRDS(raster_xy_df, file=file.path(datadir, "scal_bottom_substrate_xy.Rds"))
}else{
  raster_xy_df <- readRDS(file=file.path(datadir, "scal_bottom_substrate_xy.Rds"))
}

# Convert
raster_xy_df1 <- raster_xy_df %>% 
  mutate(substrate=as.character(substrate))

# Plot data
################################################################################

# Sample data
raster_xy_df_sample <- sample_frac(raster_xy_df, size=0.01) %>% 
  mutate(substrate=as.character(substrate))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_blank(),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = c(0.15, 0.2),
                   legend.key.size = unit(0.5, "cm"),
                   legend.key = element_rect(fill=alpha('blue', 0)),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot() +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot raster
  geom_tile(raster_xy_df1, mapping=aes(x=long_dd, y=lat_dd, fill=substrate)) +
  # Plot state waters
  geom_sf(data=state_waters, linetype="dotted", color="grey30") +
  # Legend
  scale_fill_manual(name="Substrate type",
                    labels = c('Soft', 'Hard'), 
                    values = c("khaki", "tan4")) +
  # Crop
  coord_sf(xlim = c(-121, -117), ylim = c(32.5, 34.5)) +
  # Theme
  theme_bw() + my_theme
#g

# Export
ggsave(g, filename=file.path(datadir, "scal_bottom_substrate.png"), 
       width=6.5, height=4, units="in", dpi=600)


