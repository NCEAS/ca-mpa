


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

# Read MPA data
mpas <- readRDS(file.path(outdir, "CA_MPA_polygons.Rds"))
mpa_pts <- mpas %>%
  sf::st_drop_geometry()

# Blocks
blocks <- wcfish::blocks


# Build data
################################################################################

# Simplify MPAs/blocks
mpas_simple <- mpas %>%
  select(name)
blocks_simple <- blocks %>%
  select(block_id)

# Dissolve MPAs
mpas_simple_dis <- mpas_simple %>%
  mutate(name="1") %>%
  group_by(name) %>%
  summarise(n=n())

# Intersect MPAs/blocks
data1 <- sf::st_intersection(x=blocks_simple, y=mpas_simple)

# Calculate area
intersection_areas <- data1 %>%
  sf::st_area() %>% as.numeric() %>% measurements::conv_unit(., "m2", "km2")

# Add areas to intersection sf
data2 <- data1 %>%
  mutate(area_km2=intersection_areas)

# Compute block stats
block_stats <- data2 %>%
  sf::st_drop_geometry() %>%
  group_by(block_id) %>%
  summarize(mpa_n=n_distinct(name),
            mpas=paste(name, collapse=", "),
            mpa_km2=sum(area_km2)) %>%
  ungroup()

# Add block stats to block sf
blocks_stats_sf <- blocks %>%
  filter(block_state=="California" & block_type=="Inshore") %>%
  left_join(block_stats, by="block_id")


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_blank(),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = c(0.25, 0.15),
                   legend.key.size = unit(0.2, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Plot data
g1 <- ggplot() +
  # Plot blocks
  geom_sf(data=blocks_stats_sf, color="grey40", fill=NA, lwd=0.1) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3) +
  # Plot MPA points
  # geom_sf(data=mpas, fill="red", color=NA) +
  geom_point(data=mpa_pts, mapping=aes(x=long_dd, y=lat_dd),
             size=1, pch=21, fill="white", inherit.aes=F) +
  # Legend
  # scale_size_continuous(name="Area (km2)") +
  # Crop
  coord_sf(xlim = c(-125, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + my_theme
g1

# Plot data
g2 <- ggplot() +
  # Plot blocks
  geom_sf(data=blocks_stats_sf, mapping=aes(fill=mpa_n), color="grey40", lwd=0.1) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3) +
  # Legend
  scale_fill_gradientn(name="Number\nof MPAs",
                        colors=RColorBrewer::brewer.pal(9, "YlOrRd")[2:9], na.value=NA) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +

  # Crop
  coord_sf(xlim = c(-125, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + my_theme
g2

# Plot data
g3 <- ggplot() +
  # Plot blocks
  geom_sf(data=blocks_stats_sf, mapping=aes(fill=mpa_km2), color="grey40", lwd=0.1) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3) +
  # Legend
  scale_fill_gradientn(name="MPA area (km2)",
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd")[2:9], na.value=NA) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +

  # Crop
  coord_sf(xlim = c(-125, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + my_theme
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1)
g

# Export figure
ggsave(g, filename=file.path(plotdir, "figure_mpa_block_covereage.png"),
       width=6.5, height=3.25, units="in", dpi=600)




