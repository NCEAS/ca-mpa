

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
gisdir <- file.path(basedir, "gis_data/processed")
plotdir <- "analyses/2performance_fisheries/figures"

# Read data
state_waters_poly <- readRDS(file.path(gisdir, "CA_state_waters_polygons.Rds"))
state_waters_line <- readRDS(file.path(gisdir, "CA_state_waters_polyline.Rds"))
mpas_orig <- readRDS(file.path(gisdir, "CA_MPA_polygons.Rds"))
block_mpa_key <- read.csv(file.path(gisdir, "CA_blocks_with_mpas.csv"), as.is=T)

# Get blocks
blocks_orig <- wcfish::blocks

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Add MPA stats to blocks
blocks <- blocks_orig %>% 
  # Add MPA stats
  left_join(block_mpa_key, by="block_id") %>% 
  # Redue
  filter(block_state=="California" & block_type=="Inshore")


# Plot data
################################################################################

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
                   legend.position = c(0.22, 0.15),
                   legend.key.size = unit(0.4, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot() +
  # Plot blocks
  geom_sf(data=blocks, mapping=aes(fill=mpa_km2), color="grey60", lwd=0.1) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot state waters
  geom_sf(data=state_waters_line, color="grey20", lwd=0.2) +
  # Labels
  labs(x="", y="") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  # Legend
  scale_fill_gradientn(name=expression("MPA coverage (km"^2*")"), colors=RColorBrewer::brewer.pal(9, "Blues")[3:9], na.value=NA) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Crop
  coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + my_theme
g

# Export figure
ggsave(g, filename=file.path(plotdir, "Fig1_map_figure.png"), 
       width=3.5, height=5.25, units="in", dpi=600)


