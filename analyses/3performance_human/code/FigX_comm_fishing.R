

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(lubridate)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
gisdir <- file.path(basedir, "gis_data/processed")
plotdir <- "analyses/3performance_human/figures"

# Read data
mpas_orig <- readRDS(file.path(gisdir, "CA_MPA_polygons.Rds"))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Read fishign data
data_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/consulting/halibut_bycatch/data/landings_receipts/processed/CDFW_2000_2020_landings_receipts.Rds")

# Fishing blocks
blocks_sf <- wcfish::blocks

# Build data
################################################################################

# Build data
data <- data_orig %>% 
  group_by(block_id) %>% 
  summarize(revenues_usd=sum(value_usd, na.rm = T)) %>% 
  ungroup()

# Spatialize
data_sf <- blocks_sf %>% 
  filter(block_state=="California" & block_type=="Inshore") %>% 
  left_join(data)


# Build data
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
                   legend.position = c(0.8, 0.7), # when color/size legends
                   # legend.position = c(0.8, 0.8), # when only color legend
                   legend.key.size = unit(0.4, "cm"),
                   legend.key = element_rect(fill=alpha('blue', 0)),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot() +
  # Plot block
  geom_sf(data=data_sf, mapping=aes(fill=revenues_usd/1e6), color="grey30", lwd=0.1) +
  geom_sf(data=mpas_orig, fill="black") +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Legend
  scale_fill_gradientn(name="Revenues (USD millions)\n2000-2022", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Crop
  coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + my_theme
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigX_comm_fishing.png"), 
       width=6.5, height=5.25, units="in", dpi=600)









