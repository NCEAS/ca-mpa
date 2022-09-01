

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
datadir <- "analyses/2performance_fisheries/analyses/blocks"

# Get blocks
blocks_orig <- wcfish::blocks


# Build data
################################################################################

# MPA regions
# CA/OR, Alder Creek, Pigeon Point, Point Conception, CA/MEX 
region_lats <- c(42, 39.0, 37.18, 34.5, 32.5) %>% rev()

# SF blocks
sf_blocks <- c(301, 488, 489)

# Format blocks
blocks <- blocks_orig %>% 
  # Break by regions
  mutate(mlpa_region=cut(block_lat_dd, 
                         breaks=c(-Inf, region_lats, Inf),
                         labels=c("South of state", "South Coast", "Central Coast",
                                  "North Central Coast", "North Coast", "North of state")) %>% as.character()) %>% 
  # Fix a few regions
  mutate(mlpa_region=ifelse(block_id %in% sf_blocks, "San Francisco Bay", mlpa_region),
         mlpa_region=ifelse(block_id %in% c(897, 950, 1032), "South Coast", mlpa_region)) %>% 
  # Order regions
  mutate(mlpa_region=factor(mlpa_region, 
                            levels=c("South of state", "South Coast", "Central Coast", "San Francisco Bay",
                                     "North Central Coast", "North Coast", "North of state"))) %>% 
  # Arrange
  select(block_state, mlpa_region, everything())

# Export data
saveRDS(blocks, file=file.path(datadir, "blocks_by_mlpa_region.Rds"))


# Plot data
################################################################################

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_blank(),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.4, "cm"),
                   legend.key = element_rect(fill=alpha('blue', 0)),
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Plot data
g <- ggplot() +
  # Plot blocks
  geom_sf(data=blocks, mapping=aes(fill=mlpa_region)) +
  # Plot region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Legend
  scale_fill_discrete(name="MLPA region") +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + my_theme
g

# Export figure
ggsave(g, filename=file.path(datadir, "CA_blocks_by_mlpa_region.png"), 
       width=6.5, height=6.5, units="in", dpi=600)






