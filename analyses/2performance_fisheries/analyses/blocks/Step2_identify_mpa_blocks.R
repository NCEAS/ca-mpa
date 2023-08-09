

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
# Chris
#basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
# Cori
basedir <- "/Users/lopazanski/Library/CloudStorage/GoogleDrive-lopazanski@ucsb.edu/Shared drives/NCEAS MPA network assessment/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
gisdir <- file.path(basedir, "gis_data/processed")
datadir <- "analyses/2performance_fisheries/analyses/blocks"

# Read data
mpas <- readRDS(file.path(gisdir, "CA_MPA_polygons.Rds"))
blocks_orig <- readRDS(file=file.path(datadir, "blocks_by_mlpa_region.Rds"))

# Note that block 1032 is not properly clipped. -- fix 


# Build data
################################################################################

# Simplify MPAs
mpas_simple <- mpas %>%
  # Reduce to MPAs
  filter(type!="Special Closure") %>% 
  # Simplify
  select(name)

# Simplify blocks
blocks_simple <- blocks_orig %>%
  select(block_id)

# Calculate block area
blocks_area <- blocks_simple %>% 
  sf::st_area() %>% as.numeric() %>% 
  measurements::conv_unit(., "m2", "km2")

# Add block area to main block dataframe
blocks_orig <- blocks_orig %>% 
  mutate(block_area_km2 = blocks_area)

# Add block area to simple block dataframe
blocks_simple <- blocks_simple %>% 
  mutate(block_area_km2 = blocks_area)

# Intersect MPAs/blocks
data1 <- sf::st_intersection(x=blocks_simple, y=mpas_simple)

# Calculate MPA area
intersection_areas <- data1 %>%
  sf::st_area() %>% as.numeric() %>% measurements::conv_unit(., "m2", "km2")

# Add areas to intersection sf
data2 <- data1 %>%
  mutate(area_km2=intersection_areas)

# Summarize MPA coverage for each block
block_stats <- data2 %>%
  sf::st_drop_geometry() %>%
  group_by(block_id) %>%
  dplyr::summarize(mpa_n=n_distinct(name),
                   mpas=paste(name, collapse=", "),
                   mpa_km2=sum(area_km2)) %>%
  ungroup() %>% 
  # TEMPORARY: Fix block 1032 until you fix in wcfish
  mutate(mpa_n=ifelse(block_id==1032, NA, mpa_n),
         mpas=ifelse(block_id==1032, NA, mpas),
         mpa_km2=ifelse(block_id==1032, NA, mpa_km2))


# Build final blocks data
blocks <- blocks_orig %>% 
  # Add MPA stats
  left_join(block_stats, by="block_id") %>% 
  # Mark if MPA block
  mutate(mpa_yn=ifelse(is.na(mpa_n), "Non-MPA", "MPA")) %>% 
  # Arrange
  select(-block_area_km2) %>% 
  select(block_state, block_type, block_id, block_lat_dd, block_long_dd ,
         block_sqkm,  mlpa_region, mpa_yn, everything())

# Export data
saveRDS(blocks, file=file.path(datadir, "blocks_by_mlpa_region_w_mpa_stats.Rds"))


# Plot data
################################################################################


# MPA regions
# CA/OR, Alder Creek, Pigeon Point, Point Conception, CA/MEX 
region_lats <- c(42, 39.0, 37.18, 34.5, 32.5) %>% rev()

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
  geom_sf(data=blocks, mapping=aes(fill=mlpa_region, alpha=mpa_yn), lwd=0.2) +
  # Plot region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Legend
  scale_fill_discrete(name="MLPA region") +
  scale_alpha_manual(name="Block type", values=c(1, 0.2)) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + my_theme
g

# Export figure
ggsave(g, filename=file.path(datadir, "CA_blocks_by_mlpa_region_mpa_yn.png"), 
       width=6.5, height=6.5, units="in", dpi=600)






