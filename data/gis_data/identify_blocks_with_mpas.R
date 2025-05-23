

# Setup -----------------------------------------------------------------------------

# Clear workspace
rm(list = ls())

## Packages ----
library(tidyverse)
library(countrycode)

## Directories ----
basedir <- "/Volumes/GoogleDrive-105151121202188525604/Shared drives/NCEAS MPA network assessment/MPA Network Assessment: Working Group Shared Folder/data/sync-data" # Cori Local
#basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
outdir <- file.path(basedir, "gis_data/processed")
plotdir <- "data/gis_data/figures"

## Read MPA data ----
mpas <- readRDS(file.path(outdir, "CA_MPA_polygons.Rds"))
mpa_pts <- mpas %>%
  sf::st_drop_geometry()

## Get Blocks ----
blocks <- wcfish::blocks


# P1: Build Data - All MPA Types ------------------------------------------------------

# Simplify MPAs
mpas_simple <- mpas %>%
  select(name)

# Simplify blocks
blocks_simple <- blocks %>%
  select(block_id)

# Calculate block area
blocks_area <- blocks_simple %>% 
  sf::st_area() %>% as.numeric() %>% 
  measurements::conv_unit(., "m2", "km2")

# Add block area to main block dataframe
blocks <- blocks %>% 
  mutate(block_area_km2 = blocks_area)

# Add block area to simple block dataframe
blocks_simple <- blocks_simple %>% 
  mutate(block_area_km2 = blocks_area)

# Test for any overlap among MPA polygons 
overlap <- sf::st_overlaps(mpas_simple, mpas_simple, sparse = FALSE)
isTRUE(overlap) # if FALSE, there is no overlap

# Intersect MPAs/blocks
data1 <- sf::st_intersection(x=blocks_simple, y=mpas_simple)

# Calculate MPA area
intersection_areas <- data1 %>%
  sf::st_area() %>% as.numeric() %>% measurements::conv_unit(., "m2", "km2")

# Add areas to intersection sf
data2 <- data1 %>%
  mutate(area_km2=intersection_areas)

## Summarize MPA coverage for each block ----
block_stats <- data2 %>%
  sf::st_drop_geometry() %>%
  group_by(block_id) %>%
  dplyr::summarize(mpa_n=n_distinct(name),
            mpas=paste(name, collapse=", "),
            mpa_km2=sum(area_km2)) %>%
  ungroup()

## List blocks for each MPA ----
mpa_stats <- data2 %>% 
  select(block_id, name) %>% 
  sf::st_drop_geometry() %>% 
  distinct() 

rownames(mpa_stats) <- NULL

## Examine MPAs that do not overlap with any block
no_block <- mpas %>% 
  filter(!(name %in% data1$name))
  
## Export MPA key: blocks with each MPA (all MPAs) ----
#saveRDS(mpa_stats, file.path(outdir, "mpa_block_overlap_pairs.Rds"))

## Export block key: only blocks with MPAs (all MPAs) ----
#write.csv(block_stats, file=file.path(outdir, "CA_blocks_with_mpas_all_mpa_types.csv"), row.names=F)


# P2: Build Data - Only SMRs/SMCAs and FMRs/FMCAs ---------------------------

# Only look at SMRs/SMCAs and FMRs, FMCAs
types_use <- c("SMR", "SMCA", "SMCA (No-Take)", "FMR", "FMCA")
mpas_use <- mpas %>% 
  filter(type %in% types_use)

# Simplify MPAs
mpas_simple <- mpas_use %>%
  select(name)

# Simplify blocks
blocks_simple <- blocks %>%
  select(block_id)

# Calculate block area
blocks_area <- blocks_simple %>% 
  sf::st_area() %>% as.numeric() %>% 
  measurements::conv_unit(., "m2", "km2")

# Add block area to main block dataframe
blocks2 <- blocks %>% 
  mutate(block_area_km2 = blocks_area)

# Add block area to simple block dataframe
blocks_simple2 <- blocks_simple %>% 
  mutate(block_area_km2 = blocks_area)

# Test for any overlap among MPA polygons 
overlap <- sf::st_overlaps(mpas_simple, mpas_simple, sparse = FALSE)
isTRUE(overlap) # if FALSE, there is no overlap

# Intersect MPAs/blocks
data1 <- sf::st_intersection(x=blocks_simple, y=mpas_simple)

# Calculate MPA area
intersection_areas <- data1 %>%
  sf::st_area() %>% as.numeric() %>% measurements::conv_unit(., "m2", "km2")

# Add areas to intersection sf
data2 <- data1 %>%
  mutate(area_km2=intersection_areas)

# Summarize MPA coverage
block_stats <- data2 %>%
  sf::st_drop_geometry() %>%
  group_by(block_id) %>%
  dplyr::summarize(mpa_n=n_distinct(name),
                   mpas=paste(name, collapse=", "),
                   mpa_km2=sum(area_km2)) %>%
  ungroup()

# Add MPA block stats to block dataframe
blocks_stats_df <- blocks2 %>%
  filter(block_state=="California") %>%
  left_join(block_stats, by="block_id") %>% 
  sf::st_drop_geometry() %>% 
  select(block_id, block_area_km2, mpa_n, mpas, mpa_km2)

# Export as RDS
saveRDS(blocks_stats_df, file.path(outdir, "block_mpa_coverage_reduced_types.Rds"))


# P3: Plot data  ------------------------------------------------------------------

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
  geom_point(data=mpa_use_pts, mapping=aes(x=long_dd, y=lat_dd),
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
ggsave(g, filename=file.path(plotdir, "figure_mpa_block_coverage.png"),
       width=6.5, height=3.25, units="in", dpi=600)




