## Create Histogram Showing Amount of MPA in Each Block
## Cori Lopazanski; June 13 2022

# Setup ------------------------------------------------------------------------

## Clear workspace
rm(list = ls())

##Packages
library(tidyverse)

## Directories
base.dir <- "/Volumes/GoogleDrive-105151121202188525604/Shared drives/NCEAS MPA network assessment/MPA Network Assessment: Working Group Shared Folder/data/sync-data" # Cori Local
gis.dir <- file.path(base.dir, "gis_data/processed")
out.dir <- file.path(gis.dir, "CA_block_polygons_buffers")
plot.dir <- "data/gis_data/figures"

  
## Read Data ----

### Read Block Stats
block_stats <- readRDS(file.path(gis.dir,"CA_blocks_stats.Rds"))

### Read MPA Data
mpas <- readRDS(file.path(gis.dir, "CA_MPA_polygons.Rds"))

## Calculate block-level MPA proportion ----
block_key <- block_stats %>% 
  mutate(percent_mpa = mpa_km2/block_area_km2*100)

## Steps ----

# Create buffered blocks - they will overlap with each other
# Read the shapefile
# Only look at SMRs/SMCAs and FMRs, FMCAs
# Simplify MPAs/blocks
# Calculate block area
# Intersect MPAs/blocks
# Calculate MPA area
# 

# Buffer block data -------------------------------------------------------------

# Buffer distances
buffers_km <- c(0.5, 1, 3, 5, 10, 20, 50)

# Loop through buffer distances
for(i in 1:length(buffers_km)){
  
  # Buffer
  buffer_do <- buffers_km[i]
  
  # Buffer MPAs
  block_buffer <- sf::st_buffer(block_stats, dist=buffer_do*1000)
  
  # Plot data
  if(F){
    g <- ggplot() +
      geom_sf(data=block_stats, color="black", fill=NA) +
      geom_sf(data=block_buffer, color="grey30", fill=NA)
    g
  }
  
  # Export data
  filename <- paste0("CA_block_polygons_", buffer_do, "km_buffer.Rds")
  saveRDS(block_buffer, file.path(out.dir,  filename))
  
}

## Read Buffer
buf_3k <- readRDS(file.path(out.dir, "CA_block_polygons_3km_buffer.Rds"))


ggplot() +
  geom_sf(data=block_stats, color="black") +
  geom_sf(data=buf_3k, color="grey30")

# Histograms with Raw Area -----------------------------------------------------

# All Blocks with MPAs
g1 <- ggplot(data = block_mpa_key) +
  geom_histogram(aes(mpa_km2))

## Less than 10 square km of MPA
g2 <- ggplot(data = block_mpa_key %>% 
               filter(mpa_km2 < 10)) +
  geom_histogram(aes(mpa_km2))

## Less than 2.5 square km of MPA
g3 <- ggplot(data = block_mpa_key %>% 
               filter(mpa_km2 < 2.5)) +
  geom_histogram(aes(mpa_km2))

### Merge
g <- gridExtra::grid.arrange(g1, g2, g3, nrow=3)


### Export figure
ggsave(g, filename=file.path(plotdir, "figure_mpa_block_coverage_dist_area.png"),
       width=6.5, height=3.25, units="in", dpi=600)

# Histograms with Percent Coverage ---------------------------------------------
p1 <- ggplot(data = block_mpa_key) +
        geom_histogram(aes(percent_mpa))

p2 <- ggplot(data = block_mpa_key %>% 
               filter(percent_mpa < 10)) +
        geom_histogram(aes(percent_mpa))

p3 <- ggplot(data = block_mpa_key %>% 
               filter(percent_mpa < 5)) +
        geom_histogram(aes(percent_mpa))

p4 <- ggplot(data = block_mpa_key %>% 
               filter(percent_mpa < 1)) +
  geom_histogram(aes(percent_mpa))

p <- gridExtra::grid.arrange(p1, p2, p3, p4, nrow=4)

### Export figure
ggsave(g, filename=file.path(plotdir, "figure_block_mpa_coverage_dist_percent.png"),
       width=6.5, height=3.25, units="in", dpi=600)