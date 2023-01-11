# Within-MPA Covariates
# Cori Lopazanski
# 21 Dec 2022

# Setup ------------------------------------------------------------------------

## Clear workspace
rm(list = ls())

## Packages ----
library(tidyverse)
library(sf)

## Directories ----
basedir <- "/Volumes/GoogleDrive-105151121202188525604/Shared drives/NCEAS MPA network assessment/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
datadir <- "/Volumes/GoogleDrive-105151121202188525604/My Drive/Research/NCEAS - California MPA Working Group/fisheries-data/processed"
gisdir <- file.path(basedir, "gis_data")
plotdir <- "analyses/2performance_fisheries/figures"

## Read data ---- 
port_totals <- readRDS(file.path(datadir, "annual_landings_per_port_lb.Rds"))
ports <- readRDS(file.path(gisdir, "processed", "CA_ports.Rds"))
blocks_orig <- wcfish::blocks
mpas <- readRDS(file.path(gisdir, "processed", "CA_MPA_polygons.Rds"))
blocks_with_mpas <- read_csv(file.path(gisdir, "processed", "CA_blocks_with_mpas_all_mpa_types.csv"))

# Build Data -------------------------------------------------------------------
## Join port totals with shapefile data ----
port_comb <- ports %>% 
  full_join(port_totals, by = c("port_code" = "port_id"))

## Reduce port shapefile to those in "major port" list
port_major <- port_comb %>% 
  filter(total < 99) # currently it's the top 99%

## Get major port centroids ----
port_major_centroids <- st_centroid(port_major)

## Get MPA centroid ----
mpa_centroids <- st_centroid(mpas)


# Calculate  -------------------------------------------------------------------

## Get nearest port for each MPA ----
nearest_port <- st_nearest_feature(mpa_centroids, port_major_centroids)

## Calculate distance from block centroid to port centroid ----
nearest_port_dist <- st_distance(mpa_centroids, 
                                 port_major_centroids[nearest_port,], by_element = TRUE) %>% 
  as.numeric() %>%
  measurements::conv_unit(., "m", "km")

## Join to MPA df ----
mpa_port_dist <- mpas %>% 
  select(name) %>% 
  mutate(dist_to_port_km = nearest_port_dist)

# Plot -------------------------------------------------------------------------
ggplot() +
  geom_sf(data = mpa_port_dist, color = "grey60", 
          mapping=aes(fill=dist_to_port_km), lwd = 0.1) +
  geom_sf(data = port_major_centroids, color = "red") +
  theme_classic()

# Export -----------------------------------------------------------------------
mpa_port_dist <- mpa_port_dist %>% 
  st_drop_geometry()

# To share drive
#saveRDS(mpa_port_dist, file.path(gisdir, "processed", "mpa_distance_to_port.Rds"))



