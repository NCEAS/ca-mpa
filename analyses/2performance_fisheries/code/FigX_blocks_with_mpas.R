# Display MPA Blocks

# Read data #################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(janitor)
library(sf)
library(rfishbase)

# Directories
basedir <- "/Users/lopazanski/Library/CloudStorage/GoogleDrive-lopazanski@ucsb.edu/Shared drives/NCEAS MPA network assessment/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
gisdir <- file.path(basedir, "gis_data/processed")
datadir <- "analyses/2performance_fisheries/analyses/blocks/data"
figdir <- "analyses/2performance_fisheries/analyses/blocks/figs"
fishdir <- "/Users/lopazanski/Documents/github/nceas/CDFW-fishing-data"

# Read data
mpas <- readRDS(file.path(gisdir, "CA_MPA_polygons.Rds"))
blocks <- readRDS(file.path(datadir, "blocks_by_mlpa_region_w_mpa_stats.Rds"))
landings_raw <- readRDS(file.path(fishdir, "CDFW_2000_2020_landings_receipts.Rds"))
#sp_key <- readRDS(file.path(fishdir, "CDFW_species_key.Rds"))# This seems incomplete
sp_key <- read_csv(file.path(fishdir, "CDFW_species_key.csv"))

# Plot Blocks & MPAs ####################################################
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
ggplot() +
  # Bblocks
  geom_sf(data=blocks %>% 
            filter(!mlpa_region %in% c("South of state", "North of state")), 
          mapping=aes(fill=mlpa_region, alpha=mpa_yn), lwd=0.2) +
  # Rregion lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Legend
  scale_fill_discrete(name="MLPA region") +
  scale_alpha_manual(name="Block type", values=c(1, 0.2)) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.75, 0.75),
        legend.key.size = unit(0.3, "cm"),
        legend.background = element_rect(fill=alpha("white", 1)))

ggsave(filename=file.path(figdir, "CA_blocks_test.png"))

# Zoom by region ##############################################################

## South ----
s <- ggplot() +
  # Blocks
  geom_sf(data=blocks, mapping=aes(alpha=mpa_yn), lwd=0.2) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # MPAs
  geom_sf(data=mpas, fill = "red", alpha = 0.5)+
  # Legend
  scale_fill_discrete(name="MLPA region") +
  scale_alpha_manual(name="Block type", values=c(1, 0.2)) +
  # Crop
  coord_sf(xlim = c(-121, -117), ylim = c(32.45, 34.55)) +
  # Theme
  theme_bw() + my_theme 
s
ggsave(s, filename=file.path(figdir, "CA_blocks_mpa_yn_south.png"))

## Central ----

c <- ggplot() +
  # Blocks
  geom_sf(data=blocks, mapping=aes(alpha=mpa_yn), lwd=0.2) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # MPAs
  geom_sf(data=mpas, fill = "red", alpha = 0.5)+
  # Legend
  scale_fill_discrete(name="MLPA region") +
  scale_alpha_manual(name="Block type", values=c(1, 0.2)) +
  # Crop
  coord_sf(xlim = c(-122.5, -120), ylim = c(34.5, 37.2)) +
  # Theme
  theme_bw() + my_theme 
c

ggsave(c, filename=file.path(figdir, "CA_blocks_mpa_yn_central.png"))


## North Central ----
nc <- ggplot() +
  # Blocks
  geom_sf(data=blocks, mapping=aes(alpha=mpa_yn), lwd=0.2) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # MPAs
  geom_sf(data=mpas, fill = "red", alpha = 0.5)+
  # Legend
  scale_fill_discrete(name="MLPA region") +
  scale_alpha_manual(name="Block type", values=c(1, 0.2)) +
  # Crop
  coord_sf(xlim = c(-124, -122), ylim = c(37.1, 39.5)) +
  # Theme
  theme_bw() + my_theme 
nc
ggsave(nc, filename=file.path(figdir, "CA_blocks_mpa_yn_northcentral.png"))


## North ----
n <- ggplot() +
  # Blocks
  geom_sf(data=blocks, mapping=aes(alpha=mpa_yn), lwd=0.2) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # MPAs
  geom_sf(data=mpas, fill = "red", alpha = 0.5)+
  # Legend
  scale_fill_discrete(name="MLPA region") +
  scale_alpha_manual(name="Block type", values=c(1, 0.2)) +
  # Crop
  coord_sf(xlim = c(-125, -123.5), ylim = c(39, 42)) +
  # Theme
  theme_bw() + my_theme 
n
ggsave(n, filename=file.path(figdir, "CA_blocks_mpa_yn_north.png"))

