

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
plotdir <- "analyses/3performance_human/figures"
gisdir <- file.path(basedir, "gis_data/processed")
outputdir <- "analyses/3performance_human/output"

# Read site locations
load(file.path(basedir, "monitoring/site_locations.rda"))

# Read data
state_waters_poly <- readRDS(file.path(gisdir, "CA_state_waters_polygons.Rds"))
state_waters_line <- readRDS(file.path(gisdir, "CA_state_waters_polyline.Rds"))
mpas_orig <- readRDS(file.path(gisdir, "CA_MPA_polygons.Rds"))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Reduce to MPAs of interest
sort(unique(mpas_orig$type))
types_use <- c("SMR", "SMRMA", "SMCA", "SMCA (No-Take)")
types <- c("SMR", "SMRMA", "SMCA (No-Take)", "SMCA", "SMP", "Special Closure", "FMR", "FMCA")
mpas <- mpas_orig %>% 
  # Order types
  mutate(type=factor(type, types))


# Format sites
sites <- site_locations %>% 
  # Rename
  rename(long_dd=lon, lat_dd=lat, habitat=group, site_type=mpa_designation) %>% 
  # Format lat/long
  mutate(long_dd=as.numeric(long_dd)) %>% 
  # Format habitat
  mutate(habitat=recode(habitat, 
                        "ccfrp"="Rocky reef",
                        "deep_reef"="Deep reef",
                        "kelp"="Kelp",
                        "Rocky intertidal"="rocky",
                        "surf-zone"="Surf zone")) %>% 
  # Format site type
  mutate(site_type=ifelse(site_type=="ref", "Reference", "MPA"))

# Plot data
################################################################################


# MPA regions
# CA/OR, Alder Creek, Pigeon Point, Point Conception, CA/MEX 
region_lats <- c(39.0, 37.18, 34.5)

# Region labels
region_labels <- tibble(long_dd=c(-123.9, -122.9, -121, -118, -119.5),
                        lat_dd=c(40.5, 38.7, 36, 34.1, 34.8),
                        label=c("North\n(Dec 2012)", "North Central\n(May 2010)", "Central\n(Sep 2007)", "South\n(Jan 2012)", "N. Channel\nIslands (2003)"))

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
                   legend.position = c(0.2, 0.3),
                   legend.key.size = unit(0.4, "cm"),
                   legend.key = element_rect(fill=alpha('blue', 0)),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot() +
  # Plot regions
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot MPAs
  geom_sf(data=mpas, fill=NA, color="black", lwd=0.5) +
  # Plot sampling sites
  geom_point(data=sites, mapping=aes(x=long_dd, y=lat_dd, color=site_type, shape=habitat)) +
  # Plot state waters
  geom_sf(data=state_waters_line, color="grey40", lwd=0.1) +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_color_discrete(name="Site type") +
  scale_shape_discrete(name="Habitat") +
  # Axes
  # scale_y_continuous(breaks=32:42) +
  # Crop
  coord_sf(xlim = c(-122.4, -121.4), ylim = c(36, 37)) +
  # Theme
  theme_bw() + my_theme
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigX_counterfactual.png"), 
       width=4.5, height=5.25, units="in", dpi=600)


