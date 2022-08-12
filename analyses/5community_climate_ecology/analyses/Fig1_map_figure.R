

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
gisdir <- file.path(basedir, "gis_data/processed")
plotdir <- "analyses/5community_climate_ecology/figures"

# Read site locations
load(file.path(basedir, "monitoring/site_locations.rda"))

# Read data
state_waters_poly <- readRDS(file.path(gisdir, "CA_state_waters_polygons.Rds"))
state_waters_line <- readRDS(file.path(gisdir, "CA_state_waters_polyline.Rds"))
mpas_orig <- readRDS(file.path(gisdir, "CA_MPA_polygons.Rds"))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Format data
################################################################################

# Reduce to MPAs of interest
sort(unique(mpas_orig$type))
types_use <- c("SMR", "SMCA (No-Take)")
mpas <- mpas_orig %>% 
  filter(type %in% types_use)

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
                        "rocky"="Rocky intertidal",
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
base_theme <-  theme(axis.text=element_text(size=7),
                     axis.title=element_text(size=8),
                     legend.text=element_text(size=7),
                     legend.title=element_text(size=8),
                     plot.tag=element_text(size=8),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.key = element_rect(fill=alpha('blue', 0)),
                     legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot() +
  # Plot regions
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot MPAs
  # geom_sf(data=mpas, mapping=aes(fill=type), color=NA) +
  # Plot sites
  geom_point(data=sites, mapping=aes(x=long_dd, y=lat_dd, color=habitat, shape=site_type)) +
  # Plot state waters
  geom_sf(data=state_waters_line, color="grey40", lwd=0.1) +
  # Plot region labels
  geom_text(data=region_labels, mapping=aes(x=long_dd, y=lat_dd, label=label), hjust=0, size=2.3) +
  # Labels
  labs(x="", y="", tag="A") +
  # Legend
  scale_color_discrete(name="Habitat") +
  scale_shape_manual(name="Site type", values=c(16, 4)) +
  # Axes
  scale_y_continuous(breaks=32:42) +
  # Crop
  coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title.y=element_blank(),
        legend.position = c(0.8, 0.7), # when color/size legends
        # legend.position = c(0.8, 0.8), # when only color legend
        legend.key.size = unit(0.4, "cm"))
g1

# Plot lat density
g2 <- ggplot(sites, aes(y=lat_dd, fill=habitat)) +
  geom_density(alpha=0.3) +
  # Labels
  labs(x="Density of\nmonitoring sites", y="Latitude (°N)", tag="B") +
  # Limits
  scale_y_continuous(lim=c(32.5, 42), breaks=33:42) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g2

# Merge figures
g <- gridExtra::grid.arrange(g1, g2, ncol=2, widths=c(0.7, 0.3))
g

# Export figure
ggsave(g, filename=file.path(plotdir, "Fig1_map_figure.png"), 
       width=6.5, height=6.5, units="in", dpi=600)


