

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
gisdir <- file.path(basedir, "gis_data/processed")
popdir <- file.path(basedir, "census_data/processed")
plotdir <- "analyses/3performance_human/figures"

# Read pop data
pop_ras <- readRDS(file=file.path(popdir, "CA_2010_census_tot_pop_by_block_raster.Rds")) %>% 
  filter(!is.na(people))
mpa_pop <- readRDS(file=file.path(popdir, "MPA_population_within_50km.Rds"))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")


# Plot data
################################################################################

# MPA regions
# CA/OR, Alder Creek, Pigeon Point, Point Conception, CA/MEX 
region_lats <- c(39.0, 37.18, 34.5)

# Region labels
region_labels <- tibble(long_dd=c(-123.5, -122.5, -121, -118, -119.2),
                        lat_dd=c(40.5, 38.5, 36, 33.9, 34.6),
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
                   legend.position = "right",
                   legend.key.size = unit(0.4, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot() +
  # Plot regions
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot raster
  geom_tile(data=pop_ras, mapping=aes(x=long_dd, y=lat_dd, fill=people_sqkm)) +
  # Plot MPAs
  geom_point(data=mpa_pop, mapping=aes(x=long_dd, y=lat_dd, size=npeople_50km/1e6)) +
  # Labels
  labs(x="", y="") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  scale_size_continuous(name="Millions of people\nwithin 50 km") +
  scale_fill_gradientn(name="Population density\n(people/sqkm)", 
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"),
                       trans="log10", na.value = NA) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Crop
  coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + my_theme
#g

# Export figure
ggsave(g, filename=file.path(plotdir, "Fig2_pop_data.png"), 
       width=4.75, height=5.25, units="in", dpi=600)


