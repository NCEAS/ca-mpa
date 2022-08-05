

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data/"
indir <- file.path(basedir, "mpa_watch/raw")
outdir <- file.path(basedir, "mpa_watch/processed")
gisdir <- file.path(basedir, "gis_data/processed")
plotdir <- "data/mpa_watch/figures"

# Read MPA data
mpas <- readRDS(file.path(gisdir, "CA_MPA_polygons.Rds"))

# Read MPA Watch data
sites <- readRDS(file=file.path(outdir, "mpa_watch_survey_sites_clean.Rds"))


# Plot data
################################################################################

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# MPA regions
# CA/OR, Alder Creek, Pigeon Point, Point Conception, CA/MEX 
region_lats <- c(39.0, 37.18, 34.5)

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_blank(),
                   legend.text=element_text(size=5.5),
                   legend.title=element_text(size=7),
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
  # Plot MPAs
  geom_sf(data=mpas, fill="red", color="black") +
  # Plot sites
  geom_point(data=sites, mapping=aes(x=long_dd, y=lat_dd, color=site_type)) +
  # Labels
  labs(x="", y="") +
  scale_color_discrete(name="Site type") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  # Crop
  coord_sf(xlim = c(-120.5, -117), ylim = c(32.5, 34.5)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.2, 0.1))
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_mpa_watch_survey_sites.png"), 
       width=3.5, height=5.25, units="in", dpi=600)

