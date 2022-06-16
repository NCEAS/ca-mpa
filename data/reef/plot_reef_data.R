
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data/"
indir <- file.path(basedir, "reef/raw")
outdir <- file.path(basedir, "reef/processed")
plotdir <- "data/reef/figures"

# Read data
data <- readRDS(file=file.path(outdir, "REEF_1994_2022_survey_metadata.Rds"))

# Sites
site_key <- data %>% 
  group_by(site_id, site_name, lat_dd, long_dd) %>% 
  summarize(n=n()) %>% 
  ungroup()

# Stats
stats <- data %>% 
  mutate(year=lubridate::year(date)) %>% 
  group_by(year, habitat) %>% 
  summarise(n=n()) %>% 
  ungroup()


# Plot data
################################################################################

# MPA regions
# CA/OR, Alder Creek, Pigeon Point, Point Conception, CA/MEX 
region_lats <- c(39.0, 37.18, 34.5)

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Theme
theme1 <-  theme(axis.text=element_text(size=7),
                 axis.text.y = element_text(angle = 90, hjust = 0.5),
                 axis.title=element_text(size=8),
                 plot.title=element_blank(),
                 legend.text=element_text(size=6),
                 legend.title=element_text(size=8),
                 plot.tag=element_text(size=9),
                 # Gridlines
                 panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(), 
                 axis.line = element_line(colour = "black"),
                 # Legend
                 legend.key = element_blank(),
                 legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot() +
  # Plot regions
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot REEF sites
  geom_point(data=site_key, mapping=aes(x=long_dd, y=lat_dd, size=n)) +
  # Labels
  labs(x="", y="", tag="A") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  # Legend
  scale_size_continuous(name="# of surveys") +
  # Crop
  coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + theme1 +
  theme(axis.title.y=element_blank(),
        legend.position = c(0.8, 0.7),
        legend.key.size = unit(0.4, "cm"))
g1

# Export data
ggsave(g, filename=file.path(plotdir, "reef_site_map.png"), 
       width=6.5, height=2.5, units="in", dpi=600)

# Plot data
g2 <- ggplot(stats, mapping=aes(x=year, y=n, fill=habitat)) +
  geom_bar(stat="identity", color="grey30", lwd=0.1) +
  # Labels
  labs(x="Year", y="# of surveys") +
  scale_x_continuous(breaks=seq(1995, 2020, 5)) +
  # Legend
  scale_fill_discrete(name="Habitat") +
  # Theme
  theme_bw() + theme1 +
  theme(legend.key.size = unit(0.3, "cm"))
g2






