

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
datadir <- file.path(basedir, "scientific_permits/processed")
traitdir <- file.path(basedir, "mpa_traits/processed")
plotdir <- "analyses/3performance_human/figures"

# Read data
mpas <- readRDS(file=file.path(traitdir, "CA_mpa_metadata.Rds"))
data_orig <- readRDS(file=file.path(datadir, "CA_2012_2021_mpa_scientific_permits.Rds"))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# MPA types
types_use <- c("SMR", "SMRMA", "SMCA", "SMCA (No-Take)")


# Survey coverage
################################################################################

# Build data
#####################################

# Build data
coverage <- data_orig %>% 
  # Add MPA meta data
  left_join(mpas %>% select(mpa, type)) %>% 
  # MPAs of interest
  filter(type %in% types_use)

# MPA order
mpa_order <- coverage %>% 
  group_by(region, mpa) %>% 
  summarize(npermits=sum(npermits)) %>% 
  ungroup() %>% 
  arrange(region, desc(npermits))


# Plot data
#####################################

# Theme
theme1 <-  theme(axis.text=element_text(size=6),
                 axis.text.y=element_text(size=5),
                 axis.title=element_text(size=8),
                 axis.title.y=element_blank(),
                 legend.text=element_text(size=6),
                 legend.title=element_text(size=7),
                 strip.text=element_text(size=7),
                 # Gridlines
                 panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(), 
                 axis.line = element_line(colour = "black"),
                 # Legend
                 legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(coverage, aes(x=year, y=mpa %>% factor(., levels=mpa_order$mpa), fill=npermits)) +
  facet_grid(region~., space="free_y", scale="free_y") +
  geom_tile(color="grey30", lwd=0.05) +
  # Labels
  labs(x="Year", y="") +
  scale_x_continuous(breaks=2012:2021) +
  # Legend
  scale_fill_gradientn(name="# of surveys", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + theme1
g  

# Export plot
ggsave(g, filename=file.path(plotdir, "FigS4_sci_permit_coverage.png"), 
       width=6.5, height=7.75, units="in", dpi=600)


# Build data
################################################################################

# Build data
data <- data_orig %>% 
  # Summ
  group_by(mpa) %>% 
  summarize(npermits=sum(npermits)) %>% 
  ungroup() %>% 
  # Add lat/long and type
  left_join(mpas %>% select(mpa, type, lat_dd, long_dd), by="mpa") %>% 
  # Types of interest
  filter(type %in% types_use)

# Stats for manuscript
n_distinct(data$mpa)
sum(data$npermits)

# Data time series
data_ts <- data_orig %>% 
  group_by(year, region) %>% 
  summarize(npermits=sum(npermits)) %>% 
  ungroup()


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
  # Plot MPAs
  geom_point(data=data, mapping=aes(x=long_dd, y=lat_dd, size=npermits)) +
  # Labels
  labs(x="", y="", tag="A") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  # Legend
  scale_size_continuous(name="# of scientific permits\nissued from 2012-2021") +
  # Crop
  coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + theme1 + 
  theme(axis.title.y=element_blank(),
        legend.position = c(0.75, 0.8),
        legend.key.size = unit(0.4, "cm"))
g1

# Plot time series
g2 <- ggplot(data_ts, aes(x=year, y=npermits, fill=region)) +
  geom_bar(stat="identity", color="grey30", lwd=0.1) + 
  # Labels
  labs(x="Year", y="Number of permits", tag="B") +
  scale_x_continuous(breaks=2012:2021) +
  # Legend
  scale_fill_ordinal(name="Region") +
  # Theme
  theme_bw() + theme1 +
  theme(legend.position = c(0.8, 0.8),
        legend.key.size = unit(0.3, "cm"))
g2

# Merge data
layout_matrix <- matrix(c(1,2, 
                          1,3), ncol=2, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, layout_matrix=layout_matrix, widths=c(0.52, 0.48))

# Export
ggsave(g, filename=file.path(plotdir, "Fig6_scientific_permit_data.png"), 
       width=6.5, height=5.25, units="in", dpi=600)


