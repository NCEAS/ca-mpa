

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
outputdir <- "analyses/3performance_human/output"

# Read data
mpas <- readRDS(file=file.path(traitdir, "CA_mpa_metadata.Rds"))
data_orig <- readRDS(file=file.path(datadir, "CA_2012_2021_mpa_scientific_permits.Rds"))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")


# Survey coverage
################################################################################

# Build data
#####################################

# Build data
coverage <- data_orig %>% 
  # Add MPA meta data
  left_join(mpas %>% select(mpa, type, mlpa)) %>% 
  # MPAs of interest
  filter(mlpa=="MLPA")

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
  scale_fill_gradientn(name="# of permits", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + theme1
g  

# Export plot
ggsave(g, filename=file.path(plotdir, "FigS11_sci_permit_coverage.png"), 
       width=6.5, height=7.75, units="in", dpi=600)


# Build data
################################################################################

# Build data
data_full <- data_orig %>% 
  filter(year %in% 2012:2021) %>% 
  # Summ
  group_by(mpa) %>% 
  summarize(npermits=sum(npermits),
            nyears=n_distinct(year)) %>% 
  ungroup() %>% 
  # Add lat/long and type
  left_join(mpas %>% select(mpa, type, mlpa, lat_dd, long_dd,), by="mpa")
  
# Reduce
data <- data_full %>% 
  # Types of interest
  filter(mlpa=="MLPA")

# Export
saveRDS(data_full, file=file.path(outputdir, "scientific_permits_indicators.Rds"))

# Stats for manuscript
n_distinct(data$mpa)
sum(data$npermits)

# Data time series
data_ts <- data_orig %>% 
  # MPAs of interesrt
  left_join(mpas %>% select(mpa, type, mlpa), by="mpa") %>% 
  filter(mlpa=="MLPA") %>% 
  # Summarize
  group_by(year, region) %>% 
  summarize(npermits=sum(npermits)) %>% 
  ungroup()

# Proportion by MPA type
prop_ts <- data_orig %>% 
  # MPAs of interesrt
  left_join(mpas %>% select(mpa, type, mlpa), by="mpa") %>% 
  filter(mlpa=="MLPA") %>% 
  # Summarize
  group_by(year, type) %>% 
  summarize(npermits=sum(npermits)) %>% 
  ungroup() %>% 
  # Proportion
  group_by(year) %>% 
  mutate(permits_prop=npermits/sum(npermits)) %>% 
  # Order
  mutate(type=factor(type, levels=c("SMR", "SMCA", "SMCA (No-Take)", "SMRMA") %>% rev()))

# Proportion of network by type
network_prop <- mpas %>% 
  filter(mlpa=="MLPA") %>%
  # Summarize
  group_by(type) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  mutate(prop=n/sum(n)) %>% 
  # Order
  mutate(type=factor(type, levels=c("SMR", "SMCA", "SMCA (No-Take)", "SMRMA") %>% rev())) %>% 
  arrange(desc(type)) %>% 
  mutate(prop_cum=cumsum(prop))


# Plot data
################################################################################

# MPA regions
# CA/OR, Alder Creek, Pigeon Point, Point Conception, CA/MEX 
region_lats <- c(39.0, 37.18, 34.5)

# Region labels
region_labels <- tibble(long_dd=c(-123.5, -122.5, -121, -118, -119.2),
                        lat_dd=c(40.5, 38.5, 36, 33.9, 34.6),
                        label=c("North\n(Dec 2012)", "North Central\n(May 2010)", 
                                "Central\n(Sep 2007)", "South\n(Jan 2012)", "N. Channel\nIslands (2003)"))

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
  geom_point(data=data, mapping=aes(x=long_dd, y=lat_dd, size=npermits, fill=nyears), pch=21) +
  # Labels
  labs(x="", y="", tag="A") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  # Legend
  scale_fill_gradientn(name="# of years with permits", colors=RColorBrewer::brewer.pal(9, "Blues")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  scale_size_continuous(name="# of scientific permits\nissued from 2012-2021") +
  # Crop
  coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + theme1 + 
  theme(axis.title.y=element_blank(),
        legend.position = c(0.75, 0.7),
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

# Plot proportion by type
g3 <- ggplot(prop_ts, aes(x=year, y=permits_prop, fill=type)) +
  geom_bar(stat="identity", color="grey30", lwd=0.1, alpha=0.5) + 
  # Reference props
  geom_hline(data=network_prop, mapping=aes(yintercept=prop_cum, color=type), linetype="dashed") +
  # Labels
  labs(x="Year", y="Percent of permits", tag="C") +
  scale_x_continuous(breaks=2012:2021) +
  scale_y_continuous(labels=scales::percent) +
  # Legend
  scale_fill_discrete(name="Type", guide = guide_legend(reverse = TRUE)) +
  scale_color_discrete(name="Type", guide = guide_legend(reverse = TRUE)) +
  # Theme
  theme_bw() + theme1 +
  theme(legend.position = "top",
        legend.key.size = unit(0.3, "cm"))
g3

# Merge data
layout_matrix <- matrix(c(1,2, 
                          1,3), ncol=2, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, g3, layout_matrix=layout_matrix, widths=c(0.52, 0.48))

# Export
ggsave(g, filename=file.path(plotdir, "FigS12_scientific_permit_data.png"), 
       width=6.5, height=5.25, units="in", dpi=600)


