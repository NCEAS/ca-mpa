

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
datadir <- "analyses/3performance_human/output"
plotdir <- "analyses/3performance_human/figures"

# Read data
data_orig <- readRDS(file=file.path(datadir, "CA_MPA_human_use_indicators.Rds"))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Build data
################################################################################

# Build data
data <- data_orig %>% 
  # Reduce to MPAs of interest
  filter(mlpa=="MLPA") %>% 
  # Order MPA type
  mutate(type=factor(type, 
                     levels=c("SMR", "SMCA (No-Take)", "SMCA", "SMRMA")))

# MPA regions
# CA/OR, Alder Creek, Pigeon Point, Point Conception, CA/MEX 
region_lats <- c(39.0, 37.18, 34.5)



# Plot data
################################################################################

# Theme
theme1 <-  theme(axis.text=element_text(size=6),
                 axis.text.y = element_text(angle = 90, hjust = 0.5),
                 axis.title=element_blank(),
                 plot.title=element_text(size=7, face="bold"),
                 legend.text=element_text(size=5),
                 legend.title=element_text(size=6),
                 plot.tag=element_text(size=7),
                 plot.tag.position = c(0, 0.99),
                 # Gridlines
                 panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(), 
                 axis.line = element_line(colour = "black"),
                 # Legend
                 legend.key = element_blank(),
                 legend.margin = margin(),
                 legend.position = c(0.73, 0.65),
                 legend.key.size = unit(0.25, "cm"),
                 legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot() +
  # Plot regions
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot data
  geom_point(data=data,
             mapping=aes(x=long_dd, y=lat_dd, size=nonconsump_hr, fill=nonconsump_psurveys), 
             pch=21, inherit.aes = F) +
  # Labels
  labs(x="", y="", title="MPA Watch (non-consumptive)", tag="A") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  scale_x_continuous(breaks=seq(-124, -116, 2)) +
  # Legend
  scale_size_continuous(name="Activities per hour") +
  scale_fill_gradientn(name="% of surveys", colors=RColorBrewer::brewer.pal(9, "Blues"), labels=scales::percent) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", order=2), size=guide_legend(order=1)) +
  # Crop
  coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + theme1
g1


# Plot data
g2 <- ggplot() +
  # Plot regions
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot MPAs
  geom_point(data=data,
             mapping=aes(x=long_dd, y=lat_dd, 
                         size=consump_hr, fill=consump_psurveys, pch=type), #  pch=type, if plotting shape
             inherit.aes = F) + #  pch=21, if not plotting shape
  # Labels
  labs(x="", y="", title="MPA Watch (consumptive)", tag="B") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  scale_x_continuous(breaks=seq(-124, -116, 2)) +
  # Legend
  scale_size_continuous(name="Activities per hour") +
  scale_shape_manual(name="MPA type", values=c(21, 22, 23, 24)) + # if plotting shape
  scale_fill_gradientn(name="% of surveys", colors=RColorBrewer::brewer.pal(9, "Blues"), labels=scales::percent) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", order=2), size=guide_legend(order=1)) +
  # Crop
  coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + theme1
g2

# Plot data
g3 <- ggplot() +
  # Plot regions
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot MPAs
  geom_point(data=data, mapping=aes(x=long_dd, y=lat_dd, 
                                     size=inat_observers_n, fill=inat_observations_n), pch=21, inherit.aes = F) +
  # Plot zero MPAs
  # geom_point(data=mpas_zero, mapping=aes(x=long_dd, y=lat_dd), pch="x", size=2.3) +
  # Labels
  labs(x="", y="", title="iNaturalist", tag="C") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  scale_x_continuous(breaks=seq(-124, -116, 2)) +
  # Legend
  scale_size_continuous(name="# of observers") +
  scale_fill_gradientn(name="# of observations", colors=RColorBrewer::brewer.pal(9, "Blues")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", order=2), size=guide_legend(order=1)) +
  # Crop
  coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + theme1
g3

# Plot data
g4 <- ggplot() +
  # Plot regions
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot MPAs
  geom_point(data=data, 
             mapping=aes(x=long_dd, y=lat_dd, size=ebird_observers_n, fill=ebird_surveys_n), # change to surveys
             pch=21, inherit.aes = F) +
  # Plot zero MPAs
  # geom_point(data=mpas_zero, mapping=aes(x=long_dd, y=lat_dd), pch="x", size=2.3) +
  # Labels
  labs(x="", y="", title="eBird", tag="D") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  scale_x_continuous(breaks=seq(-124, -116, 2)) +
  # Legend
  scale_size_continuous(name="# of observers") +
  scale_fill_gradientn(name="# of surveys", colors=RColorBrewer::brewer.pal(9, "Blues")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", order=2), size=guide_legend(order=1)) +
  # Crop
  coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + theme1
g4

# Plot data
g5 <- ggplot() +
  # Plot regions
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot MPAs
  geom_point(data=data, mapping=aes(x=long_dd, y=lat_dd, size=permits_n, fill=permits_nyr), pch=21) +
  # Labels
  labs(x="", y="", title="Scientific permits", tag="E") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  scale_x_continuous(breaks=seq(-124, -116, 2)) +
  # Legend
  scale_fill_gradientn(name="# of years with permits", colors=RColorBrewer::brewer.pal(9, "Blues")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  scale_size_continuous(name="# of scientific permits") +
  # Crop
  coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + theme1
g5

# Plot data
g6 <- ggplot() +
  # Plot regions
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot MPAs
  geom_point(data=data, 
             mapping=aes(x=long_dd, y=lat_dd, size=citations_n, fill=citations_nyr), pch=21) +
  # Plot zero MPAs
  # geom_point(data=mpas_zero, mapping=aes(x=long_dd, y=lat_dd), pch="x", size=2.3) +
  # Labels
  labs(x="", y="", title="Regulatory citations", tag="F") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  scale_x_continuous(breaks=seq(-124, -116, 2)) +
  # Legend
  scale_fill_gradientn(name="# of years with citations", colors=RColorBrewer::brewer.pal(9, "Blues")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  scale_size_continuous(name="# of citations") +
  # Crop
  coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + theme1
g6

# Merge plots
g <- gridExtra::grid.arrange(g1, g3, g5, 
                             g2, g4, g6, ncol=3, nrow=2)
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig2_indicator_maps.png"), 
       width=6.5, height=6.75, units="in", dpi=600)

