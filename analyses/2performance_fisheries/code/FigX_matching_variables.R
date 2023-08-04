
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
datadir <- "analyses/2performance_fisheries/analyses/blocks"
plotdir <- "analyses/2performance_fisheries/figures"

# Read data
data_orig <- readRDS(file.path(datadir, "block_all_covariates.Rds"))

# Get blocks
blocks <- wcfish::blocks



# Build data
################################################################################

# Build data
data <- blocks %>%
  select(block_id, block_type, block_state) %>% 
  # Add data
  left_join(data_orig, by="block_id") %>% 
  # Reduce
  filter(block_state=="California" & block_type=="Inshore") %>% 
  # Format treatment
  mutate(block_treatment=ifelse(block_treatment==0, "Control", "MPA") %>% factor(., levels=c("MPA", "Control"))) %>% 
  # Format landings
  mutate(total_kg=measurements::conv_unit(total_lb, "lbs", "kg"),
         total_mt=total_kg/1000)

  


# Plot data
################################################################################

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# MPA regions
# CA/OR, Alder Creek, Pigeon Point, Point Conception, CA/MEX 
region_lats <- c(39.0, 37.18, 34.5)

my_theme <-  theme(axis.text=element_text(size=5),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_blank(),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = c(0.7, 0.7),
                   legend.key.size = unit(0.3, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Treatment
g0 <- ggplot() +
  # Plot blocks
  geom_sf(data=data, mapping=aes(fill=block_treatment), color="grey60", lwd=0.1) +
  # Plot regions
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Labels
  labs(x="", y="") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  # Legend
  scale_fill_manual(name="Block type", values=viridis(2)) +
  # Crop
  coord_sf(xlim = c(-125, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + my_theme
#g0

# Area
g1 <- ggplot() +
  # Plot blocks
  geom_sf(data=data, mapping=aes(fill=block_area_km2), color="grey60", lwd=0.1) +
  # Plot regions
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Labels
  labs(x="", y="") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  # Legend
  scale_fill_gradientn(name="Block\narea (sqkm)", colors=RColorBrewer::brewer.pal(9, "Purples"), na.value=NA) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Crop
  coord_sf(xlim = c(-125, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + my_theme
#g1

# Depth
g2 <- ggplot() +
  # Plot blocks
  geom_sf(data=data, mapping=aes(fill=block_mean_depth_m), color="grey60", lwd=0.1) +
  # Plot regions
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Labels
  labs(x="", y="") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  # Legend
  scale_fill_gradientn(name="Block\ndepth (m)", colors=RColorBrewer::brewer.pal(9, "Blues") %>% rev(), na.value=NA) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Crop
  coord_sf(xlim = c(-125, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + my_theme
#g2

# Distance to shore
g3 <- ggplot() +
  # Plot blocks
  geom_sf(data=data, mapping=aes(fill=distance_to_shore_km), color="grey60", lwd=0.1) +
  # Plot regions
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Labels
  labs(x="", y="") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  # Legend
  scale_fill_gradientn(name="Distance\nto shore (km)", colors=RColorBrewer::brewer.pal(9, "Greens"), na.value=NA) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Crop
  coord_sf(xlim = c(-125, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + my_theme
#g3

# Distance to port
g4 <- ggplot() +
  # Plot blocks
  geom_sf(data=data, mapping=aes(fill=dist_to_port_km), color="grey60", lwd=0.1) +
  # Plot regions
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Labels
  labs(x="", y="") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  # Legend
  scale_fill_gradientn(name="Distance\nto port (km)", colors=RColorBrewer::brewer.pal(9, "Oranges"), na.value=NA) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Crop
  coord_sf(xlim = c(-125, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + my_theme
#g4

# Latitude
g5 <- ggplot() +
  # Plot blocks
  geom_sf(data=data, mapping=aes(fill=block_lat_dd), color="grey60", lwd=0.1) +
  # Plot regions
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Labels
  labs(x="", y="") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  # Legend
  scale_fill_gradientn(name=" \nLatitude (°N)", colors=RColorBrewer::brewer.pal(9, "Spectral"), na.value=NA) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Crop
  coord_sf(xlim = c(-125, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + my_theme

# Landings
g6 <- ggplot() +
  # Plot blocks
  geom_sf(data=data, mapping=aes(fill=total_mt), color="grey60", lwd=0.1) +
  # Plot regions
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Labels
  labs(x="", y="") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  # Legend
  scale_fill_gradientn(name="Annual\nlandings (mt)", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(), 
                       na.value=NA,
                       trans="log10") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Crop
  coord_sf(xlim = c(-125, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + my_theme
g6

# Merge
layout_matrix <- matrix(data=c(1,2,3,4,
                               1,5,6,7), byrow=T, nrow=2)
g <- gridExtra::grid.arrange(g0, g1, g2, g3, g4, g5, g6, nrow=2 ) # layout_matrix=layout_matrix
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_matching_variables.png"), 
       width=6.5, height=4.5, units="in", dpi=600)




