

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
plotdir <- "analyses/2performance_fisheries/analyses/lobster_logbooks/figures" 
outdir <- "analyses/2performance_fisheries/analyses/lobster_logbooks/output"

# Read data
data_orig <- readRDS(file=file.path(outdir, "CDFW_2017_2020_lobster_logbook_data_w_latlong.Rds"))

# MPAs
mpas <- wcfish::mpas_ca %>% 
  sf::st_as_sf() %>% 
  filter(type!="SMP")


# Build data
################################################################################

# Logbooks use
data <- data_orig %>% 
  # Reliable points
  filter(reliable_yn=="yes" & !is.na(n_traps_pulled) & !is.na(n_nights)) %>% 
  # Compute CPUE
  mutate(trap_nights=n_traps_pulled * n_nights,
         cpue=n_kept/trap_nights) 

# Bin data
data_binned <- data %>% 
  # Convert distance to bins
  mutate(dist_catg=cut(dist_km, breaks=seq(0,112,0.5), 
                      label=seq(0,111.5,0.5), right=F) %>% as.character() %>% as.numeric()) %>% 
  # Summarize
  group_by(dist_catg) %>% 
  summarize(nvessels=n_distinct(vessel_id), 
            trap_nights=sum(trap_nights, na.rm=T)) %>% 
  ungroup()

# Setup theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=10),
                   plot.subtitle=element_text(size=10, face=3),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Plot data
g <- ggplot(data_binned, aes(x=dist_catg, y=trap_nights/1e3, fill=nvessels>=3)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Distance from nearest MPA (km)", y="Thousands of trap nights\n(from 2017-2020, cumlatively)") +
  scale_x_continuous(breaks=seq(0, 110, 5)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_reliable_lobster_fishing_line_bar.png"), 
       width=6.5, height=5, units="in", dpi=600)



# Fishing the line (non-conf)
################################################################################


# Plot data
g <- ggplot(data_binned %>% filter(nvessels>=3), aes(x=dist_catg, y=trap_nights/1e3)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Distance from nearest MPA (km)", 
       y="Thousands of trap nights\n(from 2017-2020, cumlatively)",
       subtitle = "A few rare bins hidden to comply with rule-of-three") +
  scale_x_continuous(breaks=seq(0, 110, 5), lim=c(0,30)) +
  scale_y_continuous(lim=c(0, 500)) +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_reliable_lobster_fishing_line_bar_nonconf.png"), 
       width=6.5, height=5, units="in", dpi=600)

# Build raster
################################################################################

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Convert data to sf
data_sf <- data %>% 
  sf::st_as_sf(coords=c("long_dd", "lat_dd"), crs=sf::st_crs(usa))

# Build raster template
bbox <- sf::st_bbox(data_sf)
ras_temp <- raster::raster(xmn=bbox$xmin, xmx=bbox$xmax, 
                           ymn=bbox$ymin, ymx=bbox$ymax, resolution=0.005)

# Build raster
data_ras <- raster::rasterize(x=data_sf, y=ras_temp, field="trap_nights", fun="sum")

# Convert to datafram
data_ras_df <- data_ras %>% 
  raster::as.data.frame(xy=T) %>% 
  setNames(c("long_dd", "lat_dd", "trap_nights")) %>% 
  filter(!is.na(trap_nights))


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_blank(),
                   plot.title=element_blank(),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot() +
  geom_tile(data=data_ras_df, mapping=aes(x=long_dd, y=lat_dd, fill=trap_nights)) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot MPAs
  geom_sf(data=mpas, fill=NA, color="grey30", lwd=0.1) +
  # Crop
  coord_sf(xlim = c(-120.5, -117), ylim = c(32.3, 34.5)) +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_fill_gradientn(name="Trap nights", 
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"), 
                       trans="log10", breaks=c(1, 10, 100, 1000, 10000)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.2, 0.2))
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_fishing_effort_raster.png"), 
       width=6.5, height=5, units="in", dpi=600)


# Plot data
g <- ggplot() +
  geom_tile(data=data_ras_df %>% filter(long_dd > -117.5 & long_dd < -117 & lat_dd < 33), 
            mapping=aes(x=long_dd, y=lat_dd, fill=trap_nights)) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot MPAs
  geom_sf(data=mpas, fill=NA, color="grey30", lwd=0.1) +
  # Crop
  coord_sf(xlim = c(-117.5, -117), ylim = c(32.5, 33)) +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_fill_gradientn(name="Trap nights", 
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"), 
                       trans="log10", breaks=c(1, 10, 100, 1000, 10000)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.2, 0.2))
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_fishing_effort_raster_south.png"), 
       width=3.5, height=4.25, units="in", dpi=600)



# Plot data
g <- ggplot() +
  geom_tile(data=data_ras_df %>% filter(long_dd > -120.5 & long_dd < -119.25 & lat_dd > 33.75 & lat_dd < 34.25), 
            mapping=aes(x=long_dd, y=lat_dd, fill=trap_nights)) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot MPAs
  geom_sf(data=mpas, fill=NA, color="grey30", lwd=0.1) +
  # Crop
  coord_sf(xlim = c(-120.5, -119.25), ylim = c(33.75, 34.25)) +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_fill_gradientn(name="Trap nights", 
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"), 
                       trans="log10", breaks=c(1, 10, 100, 1000, 10000)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.1, 0.2),
        legend.key.size=unit(0.2, "cm"))
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_fishing_effort_raster_channel.png"), 
       width=4, height=2.5, units="in", dpi=600)






