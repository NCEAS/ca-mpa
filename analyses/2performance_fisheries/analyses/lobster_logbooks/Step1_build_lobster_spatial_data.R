

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
plotdir <- "analyses/2performance_fisheries/analyses/lobster_logbooks/figures" 
outdir <- "analyses/2performance_fisheries/analyses/lobster_logbooks/output"

# Read logbook data
data_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/california/cdfw_data/data/confidential/lobster_logbooks/processed/CDFW_2000_2020_logbook_data.Rds")

# Blocks
blocks <- wcfish::blocks
blocks_sp <- sf::as_Spatial(blocks)

# MPAs
mpas <- wcfish::mpas_ca %>% 
  sf::st_as_sf() %>% 
  filter(type!="SMP")

# Projections
utm11 <- "+proj=utm +zone=11 +datum=NAD83"


# Learned from this
# 1) Some logbooks have XY but no logbook id - should we assign block id to these?
# 2) Some logbooks have INVALID block ids that might be able to be udpated based on location NAME

# Build data
################################################################################

# Step 1. Reduce to data with lat/long
data_xy <- data_orig %>% 
  filter(!is.na(lat_dd) & !is.na(long_dd))

# Step 2. Convert lat/long data to SP
data_xy_sp <- data_xy %>% 
  # Convert to SF
  sf::st_as_sf(coords=c("long_dd", "lat_dd"), crs=sf::st_crs(mpas), remove=F) %>% 
  # Convert to SP
  sf::as_Spatial()

# Step 3. Intersect with blocks
data_xy_block_ids <- sp::over(data_xy_sp, blocks_sp) %>% 
  pull(block_id)

# Step 4. Add block ids to data
data_xy1 <- data_xy %>% 
  # Record block ids
  mutate(block_id_latlong=data_xy_block_ids) %>% 
  # Record reliable point?
  mutate(reliable_yn=ifelse(block_id==block_id_latlong & !is.na(block_id_latlong) & !is.na(block_id), "yes", "no")) %>% 
  # Arrange
  select(logbook_id:lat_dd, block_id_latlong, reliable_yn, everything())


# Plot data
################################################################################

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_blank(),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   plot.tag=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.4, "cm"),
                   legend.key = element_rect(fill = NA), 
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Plot data
g1 <- ggplot() +
  # Plot blocks
  geom_sf(data=blocks, color="grey60", fill=NA, lwd=0.1) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot points
  geom_point(data=data_xy1, mapping=aes(x=long_dd, y=lat_dd), pch=21, size=0.8, alpha=0.5) +
  # Labels
  labs(x="", y="", tag="A") +
  # Axes
  # scale_y_continuous(breaks=32:42) +
  # Crop
  coord_sf(xlim = c(-122, -117), ylim = c(31.9, 39.5)) +
  # Theme
  theme_bw() + my_theme
g1

# Plot data
g2 <- ggplot() +
  # Plot blocks
  geom_sf(data=blocks, color="grey60", fill=NA, lwd=0.1) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot points
  geom_point(data=data_xy1, mapping=aes(x=long_dd, y=lat_dd, color=reliable_yn), pch=21, size=0.8, alpha=0.5) +
  # Labels
  labs(x="", y="", tag="B") +
  scale_color_discrete(name="Reliable point?") +
  # Axes
  # scale_y_continuous(breaks=32:42) +
  # Crop
  coord_sf(xlim = c(-122, -117), ylim = c(31.9, 39.5)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.75, 0.8))
g2

# Plot data
g3 <- ggplot() +
  # Plot blocks
  geom_sf(data=blocks, color="grey60", fill=NA, lwd=0.1) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot points
  geom_point(data=data_xy1 %>% filter(reliable_yn=="yes"), mapping=aes(x=long_dd, y=lat_dd), 
             pch=21, size=0.8, alpha=0.5) +
  # Labels
  labs(x="", y="", tag="C") +
  # Axes
  # scale_y_continuous(breaks=32:42) +
  # Crop
  coord_sf(xlim = c(-122, -117), ylim = c(31.9, 39.5)) +
  # Theme
  theme_bw() + my_theme
g3

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1)
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_reliable_lobster_xy_data.png"), 
       width=6.5, height=3.7, units="in", dpi=600)


# Plot data (zoom)
################################################################################

# Plot data
g <- ggplot() +
  # Plot blocks
  geom_sf(data=blocks, color="grey60", fill=NA, lwd=0.1) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot points
  geom_point(data=data_xy1 %>% filter(reliable_yn=="yes"), mapping=aes(x=long_dd, y=lat_dd), 
             pch=21, size=0.8, alpha=0.3, color="grey30") +
  # Plot MPAs
  geom_sf(data=mpas, fill="red", color=NA, alpha=0.6) +
  # Labels
  labs(x="", y="", tag="C") +
  # Axes
  # scale_y_continuous(breaks=32:42) +
  # Crop
  coord_sf(xlim = c(-120.5, -117), ylim = c(32.3, 34.5)) +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_reliable_lobster_xy_data_zoom.png"), 
       width=6.5, height=5, units="in", dpi=600)



# Assign to nearest MPA
################################################################################

# Convert to SP
data_xy_sp_utm <- data_xy_sp %>% 
  sp::spTransform(utm11)

# Convert MPAs to SP
mpas_sp <- mpas %>% 
  sf::st_transform(crs=utm11) %>% 
  sf::as_Spatial()

# Calculate distance of each point to an MPA
dist_mat <- rgeos::gDistance(data_xy_sp_utm, mpas_sp, byid=T)

# Format distance matrix
dist_df <- dist_mat %>% 
  as.data.frame() %>% 
  # Add MPA id
  mutate(mpa=mpas$name) %>% 
  select(mpa, everything()) %>% 
  # Gather
  gather(key="row_id", value="dist_m", 2:ncol(.)) %>% 
  # Find closest MPA to each point
  group_by(row_id) %>% 
  arrange(dist_m) %>% 
  slice(1) %>% 
  ungroup() %>% 
  # Convert row id to number and sort
  mutate(row_id=as.numeric(row_id)) %>% 
  arrange(row_id) %>% 
  select(row_id, mpa, dist_m)

# Add distance back onto data
data_xy2 <- data_xy1 %>% 
  cbind(dist_df %>% select(mpa, dist_m)) %>% 
  # Add distance in kilometers
  mutate(dist_km=dist_m/1000)


# Export data
################################################################################

# Save data
saveRDS(data_xy2, file=file.path(outdir, "CDFW_2017_2020_lobster_logbook_data_w_latlong.Rds"))

