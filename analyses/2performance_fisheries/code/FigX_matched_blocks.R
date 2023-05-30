
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
data_orig <- readRDS(file.path(datadir, "block_counterfactual_key.Rds"))

# Get blocks
blocks <- wcfish::blocks



# Build data
################################################################################

# Build data
data <- data_orig  %>%
  # Reduce
  select(block_id, subclass, block_treatment) %>% 
  rename(pair=subclass, block_type=block_treatment) %>% 
  # Format treatment
  mutate(block_type=ifelse(block_type==0, "Control", "MPA") %>% factor(., levels=c("MPA", "Control"))) %>% 
  # Add spatial
  left_join(blocks %>% select(block_id, block_lat_dd, block_long_dd), by="block_id") %>% 
  sf::st_as_sf()

# Pairs
pair_lines <- data %>% 
  select(-geometry)


  


# Plot data
################################################################################

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# MPA regions
# CA/OR, Alder Creek, Pigeon Point, Point Conception, CA/MEX 
region_lats <- c(39.0, 37.18, 34.5)

my_theme <-  theme(axis.text=element_text(size=6),
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
                   legend.position = "none",
                   legend.key.size = unit(0.3, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Treatment
colors <- rainbow(n_distinct(data$pair))  %>%  sample()
g <- ggplot() +
  # Plot blocks
  geom_sf(data=data, mapping=aes(fill=pair), color="grey60", lwd=0.1, alpha=0.3) + # block_type
  # Plot regions
  geom_hline(mapping=aes(yintercept=region_lats), color="grey60", linetype="dotted") +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot lines
  geom_path(data=pair_lines, mapping=aes(x=block_long_dd, y=block_lat_dd, group=pair, color=pair)) +
  # Labels 
  labs(x="", y="") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  # Legend
  # scale_fill_manual(name="Block type", values=viridis(2)) +
  scale_fill_manual(values=colors ) +
  scale_color_manual(values=colors ) +
  # Crop
  coord_sf(xlim = c(-125, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_matched_blocks.png"), 
       width=3.5, height=4.8, units="in", dpi=600)


# Zoom
g <- ggplot() +
  # Plot blocks
  geom_sf(data=data, mapping=aes(fill=pair), color="grey60", lwd=0.1, alpha=0.3) + # block_type
  # Plot regions
  geom_hline(mapping=aes(yintercept=region_lats), color="grey60", linetype="dotted") +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot lines
  geom_path(data=pair_lines, mapping=aes(x=block_long_dd, y=block_lat_dd, group=pair, color=pair)) +
  # Labels 
  labs(x="", y="") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  # Legend
  # scale_fill_manual(name="Block type", values=viridis(2)) +
  scale_fill_manual(values=colors ) +
  scale_color_manual(values=colors ) +
  # Crop
  coord_sf(xlim = c(-121, -117), ylim = c(32.5, 35)) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_matched_blocks_south.png"), 
       width=5, height=4, units="in", dpi=600)
