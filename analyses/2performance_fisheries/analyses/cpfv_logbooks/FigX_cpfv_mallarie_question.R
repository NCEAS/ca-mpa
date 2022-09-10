

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
blockdir <- "analyses/2performance_fisheries/analyses/blocks"
cpfvdir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/california/cdfw_data/data/confidential/cpfv_logbooks/processed"
plotdir <- "analyses/2performance_fisheries/analyses/cpfv_logbooks/figures" 

# Read block data
blocks <- wcfish::blocks

# Read CPFV logbook data
data_orig <- readRDS(file.path(cpfvdir, "CDWF_2000_2020_cpfv_logbook_data.Rds"))


# Build data
################################################################################

port_complexes <- c("Eureka", "Fort Bragg", "Bodega Bay", "San Francisco",
                    "Sacramento Delta", "Monterey", "Morro Bay", 
                    "Santa Barbara", "Los Angeles", "San Diego")

# Build data
data <- data_orig %>% 
  # Reduce to valid blocks
  filter(!is.na(block_type)) %>% 
  # Summarize # of trips by block id
  group_by(port_complex, block_id, block_type) %>% 
  summarize(n_trips=n_distinct(logbook_id)) %>% 
  ungroup() %>% 
  # Remove offshore blocks
  filter(block_type!="Offshore") %>% 
  # Reduce to ports of interest
  filter(!port_complex %in% c("Invalid", "Oregon")) %>% 
  # Order ports
  mutate(port_complex=factor(port_complex, levels=port_complexes)) %>% 
  # Spatialize
  left_join(blocks %>% select(block_id, geometry), by="block_id") %>% 
  sf::st_as_sf()


# Plot data
################################################################################

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Setup theme
my_theme <-  theme(axis.text=element_text(size=5),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=7),
                   plot.title=element_text(size=8),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot() +
  # Facet
  facet_wrap(~port_complex, ncol=5) +
  # Plot blocks
  geom_sf(data=data, mapping=aes(fill=n_trips),
          color="grey30", lwd=0.1) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.2) +
  # Legend
  scale_fill_gradientn(name="# of CPFV trips\n(from 2000-2020)", trans="log10",
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"),
                       breaks=c(1, 10, 100, 1000, 10000)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.5, "cm"))
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_cpfv_fishing_effort.png"), 
       width=6.5, height=4.5, units="in", dpi=600)





