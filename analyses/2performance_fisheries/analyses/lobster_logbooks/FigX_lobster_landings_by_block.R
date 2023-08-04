

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


# Build data
################################################################################

# Build data
range(data_orig$date)
data <- data_orig %>% 
  # Years
  filter(year>=2006) %>% 
  # Summarize
  group_by(block_id) %>% 
  summarise(nvessels=n_distinct(vessel_id),
            landings_tot=sum(n_kept, na.rm=T)) %>% 
  ungroup() %>% 
  # Calculate
  mutate(landings_yr=landings_tot/length(2006:2020)) %>% 
  # Filter
  filter(nvessels>=3)

# Spatialize
blocks_sf <- blocks %>% 
  left_join(data) %>% 
  filter(!is.na(landings_yr))
  



# Plot data
################################################################################

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_blank(),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   plot.subtitle=element_text(size=7, face=3),
                   plot.tag=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.4, "cm"),
                   legend.position = c(0.8, 0.8),
                   legend.key = element_rect(fill = NA), 
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Plot data
g <- ggplot() +
  # Plot blocks
  geom_sf(data=blocks_sf, mapping=aes(fill=landings_yr), color="grey60", lwd=0.1) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Labels
  labs(x="", y="", subtitle="A few rarely visited blocks hidden to comply with rule-of-three") +
  # Legend
  scale_fill_gradientn(name="Lobster per year\n(2006-2020 mean)",
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"), trans="log10") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Crop
  coord_sf(xlim = c(-122, -117), ylim = c(31.9, 39.5)) +
  # Theme
  theme_bw() + my_theme
g


# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_lobster_landings_by_block.png"), 
       width=4, height=6.5, units="in", dpi=600)




