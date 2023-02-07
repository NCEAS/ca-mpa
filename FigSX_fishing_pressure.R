
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
datadir <- file.path(basedir, "data")
gisdir <- file.path(basedir, "gis_data/processed")
outputdir <- file.path("analyses/1performance_eco/output")
plotdir <- "analyses/5community_climate_ecology/figures"

# Read blocks
blocks <- wcfish::blocks

# Read MPAs
mpas <- readRDS(file.path(gisdir, "CA_MPA_polygons.Rds"))

# Read data
landings_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/california/cdfw_data/data/confidential/landings_receipts/processed/CDFW_2000_2020_landings_receipts.Rds")


# Build data
################################################################################

# Build data
landings <- landings_orig %>% 
  # 2000-2007
  filter(year %in% 2000:2006) %>% 
  # Summarize by year-block
  group_by(block_id, year) %>% 
  summarize(landings_lb=sum(landings_lb, na.rm=T)) %>% 
  ungroup() %>% 
  # Annual average
  group_by(block_id) %>% 
  summarise(landings_lb_yr=mean(landings_lb)) %>% 
  ungroup() %>% 
  # Add area
  left_join(blocks %>% select(block_id, block_type, block_sqkm), by="block_id") %>% 
  # Calculate pressure desity
  mutate(landings_lb_yr_sqkm=landings_lb_yr / block_sqkm) %>% 
  # Reduce to inshore
  filter(block_type=="Inshore") %>% 
  # Spatialize
  sf::st_as_sf()
  

# Plot data
################################################################################

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_blank(),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = c(0.2, 0.15), # when color/size legends
                   legend.key.size = unit(0.4, "cm"),
                   legend.key = element_rect(fill=alpha('blue', 0)),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot() +
  # Plot data
  geom_sf(data=landings, mapping=aes(fill=landings_lb_yr_sqkm), lwd=0.05) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot MPAs
  geom_sf(data=mpas, fill="black", color="black") +
  # Clip
  coord_sf(ylim=c(32.3, 42), xlim=c(-125, -116.5)) +
  # Legend
  scale_fill_gradientn(name="Annual landings\n(pounds/sqkm)", 
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"),
                       trans="log10",
                       breaks=c(1, 10, 100, 1000, 10000, 100000),
                       labels=c("1", "10", "100", "1,000", "10,000", "100,000")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_fishing_pressure.png"), 
       width=3.2, height=4.5, units="in", dpi=600)

