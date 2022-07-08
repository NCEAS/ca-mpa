

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
plotdir <- "analyses/2performance_fisheries/figures"

# Get blocks
blocks <- wcfish::blocks

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Read data
chrisdir <- "/Users/cfree/Dropbox/Chris/UCSB/consulting/halibut_bycatch/data/landings_receipts/processed/"
data_orig <- readRDS(file.path(chrisdir, "CDFW_2000_2020_landings_receipts.Rds"))

# To-do list
# 1. Check block assignments
# 2. Assign invalid blocks to regions


# Format data
################################################################################

# Region latitudes
region_lats <- c(39.0, 37.18, 34.5)

# Stats by year
stats_yr <- data_orig %>% 
  # Summarize by year and block (to make smaller)
  group_by(year, block_id) %>% 
  summarize(landings_lb=sum(landings_lb, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup() %>% 
  # Add lat
  left_join(blocks %>% select(block_id, block_lat_dd), by="block_id") %>% 
  # Assign region
  mutate(mlpa_region=cut(block_lat_dd, 
                         breaks=c(c(30, region_lats, 50)),
                         labels=c("South", "Central", "North Central", "North")) %>% as.character(),
         mlpa_region=ifelse(is.na(mlpa_region), "Unknown", mlpa_region),
         mlpa_region=factor(mlpa_region, 
                            levels=c("Unknown", "North", "North Central", "Central", "South"))) %>% 
  # Sum by region
  group_by(mlpa_region, year) %>% 
  summarize(landings_lb=sum(landings_lb, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup()

# Stats by block
stats_block <- data_orig %>% 
  # Summarize block
  group_by(block_id) %>% 
  summarize(landings_lb=sum(landings_lb, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup() %>% 
  # Spatialize
  right_join(blocks %>% filter(block_state=="California" & block_type=="Inshore")) %>% 
  sf::st_as_sf()


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   plot.tag = element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot() +
  # Plot blocks
  geom_sf(data=stats_block, mapping=aes(fill=value_usd), color="grey30", lwd=0.1) +
  # Plot regions
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Labels
  labs(x="", y="", tag="A") +
  # Legend
  scale_fill_gradientn(name="Revenues\n(USD, 2000-2020)",
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                       trans="log10", breaks=10^c(0:8),
                       labels=parse(text=paste0("10^", 0:8))) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Crop
  coord_sf(xlim = c(-125, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title=element_blank(),
        legend.position = c(0.7, 0.7),
        legend.key.size = unit(0.4, "cm"))
g1

# Plot data
g2 <- ggplot() +
  # Plot blocks
  geom_sf(data=stats_block, mapping=aes(fill=landings_lb), color="grey30", lwd=0.1) +
  # Plot regions
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Labels
  labs(x="", y="", tag="B") +
  # Legend
  scale_fill_gradientn(name="Landings\n(lbs, 2000-2020)",
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(), 
                       trans="log10", breaks=10^c(0:8),
                       labels=parse(text=paste0("10^", 0:8))) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Crop
  coord_sf(xlim = c(-125, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title=element_blank(),
        legend.position = c(0.7, 0.7),
        legend.key.size = unit(0.4, "cm"))
g2

# Plot data
region_colors <- c("grey80", RColorBrewer::brewer.pal(4, "Set2"))
g3 <- ggplot(stats_yr, aes(x=year, y=value_usd/1e6, fill=mlpa_region)) +
  geom_bar(stat="identity", color="grey30", lwd=0.2) +
  # Labels
  labs(x="Year", y="Revenues\n(USD millions)", tag="C") +
  # Legend
  scale_fill_manual(name="", values=region_colors) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.title.x=element_blank(),
        legend.position = c(0.25, 0.8), 
        legend.key.size = unit(0.2, "cm"))
g3

# Plot data
g4 <- ggplot(stats_yr, aes(x=year, y=landings_lb/1e6, fill=mlpa_region)) +
  geom_bar(stat="identity", color="grey30", lwd=0.2) +
  # Labels
  labs(x="Year", y="Landings\n(millions of pounds)", tag="D") +
  # Legend
  scale_fill_manual(name="", values=region_colors) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.title.x=element_blank(),
        legend.position = "none") 
g4

# Merge
layout_matrix <- matrix(c(1, 2, 3,
                          1, 2, 4), byrow=T, ncol=3)
g <- gridExtra::grid.arrange(g1, g2, g3, g4, 
                             layout_matrix=layout_matrix)
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_landings_revenues_by_mpa_region.png"), 
       width=6.5, height=3.0, units="in", dpi=600)



