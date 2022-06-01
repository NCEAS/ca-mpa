

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
gisdir <- file.path(basedir, "gis_data/processed")
datadir <- file.path(basedir, "inaturalist/processed")
plotdir <- "analyses/3performance_human/figures"

# Read data
state_waters_poly <- readRDS(file.path(gisdir, "CA_state_waters_polygons.Rds"))
state_waters_line <- readRDS(file.path(gisdir, "CA_state_waters_polyline.Rds"))
mpas_orig <- readRDS(file.path(gisdir, "CA_MPA_polygons.Rds"))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Reduce to MPAs of interest
sort(unique(mpas_orig$type))
types_use <- c("SMR", "SMRMA", "SMCA", "SMCA (No-Take)")
mpas <- mpas_orig %>% 
  filter(type %in% types_use)
mpas_df <- mpas %>% 
  sf::st_drop_geometry()


# Build data
################################################################################

# Build data
data_orig <- readRDS(file.path(datadir, "2000_2020_inaturalist_data_inside_mpas_100m_buffer.Rds"))

# Summarize
stats <- data_orig %>% 
  # 2018
  filter(year_obs==2018 & !is.na(taxa_catg)) %>% 
  # Summarize
  group_by(mpa) %>% 
  summarize(nobservers=n_distinct(user_id),
            nobservations=n(),
            nspecies=n_distinct(sci_name)) %>% 
  ungroup()
  
# Spatialize
stats1 <- mpas_df %>% 
  select(name, region, area_sqkm, lat_dd, long_dd) %>% 
  left_join(stats, by=c("name"="mpa"))

# Time series stats
stats_ts <- data_orig %>% 
  # Before 2018
  filter(year_obs<=2018 & !is.na(taxa_catg)) %>% 
  # Summarize
  group_by(year_obs, taxa_catg) %>% 
  summarize(nobservations=n()) %>% 
  ungroup() %>% 
  # Rename taxa
  mutate(taxa_catg=recode(taxa_catg,
                          "Actinopterygii"="Fish",
                          "Amphibia"="Amphibians",     
                          "Animalia"="Other",  
                          "Arachnida"="Spiders",
                          "Aves"="Birds",          
                          "Chromista"="Chromista",      
                          "Fungi"="Fungi",      
                          "Insecta"="Insects",      
                          "Mammalia"="Mammals",   
                          "Mollusca"="Mollusks",    
                          "Plantae"="Plants",   
                          "Protozoa"="Protozoa",     
                          "Reptilia"="Reptiles"))


# Plot data
################################################################################

# MPA regions
# CA/OR, Alder Creek, Pigeon Point, Point Conception, CA/MEX 
region_lats <- c(39.0, 37.18, 34.5)

# Region labels
region_labels <- tibble(long_dd=c(-123.5, -122.5, -121, -118, -119.2),
                        lat_dd=c(40.5, 38.5, 36, 33.9, 34.6),
                        label=c("North\n(Dec 2012)", "North Central\n(May 2010)", "Central\n(Sep 2007)", "South\n(Jan 2012)", "N. Channel\nIslands (2003)"))

# Theme
theme1 <-  theme(axis.text=element_text(size=7),
                 axis.text.y = element_text(angle = 90, hjust = 0.5),
                 axis.title=element_text(size=8),
                 plot.title=element_blank(),
                 legend.text=element_text(size=6),
                 legend.title=element_text(size=8),
                 plot.tag=element_text(size=8),
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
  geom_point(data=stats1, mapping=aes(x=long_dd, y=lat_dd, size=nobservers, fill=nobservations), pch=21, inherit.aes = F) +
  # Labels
  labs(x="", y="", tag="A") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  # Legend
  scale_size_continuous(name="# of observers") +
  scale_fill_gradientn(name="# of observations", colors=RColorBrewer::brewer.pal(9, "Blues")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", order=2), size=guide_legend(order=1)) +
  # Crop
  coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + theme1 +
  theme(axis.title.y=element_blank(),
        legend.position = c(0.8, 0.7),
        legend.key.size = unit(0.4, "cm"))
g1

# Plot data
g2 <- ggplot(stats_ts, aes(x=year_obs, y=nobservations/1e3, fill=taxa_catg)) +
  geom_bar(stat="identity", lwd=0.1, color="grey30") +
  # Labels
  labs(x="Year", y="Thousands of observations", tag="B") +
  # Axes
  scale_x_continuous(lim=c(2000,2021), breaks=seq(2000, 2020, 5)) +
  # Legend
  scale_fill_discrete(name="Taxa") +
  # Theme
  theme_bw() + theme1 +
  theme(legend.position = c(0.2, 0.6),
        legend.key.size = unit(0.25, "cm"))
g2

# Plot
g3 <- ggplot() +
  lims(x=c(0,10), y=c(0,10)) +
  # Labels
  labs(x="Something cool", y="Something cool", tag="C") +
  # Theme
  theme_bw() + theme1
g3

# Merge
layout_matrix <- matrix(c(1,2, 
                          1,3), ncol=2, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, g3, layout_matrix=layout_matrix, widths=c(0.52, 0.48))

# Export figure
ggsave(g, filename=file.path(plotdir, "Fig4_inaturalist_data.png"), 
       width=6.5, height=5.25, units="in", dpi=600)


