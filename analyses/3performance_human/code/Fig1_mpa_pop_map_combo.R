

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
gisdir <- file.path(basedir, "gis_data/processed")
popdir <- file.path(basedir, "census_data/processed")
plotdir <- "analyses/3performance_human/figures"
tabledir <- "analyses/3performance_human/tables"
outdir <- "analyses/3performance_human/output"

# Read data
state_waters_poly <- readRDS(file.path(gisdir, "CA_state_waters_polygons.Rds"))
state_waters_line <- readRDS(file.path(gisdir, "CA_state_waters_polyline.Rds"))
mpas_orig_sf <- readRDS(file.path(gisdir, "CA_MPA_polygons.Rds"))
mpas_orig <- readRDS(file.path(basedir, "mpa_traits/processed", "CA_mpa_metadata.Rds"))

# Read population raster
pop_ras <- readRDS(file=file.path(popdir, "CA_2010_census_tot_pop_by_block_raster.Rds")) %>% 
  filter(!is.na(people) & people>0)

# Read MPA population data
pop_orig <- readRDS(file.path(basedir, "census_data/processed", "MPA_population_within_50km.Rds"))

# Reduce MPA data
pop <- pop_orig %>% 
  rename(mpa=name) %>% 
  left_join(mpas_orig %>% dplyr::select(mpa, mlpa), by="mpa") %>% 
  filter(mlpa=="MLPA")

# Read city key
cities <- readxl::read_excel(file.path(tabledir, "city_key.xlsx"))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")


# Format data
################################################################################

# MPA types
sort(unique(mpas_orig$type))

# MPA order
# Note: There are no SMPs outside SF Bay
types <- c("SMR", "SMCA (No-Take)", "SMCA",  "SMRMA", 
           "SMCA (SF Bay)", "SMP (SF Bay)", "Special Closure", 
           "FMR", "FMCA")

# Format MPA keys
mpas <- mpas_orig_sf %>% 
  # Add SF types
  mutate(type=case_when(region=="SFBSR" ~ paste(type, "(SF Bay)"),
                        T ~ type)) %>% 
  # Order types
  mutate(type=factor(type, types))
  # Reduce
  # filter(type %in% types_use)

# MPA types
sort(unique(mpas$type))

# Stats for paper
mpas %>% 
  sf::st_drop_geometry() %>% 
  group_by(type) %>% 
  summarize(n=n(),
            area_km2=sum(area_sqkm))


# Plot data
################################################################################

# MPA regions
# CA/OR, Alder Creek, Pigeon Point, Point Conception, CA/MEX 
region_lats <- c(39.0, 37.18, 34.5)

# Region labels
region_labels <- tibble(long_dd=c(-123.9, -122.9, -121.8, -121, -118, -119.5),
                        lat_dd=c(40.5, 38.7, 37.6, 36, 34.1, 34.8),
                        label=c("North\n(Dec 2012)", "North Central\n(May 2010)", "San Francisco Bay\n(prior to 2007)",
                                "Central\n(Sep 2007)", "South\n(Jan 2012)", "Northern Channel\nIslands (2003/2007)"))

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_blank(),
                   plot.tag = element_text(size=10, face="bold"),
                   plot.tag.position = c(0.08, 0.98),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = c(0.8, 0.68), # when color/size legends
                   # legend.position = c(0.8, 0.8), # when only color legend
                   legend.key.size = unit(0.4, "cm"),
                   legend.key = element_rect(fill=alpha('blue', 0)),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Type colors
type_colors <- c(RColorBrewer::brewer.pal(9, "Greens")[5:8] %>% rev(), # 4
                 RColorBrewer::brewer.pal(9, "Oranges")[5:7] %>% rev(), # 3
                 RColorBrewer::brewer.pal(9, "Purples")[6:7] %>% rev()) # 2

# Plot data
g1 <- ggplot() +
  # Plot regions
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot MPAs
  # geom_sf(data=mpas, mapping=aes(fill=type), color=NA) +
  geom_point(data=mpas, mapping=aes(x=long_dd, y=lat_dd, fill=type, size=area_sqkm), 
             pch=21, color="grey30", stroke=0.1) + # size=2.5
  # Plot state waters
  geom_sf(data=state_waters_line, color="grey40", lwd=0.1) +
  # Plot region labels
  geom_text(data=region_labels, mapping=aes(x=long_dd, y=lat_dd, label=label), hjust=0, size=2.3) +
  # Labels
  labs(x="", y="", tag="A") +
  # Legend
  scale_fill_manual(name="Designation", values=type_colors) +
  scale_size_continuous(name="Area (sqkm)") +
  guides(fill = guide_legend(order=1), size = guide_legend(order=2)) +
  # Axes
  scale_y_continuous(breaks=32:42) +
  # Crop
  coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + my_theme
g1

# Plot data
g2 <- ggplot() +
  # Plot regions
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot raster
  geom_tile(data=pop_ras, mapping=aes(x=long_dd, y=lat_dd, fill=people_sqkm)) +
  # Plot MPAs
  geom_point(data=pop, mapping=aes(x=long_dd, y=lat_dd, size=npeople_50km/1e6), 
             pch=21, fill="grey50", color="black", alpha=0.4) +
  # Plot city labels
  geom_text(data=cities, aes(x=long, y=lat, label=city, hjust=hjust), size=2.5) +
  # Labels
  labs(x="", y="", tag="B") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  scale_size_continuous(name="Millions of people\nwithin 50 km") +
  scale_fill_gradientn(name="Population density\n(people/sqkm)", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                       trans="log10", na.value = NA, 
                       breaks=c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000), 
                       labels=c("0.0001", "0.001", "0.01", "0.1", "1", "10", "100", "1000", "10,000")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Crop
  coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + my_theme
#g2

# Merge
g <- gridExtra::grid.arrange(g1, g2,nrow=1)


# Export figure
ggsave(g, filename=file.path(plotdir, "Fig1_mpa_pop_map_combo.png"), 
       width=6.5, height=5, units="in", dpi=600)


