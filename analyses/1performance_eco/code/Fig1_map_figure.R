

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
gisdir <- file.path(basedir, "gis_data/processed")
plotdir <- "analyses/1performance_eco/figures"

# Read site locations
# sites <- readRDS(file.path(basedir, "monitoring/monitoring_sites_clean.Rds"))
load(file.path(basedir, "monitoring/site_locations.Rda"))

# Read data
state_waters_poly <- readRDS(file.path(gisdir, "CA_state_waters_polygons.Rds"))
state_waters_line <- readRDS(file.path(gisdir, "CA_state_waters_polyline.Rds"))
mpas_orig <- readRDS(file.path(basedir, "mpa_traits/processed", "CA_mpa_metadata.Rds"))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")


# Temporary
################################################################################

# Format sites
sites <- site_locations %>% 
  # Rename
  rename(long_dd=lon, lat_dd=lat, habitat=group, site_type=mpa_designation,
         mpa=affiliated_mpa, mpa_type=mpa_class) %>% 
  # Format lat/long
  mutate(long_dd=as.numeric(long_dd)) %>% 
  # Format habitat
  mutate(habitat=recode_factor(habitat, 
                               "surf-zone"="Surf zone",
                               "rocky"="Rocky intertidal",
                               "kelp"="Kelp",
                               "ccfrp"="Rocky reef",
                               "deep_reef"="Deep reef")) %>% 
  # Format site type
  mutate(site_type=ifelse(site_type=="ref", "Reference", "MPA")) %>% 
  # Format MPA
  mutate(mpa_type=toupper(mpa_type),
         mpa=stringr::str_to_title(mpa),
         mpa=mpa %>% gsub("Smr", "SMR", .) %>% gsub("Smca", "SMCA", .),
         mpa=recode(mpa, 
                    "Ano Nuevo SMR"="Año Nuevo SMR",
                    "Arrow Point To Lion Head Point SMCA"="Arrow Point to Lion Head Point SMCA",
                    "Blue Cavern Onshore SMCA"="Blue Cavern Offshore SMCA",           
                    "Bodega Head "="Bodega Head SMR/SMCA",                     
                    "Campus Point SMCA"="Campus Point SMCA (No-Take)",                
                    "Cape Mendocino SMCA"="South Cape Mendocino SMR",              
                    "Laguna Beach SMCA"="Laguna Beach SMCA (No-Take)",                
                    "Mackerricher SMCA"="MacKerricher SMCA",              
                    "N Farallon Islands SMR"="North Farallon Islands SMR",              
                    "N/A"="None",         
                    "None"="None",                             
                    "Piedras Blancas SMCA/ Smcr"="Piedras Blancas SMR/SMCA",     
                    "Point Arena "="Point Arena SMR/SMCA",      
                    "Point Vicente SMCA"="Point Vicente SMCA (No-Take)",                 
                    "Soquel Canyon SMR"="Soquel Canyon SMCA",              
                    "Swamis SMCA"="Swami's SMCA",           
                    "Trinidad SMR"="None")) 

# Check names
sites_in_data <- sort(unique(sites$mpa))
sites_in_data[!sites_in_data %in% mpas_orig$mpa]


# Summarize
################################################################################

# Build data
stats <- sites %>% 
  # Unique
  count(mpa, habitat) %>% 
  # Order habiat
  mutate(habitat=recode_factor(habitat,
                               "Surf zone"="Surf\nzone", 
                               "Rocky intertidal"="Rocky\nintertidal",
                               "Kelp"="Kelp\nforest",
                               "Rocky reef"="Rocky\nreef",
                               "Deep reef"="Deep\nreef")) %>%
  # Reduce
  filter(!mpa %in% c("None")) %>% 
  # Add region
  left_join(mpas %>% select(mpa, region), by="mpa") %>% 
  # Fix regions
  mutate(region=as.character(region),
         region=ifelse(grepl("Bodega|Arena", mpa), "North Central Coast", region),
         region=ifelse(grepl("Piedras", mpa), "Central Coast", region),
         region=ifelse(grepl("Anacapa", mpa), "South Coast", region),
         region=factor(region, levels=levels(mpas$region)))

# MPA stats
stats_mpa <- stats %>% 
  count(mpa)

# MPAs
mpas <- mpas_orig %>% 
  # Reduce to MPAs of interest
  filter(mlpa=="MLPA") %>% 
  # Add stats
  left_join(stats_mpa)

# Subset
mpas_zero <- mpas %>% filter(is.na(n))
mpas_data <- mpas %>% filter(!is.na(n))


# Plot data
################################################################################

# MPA regions
# CA/OR, Alder Creek, Pigeon Point, Point Conception, CA/MEX 
region_lats <- c(39.0, 37.18, 34.5)

# Region labels
region_labels <- tibble(long_dd=c(-123.9, -122.9, -121, -118, -119.5),
                        lat_dd=c(40.5, 38.7, 36, 34.1, 34.8),
                        label=c("North\n(Dec 2012)", "North Central\n(May 2010)", "Central\n(Sep 2007)", "South\n(Jan 2012)", "N. Channel\nIslands (2003)"))

# Theme
base_theme <-  theme(axis.text=element_text(size=7),
                     axis.title=element_text(size=8),
                     legend.text=element_text(size=7),
                     legend.title=element_text(size=8),
                     plot.tag=element_text(size=8),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.key = element_rect(fill=alpha('blue', 0)),
                     legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot() +
  # Plot regions
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot state waters
  geom_sf(data=state_waters_line, color="grey40", lwd=0.1) +
  # Plot MPAs
  geom_point(data=mpas_data, mapping=aes(x=long_dd, y=lat_dd, size=n), color="grey50") +
  geom_point(data=mpas_zero, mapping=aes(x=long_dd, y=lat_dd), shape="x", size=3) +
  # Plot region labels
  geom_text(data=region_labels, mapping=aes(x=long_dd, y=lat_dd, label=label), hjust=0, size=2.3) +
  # Labels
  labs(x="", y="", tag="A") +
  scale_size_continuous(name="# of habitats monitored") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  # Crop
  coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title=element_blank(),
        legend.position = c(0.8, 0.7),
        legend.key.size = unit(0.4, "cm"))
g1

# Plot habitat availabiliy
g2 <- ggplot(stats, aes(x=habitat, y=mpa, fill=habitat)) +
  facet_grid(region~., space="free_y", scales="free_y") +
  geom_raster() +
  # Labels
  labs(y="MPA", x="Habitat", tag="B") +
  # Legend
  scale_fill_discrete(guide="none") +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y=element_blank(),
        axis.title.x=element_blank())
g2

# Merge figures
g <- gridExtra::grid.arrange(g1, g2, ncol=2, widths=c(0.65, 0.35))
g

# Export figure
ggsave(g, filename=file.path(plotdir, "Fig1_map_figure.png"), 
       width=6.5, height=6.5, units="in", dpi=600)


