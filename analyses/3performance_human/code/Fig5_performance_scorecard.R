

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


# Read MPA attributes
# (make a clean MPA attributes table later to you don't have to use the GIS file)
mpas_orig <- readRDS(file.path(gisdir, "CA_MPA_polygons.Rds"))
sort(unique(mpas_orig$type))
types_use <- c("SMR", "SMRMA", "SMCA", "SMCA (No-Take)")
mpas <- mpas_orig %>% 
  filter(type %in% types_use)
mpas_df <- mpas %>% 
  sf::st_drop_geometry()

# Read score card ingredients
# (only iNaturalist is ready now)
inaturalist_orig <- readRDS(file.path(datadir, "2000_2020_inaturalist_data_inside_mpas_100m_buffer.Rds"))


# Build data
################################################################################


# Summarize iNaturalist performance
inaturalist <- inaturalist_orig %>% 
  # 2018
  filter(year_obs==2018 & !is.na(taxa_catg)) %>% 
  # Summarize
  group_by(mpa) %>% 
  summarize(nobservers=n_distinct(user_id),
            nobservations=n()) %>% 
  ungroup()
  
# Build data
data <- mpas_df %>% 
  select(region, name) %>% 
  rename(mpa_name=name) %>% 
  # Recode region
  mutate(region=recode_factor(region,
                              "NCCSR"="North Coast",   
                              "NCSR"="North Central Coast",   
                              "SFBSR"="North Central Coast",
                              "CCSR"="Central Coast",
                              "SCSR"="South Coast")) %>% 
  # Add iNaturalist data
  left_join(inaturalist, by=c("mpa_name"="mpa")) %>% 
  # Add othter
  mutate(pop_dens=NA,
         mpa_watch_rec=NA,
         mpa_watch_fish=NA,
         sandy_beach=NA,
         boats_sar=NA,
         fishing_comm=NA,
         fishing_rec=NA) %>% 
  # Gather
  gather(key="metric", value="value", 3:ncol(.)) %>% 
  # Recode metrics (potentially organize by theme later)
  mutate(metric=recode_factor(metric,
                              "nobservers"="iNat observers",
                              "nobservations"="iNat observations",
                              "pop_dens"="Population density",
                              "mpa_watch_rec"="MPA Watch (recreation)",
                              "mpa_watch_fish"="MPA Watch (fishing)",
                              "sandy_beach"="Beach visits",
                              "boats_sar"="Boat activity (SAR)",
                              "fishing_comm"="Commercial fishing",
                              "fishing_rec"="Recreational fishing")) %>% 
  # Scale metrics
  group_by(metric) %>% 
  mutate(value_scaled=value/max(value, na.rm=T)) %>% 
  ungroup()


# Plot data
################################################################################

# Theme
theme1 <- theme(axis.text=element_text(size=5),
                axis.text.y=element_text(size=4.5),
                axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                axis.title=element_blank(),
                legend.text=element_text(size=6),
                legend.title=element_text(size=8),
                strip.text=element_text(size=7),
                plot.title=element_blank(),
                # Gridlines
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black"),
                # Legend
                legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data, aes(x=metric, y=mpa_name, fill=value_scaled)) +
  # Facet
  facet_grid(region~., scales="free_y", space="free_y") +
  # Raster
  geom_tile(lwd=0.1, color="grey60") +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_fill_gradientn("Performance\n(scaled to max value)", colors=RColorBrewer::brewer.pal(9, "Blues"), na.value="white") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + theme1
g

# Export figure
ggsave(g, filename=file.path(plotdir, "Fig5_performance_scorecard.png"), 
       width=4.5, height=7.5, units="in", dpi=600)






