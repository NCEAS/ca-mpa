

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
mpadir <- file.path(basedir, "mpa_traits/processed")
datadir <- file.path(basedir, "inaturalist/processed")
plotdir <- "analyses/3performance_human/figures"
outputdir <- "analyses/3performance_human/output"

# Read data
mpas_orig <- readRDS(file.path(mpadir, "CA_mpa_metadata.Rds"))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")


# Build data
################################################################################

# Build data
data_orig <- readRDS(file.path(datadir, "2000_2021_inaturalist_data_inside_mpas_100m_buffer.Rds"))

# Stats
nrow(data_orig)
n_distinct(data_orig$user_id)

# Summarize
stats_full <- data_orig %>% 
  # 2012-2021
  filter(date_obs>=lubridate::ymd("2012-01-01") & date_obs<=lubridate::ymd("2021-12-31")) %>% 
  # Summarize
  group_by(mpa) %>% 
  summarize(nobservers=n_distinct(user_id),
            nobservations=n(),
            nspecies=n_distinct(sci_name)) %>% 
  ungroup()

# Export data
saveRDS(stats_full, file=file.path(outputdir, "inaturalist_indicators.Rds"))

# Reduce and spatialize
stats <- stats_full %>% 
  # Reduce to MPAs of interest
  left_join(mpas_orig %>% select(mpa, type, mlpa, lat_dd, long_dd)) %>% 
  filter(mlpa=="MLPA")
  
# Number of MPAs with observations
stats %>% filter(!is.na(nobservers)) %>% pull(mpa) %>% n_distinct(.)

# Time series stats
observations_ts <- data_orig %>%
  # Reduce to MPAs of interest
  left_join(mpas_orig %>% select(mpa, type, mlpa, lat_dd, long_dd)) %>% 
  filter(mlpa=="MLPA") %>% 
  # Summarize
  mutate(taxa_catg=ifelse(is.na(taxa_catg), "Animalia", taxa_catg)) %>% 
  group_by(year_obs, taxa_catg) %>% 
  summarize(nobservations=n()) %>% 
  ungroup() %>% 
  # Rename taxa
  mutate(taxa_catg=recode_factor(taxa_catg,
                                 "Plantae"="Plants",
                                "Mammalia"="Mammals",   
                                "Aves"="Birds", 
                                "Actinopterygii"="Fish",
                                "Mollusca"="Mollusks",    
                                "Amphibia"="Amphibians",  
                                "Reptilia"="Reptiles",
                                "Insecta"="Insects",
                                "Arachnida"="Spiders",
                                "Fungi"="Fungi",      
                                "Protozoa"="Protozoa", 
                                "Chromista"="Chromista",
                                "Animalia"="Other"))

# Observer time series
observer_ts <- data_orig %>% 
  # Reduce to MPAs of interest
  left_join(mpas_orig %>% select(mpa, type, mlpa, lat_dd, long_dd)) %>% 
  filter(mlpa=="MLPA") %>% 
  # Summarize
  group_by(year_obs) %>% 
  summarize(nobservers=n_distinct(user_id)) %>% 
  ungroup()

# Observer time series
observer_ts1 <- data_orig %>% 
  # Reduce to MPAs of interest
  left_join(mpas_orig %>% select(mpa, type, mlpa, lat_dd, long_dd)) %>% 
  filter(mlpa=="MLPA") %>% 
  # Summarize
  group_by(year_obs, user_id) %>%
  summarize(nmpas=n_distinct(mpa),
            nmpas_catg=cut(nmpas, breaks=c(0,1,3,5,10,21), labels=c("1", "2-3", "4-5", "6-10", "11-21"))) %>% 
  ungroup() %>% 
  # Summarize again
  group_by(year_obs, nmpas_catg) %>% 
  summarize(nobservers=n_distinct(user_id)) %>% 
  ungroup()


# iNaturalist coverage
################################################################################

# Build data
#####################################

# iNat coverage by month
inat_coverage <- data_orig %>% 
  # Add MPA metadata
  left_join(mpas_orig %>% select(region, type, mlpa, mpa)) %>% 
  # Reduce to MPAs of interest
  filter(mlpa == "MLPA") %>% 
  # Add year, month, dummy date
  mutate(year=lubridate::year(date_obs),
         month=lubridate::month(date_obs),
         date_dummy=lubridate::ymd(paste(year, month, 1, sep="-"))) %>% 
  # Reduce to data before 2018
  # filter(year<=2018) %>% 
  # Summarize
  group_by(region, mpa, date_dummy) %>% 
  summarize(nobservers=n_distinct(user_id),
            nobservations=n()) %>% 
  ungroup()

# MPA order
mpa_order <- inat_coverage %>% 
  # Order
  group_by(region, mpa) %>% 
  summarize(nobservations_tot=sum(nobservations)) %>% 
  ungroup() %>% 
  arrange(region, desc(nobservations_tot))

# Identify MPAs with zero engagement
mpas_zero <- mpas_orig %>% 
  # MLPA MPAs
  filter(mlpa=="MLPA") %>% 
  # MPAs without engagement
  filter(!mpa %in% stats$mpa)

# Plot data
#####################################

# Theme
theme1 <-  theme(axis.text=element_text(size=6),
                 axis.text.y=element_text(size=5),
                 axis.title=element_text(size=8),
                 axis.title.y=element_blank(),
                 legend.text=element_text(size=6),
                 legend.title=element_text(size=7),
                 strip.text=element_text(size=7),
                 # Gridlines
                 panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(), 
                 axis.line = element_line(colour = "black"),
                 # Legend
                 legend.background = element_rect(fill=alpha('blue', 0)))

# Plot iNat coverage
g <- ggplot(inat_coverage, aes(x=date_dummy, y=mpa %>% factor(., levels=mpa_order$mpa), fill=nobservations)) +
  facet_grid(region~., space="free_y", scale="free_y") +
  geom_tile(color="grey30", lwd=0.05) +
  # Labels
  labs(x="Month", y="") +
  # Legend
  scale_fill_gradientn(name="# of observations", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(), trans="log2") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + theme1
g  

# Export plot
ggsave(g, filename=file.path(plotdir, "FigS5_inat_obs_coverage.png"), 
       width=6.5, height=7.5, units="in", dpi=600)


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
                 plot.tag=element_text(size=9),
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
  geom_point(data=stats, mapping=aes(x=long_dd, y=lat_dd, size=nobservers, fill=nobservations), pch=21, inherit.aes = F) +
  # Plot zero MPAs
  geom_point(data=mpas_zero, mapping=aes(x=long_dd, y=lat_dd), pch="x", size=2.3) +
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

# Plot number of observations
taxa_colors <- c("green4", "saddlebrown", "thistle", "lightsteelblue2", "wheat2", 
                 "olivedrab3", "green3", "gold1", "grey60", "firebrick2", "darkorange", "lavenderblush", "grey90")
g2 <- ggplot(observations_ts, aes(x=year_obs, y=nobservations/1e3, fill=taxa_catg)) +
  geom_bar(stat="identity", lwd=0.1, color="grey30") +
  # Reference line
  geom_vline(xintercept = 2011.5, linetype="dotted", color="grey30") +
  # Labels
  labs(x="Year", y="Thousands of observations", tag="B") +
  # Axes
  scale_x_continuous(breaks=seq(2000, 2020, 5)) +
  # Legend
  scale_fill_manual(name="Taxa", values=taxa_colors) +
  # Theme
  theme_bw() + theme1 +
  theme(legend.position = c(0.2, 0.6),
        legend.key.size = unit(0.25, "cm"))
g2

# Plot number of observers
g3 <- ggplot(observer_ts1, aes(x=year_obs, y=nobservers, fill=nmpas_catg)) +
  geom_bar(stat="identity", lwd=0.1, color="grey30", position = position_stack(reverse = TRUE)) +
  # Reference line
  geom_vline(xintercept = 2011.5, linetype="dotted", color="grey30") +
  # Labels
  labs(x="Year", y="Number of observers", tag="C") +
  # Axes
  scale_x_continuous(breaks=seq(2000, 2020, 5)) +
  # Legend
  scale_fill_ordinal(name="# of MPAs visited", direction=-1, na.value="grey90", guide = guide_legend(reverse = TRUE)) +
  # Theme
  theme_bw() + theme1 + 
  theme(legend.position = c(0.25, 0.75),
        legend.key.size = unit(0.25, "cm"))
g3

# Merge
layout_matrix <- matrix(c(1,2, 
                          1,3), ncol=2, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, g3, layout_matrix=layout_matrix, widths=c(0.52, 0.48))

# Export figure
ggsave(g, filename=file.path(plotdir, "FigS6_inaturalist_data.png"), 
       width=6.5, height=5.25, units="in", dpi=600)


