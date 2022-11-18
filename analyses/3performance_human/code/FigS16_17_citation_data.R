

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
datadir <- file.path(basedir, "citations/processed")
popdir <- file.path(basedir, "census_data/processed")
traitdir <- file.path(basedir, "mpa_traits/processed")
plotdir <- "analyses/3performance_human/figures"
outputdir <- "analyses/3performance_human/output"

# Read data
mpas <- readRDS(file=file.path(traitdir, "CA_mpa_metadata.Rds"))
data_orig <- readRDS(file=file.path(datadir, "2016_2021_citations.Rds"))
pop_orig <- readRDS(file=file.path(popdir, "MPA_population_within_50km.Rds"))
inaturalist_orig <- readRDS(file.path(basedir, "inaturalist/processed", "2000_2020_inaturalist_data_inside_mpas_100m_buffer.Rds"))
mpa_watch <- readRDS("analyses/3performance_human/output/mpa_watch_consumptive_indicators.Rds")

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")


# Survey coverage
################################################################################

# Build data
#####################################

# Build data
coverage <- data_orig %>% 
  # Add MPA meta data
  left_join(mpas %>% select(mpa, type, mlpa)) %>% 
  # MPAs of interest
  filter(mlpa=="MLPA")

# MPA order
mpa_order <- coverage %>% 
  group_by(region, mpa) %>% 
  summarize(ncitations=sum(ncitations)) %>% 
  ungroup() %>% 
  arrange(region, desc(ncitations))


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

# Plot data
g <- ggplot(coverage, aes(x=year, y=mpa %>% factor(., levels=mpa_order$mpa), fill=ncitations)) +
  facet_grid(region~., space="free_y", scale="free_y") +
  geom_tile(color="grey30", lwd=0.05) +
  # Labels
  labs(x="Year", y="") +
  scale_x_continuous(breaks=2012:2021) +
  # Legend
  scale_fill_gradientn(name="# of citations", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + theme1
g  

# Export plot
ggsave(g, filename=file.path(plotdir, "FigS16_citation_coverage.png"), 
       width=6.5, height=7.75, units="in", dpi=600)


# Build data
################################################################################

# Summarize iNat data
# Total # of observers/observations, 2000-2018
range(inaturalist_orig$date_obs)
inaturalist <- inaturalist_orig %>% 
  filter(year_obs <= 2018) %>% 
  # Summarize
  group_by(mpa) %>% 
  summarize(inat_observers_tot=n_distinct(user_id),
            inat_observations_tot=n()) %>% 
  ungroup()

# Build data
data_full <- data_orig %>% 
  # Remove years without citatons (so count of years with works)
  filter(ncitations>0) %>% 
  # Summarize
  group_by(region, mpa) %>% 
  summarize(ncitations=sum(ncitations),
            nyears=n_distinct(year)) %>% 
  ungroup() %>% 
  # Add lat/long and type
  left_join(mpas %>% select(mpa, type, mlpa, lat_dd, long_dd, area_sqkm), by="mpa") %>% 
  mutate(region=factor(region, 
                       levels=c("South Coast", "Central Coast", "North Central Coast", "North Coast") %>% rev())) 

# Export
saveRDS(data_full, file=file.path(outputdir, "citations_indicators.Rds"))

# Add and reduce
data <- data_full %>% 
  # Add population data
  left_join(pop_orig %>% select(name, npeople_50km), by=c("mpa"="name")) %>% 
  # Add iNat data
  left_join(inaturalist %>% select(mpa, inat_observers_tot), by="mpa") %>% 
  # Add MPA watch data
  left_join(mpa_watch %>% select(mpa, psurveys, activity_hr), by="mpa") %>% 
  # Types of interest
  filter(mlpa=="MLPA")

# Stats for manuscript
n_distinct(data$mpa)
sum(data$ncitations)

# Identify MPAs with zero engagement
mpas_zero <- mpas %>% 
  # MLPA MPAs
  filter(mlpa=="MLPA") %>% 
  # MPAs without engagement
  filter(!mpa %in% data$mpa)


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
  geom_point(data=data, mapping=aes(x=long_dd, y=lat_dd, size=ncitations, fill=nyears), pch=21) +
  # Plot zero MPAs
  geom_point(data=mpas_zero, mapping=aes(x=long_dd, y=lat_dd), pch="x", size=2.3) +
  # Labels
  labs(x="", y="", tag="A") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  # Legend
  scale_fill_gradientn(name="# of years with citations", colors=RColorBrewer::brewer.pal(9, "Blues")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  scale_size_continuous(name="# of citations\nissued from 2016-2021") +
  # Crop
  coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + theme1 + 
  theme(axis.title.y=element_blank(),
        legend.position = c(0.75, 0.7),
        legend.key.size = unit(0.4, "cm"))
g1

# Plot correlation with population size
g2 <- ggplot(data, aes(x=npeople_50km/1e6, y=ncitations, fill=type, size=area_sqkm)) +
  # Plot regression
  geom_smooth(formula='y ~ x',
              # aes(x=npeople_50km/1e6, y=inat_observers_tot),
              method=glm, method.args = list(family = 'poisson'),
              color="grey50", fill="grey80", alpha=0.5, show.legend = F) +
  # Plot points
  geom_point(pch=21) +
  # Labels
  labs(x="Human population size\n(millions of people within 50 km)", y="Number of citations", tag="B") +
  # Legend
  scale_fill_discrete(name="Region", guide="none") +
  scale_size_continuous(name="Area (sqkm)", breaks=c(20,40,80), range=c(0.1,5)) +
  # Theme
  theme_bw() + theme1 +
  theme(legend.position = c(0.7, 0.75),
        legend.key.size = unit(0.3, "cm"))
g2

# Fit regression
summary(glm(ncitations ~ npeople_50km, data, family="poisson"))

# Plot correlation with engagement
g3 <- ggplot(data, aes(x=inat_observers_tot, y=ncitations, fill=type)) +
  # Plot regression
  geom_smooth(formula='y ~ x',
              # aes(x=npeople_50km/1e6, y=inat_observers_tot),
              method=glm, method.args = list(family = 'poisson'), 
              color="grey50", fill="grey80", alpha=0.5, show.legend = F) +
  # Plot points
  geom_point(pch=21) +
  # Labels
  labs(x="Human engagement\n(# of iNaturalist obervers, 2000-2018)", y="Number of citations", tag="C") +
  # Legend
  scale_fill_discrete(name="Designation") +
  # Theme
  theme_bw() + theme1 +
  theme(legend.position = c(0.7, 0.75),
        legend.key.size = unit(0.3, "cm"))
g3

# Fit regression
summary(glm(ncitations ~ inat_observers_tot, data, family="poisson"))

# Plot correlation with consumptive activitiees
g4 <- ggplot(data, aes(x=psurveys, y=ncitations, fill=type, size=activity_hr)) +
  # Plot regression
  geom_smooth(formula='y ~ x',
              # aes(x=npeople_50km/1e6, y=inat_observers_tot),
              method=glm, method.args = list(family = 'poisson'), 
              color="grey50", fill="grey80", alpha=0.5, show.legend = F) +
  # Plot points
  geom_point(pch=21) +
  # Labels
  labs(x="Consumptive activities per hour\n(based on MPA Watch surveys))", y="Number of citations", tag="D") +
  # labs(x="Consumptive activity\n(% of MPA Watch surveys))", y="Number of citations", tag="D") +
  # scale_x_continuous(labels=scales::percent) +
  # Legend
  scale_fill_discrete(name="Designation", guide="none") +
  scale_size_continuous(name="Activities per hour", range=c(0.1, 5)) +
  # Theme
  theme_bw() + theme1 +
  theme(legend.position = c(0.7, 0.75),
        legend.key.size = unit(0.3, "cm"))
g4

# Merge data
layout_matrix <- matrix(c(1,2, 
                          1,3,
                          1,4), ncol=2, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, g3, g4, layout_matrix=layout_matrix, widths=c(0.62, 0.38))

# Export
ggsave(g, filename=file.path(plotdir, "FigS17_citation_data.png"), 
       width=6.5, height=6.5, units="in", dpi=600)


