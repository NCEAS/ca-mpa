

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(lubridate)
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
mpadir <- file.path(basedir, "mpa_traits/processed")
datadir <- file.path(basedir, "ebird/processed")
plotdir <- "analyses/3performance_human/figures"

# Read data
data_orig <- readRDS(file.path(datadir, "CA_ebird_data_inside_mpas_100m_buffer.Rds"))

# Read MPA data
mpas_orig <- readRDS(file.path(mpadir, "CA_mpa_metadata.Rds"))

# MPA types
types_use <- c("SMR", "SMRMA", "SMCA", "SMCA (No-Take)")

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")


# Coverage
################################################################################

# Coverage
coverage <- data_orig %>% 
  # Add MPA metadata
  left_join(mpas_orig %>% select(region, type, mpa)) %>% 
  # Correct region
  mutate(region=recode(region, "San Francisco Bay"="North Central Coast")) %>% 
  # Reduce to MPAs of interest
  filter(type %in% types_use) %>% 
  # Add year, month, dummy date
  mutate(year=lubridate::year(survey_date),
         month=lubridate::month(survey_date),
         date_dummy=lubridate::ymd(paste(year, month, 1, sep="-"))) %>% 
  # Summarize
  group_by(region, mpa, date_dummy) %>% 
  summarize(nobservers=n_distinct(observer_id),
            nobservations=n(),
            nsurveys=n_distinct(survey_id)) %>% 
  ungroup()

# MPA order
mpa_order <- coverage %>% 
  # Order
  group_by(region, mpa) %>% 
  summarize(nobservations_tot=sum(nobservations)) %>% 
  ungroup() %>% 
  arrange(region, desc(nobservations_tot))

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

# Plot  coverage
g <- ggplot(coverage, aes(x=date_dummy, 
                          y=mpa %>% factor(., levels=mpa_order$mpa), fill=nsurveys)) +
  facet_grid(region~., space="free_y", scale="free_y") +
  geom_tile(color="grey30", lwd=0.05) +
  # Axes
  scale_x_date(lim=c("1950-01-01", NA) %>% ymd(),
               breaks = seq(ymd("1950-01-01"), ymd("2020-01-01"), by="5 years"), 
               date_labels="%Y") +
  # Labels
  labs(x="Month", y="") +
  # Legend
  scale_fill_gradientn(name="# of surveys", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(), trans="log2") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + theme1
g  

# Export plot
ggsave(g, filename=file.path(plotdir, "FigS3_ebird_obs_coverage.png"), 
       width=6.5, height=7.5, units="in", dpi=600)



# Build data
################################################################################

# Stats for manuscript
data_orig %>% 
  # Add MPA meta-data
  left_join(mpas_orig %>% select(mpa, type, region), by="mpa") %>% 
  # Reduce to MPAs of interest
  filter(type %in% types_use) %>% 
  # Group by
  group_by() %>% 
  summarize(n=n(),
            n_surveys=n_distinct(survey_id),
            n_people=n_distinct(observer_id),
            n_mpas=n_distinct(mpa))

# MPA stats
mpa_stats <- data_orig %>%
  # Summarize
  group_by(mpa) %>%
  summarize(n_observers=n_distinct(observer_id),
            n_surveys=n_distinct(survey_id),
            n_observations=n(),
            n_species=n_distinct(taxon_concept_id)) %>%
  ungroup() %>% 
  # Add MPA meta-data
  left_join(mpas_orig %>% select(mpa, type, region, lat_dd, long_dd), by="mpa") %>% 
  # Reduce to MPAs of interest
  filter(type %in% types_use)

# Observer time series
observer_ts <- data_orig %>% 
  # Add MPA meta-data
  left_join(mpas_orig %>% select(mpa, type, region), by="mpa") %>% 
  # Reduce to MPAs of interest
  filter(type %in% types_use) %>% 
  # Add year
  mutate(year=lubridate::year(survey_date)) %>% 
  # Update region
  mutate(region=recode(region, 
                       "San Francisco Bay"="North Central Coast")) %>% 
  # Summarize
  group_by(region, year) %>% 
  summarize(n_observers=n_distinct(observer_id)) %>% 
  ungroup()

# MPA time series
mpa_ts <- data_orig %>% 
  # Add MPA meta-data
  left_join(mpas_orig %>% select(mpa, type, region), by="mpa") %>% 
  # Reduce to MPAs of interest
  filter(type %in% types_use) %>% 
  # Add year
  mutate(year=lubridate::year(survey_date)) %>% 
  # Update region
  mutate(region=recode(region, 
                       "San Francisco Bay"="North Central Coast")) %>% 
  # Summarize
  group_by(region, year) %>% 
  summarize(n_mpas=n_distinct(mpa)) %>% 
  ungroup()

# Plot data
################################################################################

# MPA regions
# CA/OR, Alder Creek, Pigeon Point, Point Conception, CA/MEX 
region_lats <- c(39.0, 37.18, 34.5)

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
  geom_point(data=mpa_stats, 
             mapping=aes(x=long_dd, y=lat_dd, size=n_observers, fill=n_surveys), 
             pch=21, inherit.aes = F) +
  # Labels
  labs(x="", y="", tag="A") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  # Legend
  scale_size_continuous(name="# of observers") +
  scale_fill_gradientn(name="# of surveys", colors=RColorBrewer::brewer.pal(9, "Blues")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", order=2), size=guide_legend(order=1)) +
  # Crop
  coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + theme1 +
  theme(axis.title.y=element_blank(),
        legend.position = c(0.8, 0.7),
        legend.key.size = unit(0.4, "cm"))
g1

# Plot number of observers by region
g2 <- ggplot(observer_ts, aes(x=year, y=n_observers, fill=region)) +
  geom_bar(stat="identity", lwd=0.1, color="grey30") +
  # Labels
  labs(x="Year", y="# of observers", tag="B") +
  # Axes
  scale_x_continuous(breaks=seq(1960, 2020, 10), limits = c(1960, 2021)) +
  # Legend
  scale_fill_ordinal(name="Region") +
  # Theme
  theme_bw() + theme1 +
  theme(legend.position = c(0.25, 0.75),
        legend.key.size = unit(0.25, "cm"))
g2

# Plot number of MPAs by region
g3 <- ggplot(mpa_ts, aes(x=year, y=n_mpas, fill=region)) +
  geom_bar(stat="identity", lwd=0.1, color="grey30") +
  # Reference line
  geom_hline(yintercept=125, linetype="dotted", color="grey30") +
  # Labels
  labs(x="Year", y="# of MPAs surveyed", tag="C") +
  # Axes
  scale_x_continuous(breaks=seq(1960, 2020, 10), limits = c(1960, 2021)) +
  scale_y_continuous(breaks=seq(0,120, 20)) +
  # Legend
  scale_fill_ordinal(name="") +
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
ggsave(g, filename=file.path(plotdir, "Fig6_ebird_data.png"), 
       width=6.5, height=5.25, units="in", dpi=600)


