
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data/"
reefdir <- file.path(basedir, "reef/processed")
traitdir <- file.path(basedir, "mpa_traits/processed")
plotdir <- "analyses/3performance_human/figures"

# Read MPA metadata
mpas <- readRDS(file.path(traitdir, "CA_MPA_metadata.Rds"))

# Read data
data <- readRDS(file=file.path(reefdir, "REEF_1994_2022_survey_metadata.Rds"))


# Build data
################################################################################

# Types use
types_use <- c("SMR", "SMRMA", "SMCA", "SMCA (No-Take)")

# Build MPA data
range(data$date)
mpa_key <- data %>% 
  # MPA site only
  filter(!is.na(mpa)) %>% 
  # Add year
  mutate(year=lubridate::year(date)) %>% 
  # Number of surveys within site
  group_by(mpa) %>% 
  summarize(nsurveys=n_distinct(survey_id),
            nyrs=n_distinct(year)) %>% 
  ungroup() %>% 
  # Add MPA types and coordinates
  left_join(mpas) %>% 
  # Reduce to types of interest
  filter(type %in% types_use)

# Time series by habitat type
habitat_ts <- data %>% 
  # MPA site only
  filter(!is.na(mpa)) %>% 
  # Add year
  mutate(year=lubridate::year(date)) %>% 
  # Summarize
  group_by(year, habitat) %>% 
  summarize(nsurveys=n_distinct(survey_id)) %>% 
  ungroup()

# Time series by surveyor type
surveyor_ts <- data %>% 
  # MPA site only
  filter(!is.na(mpa)) %>% 
  # Add year
  mutate(year=lubridate::year(date)) %>% 
  # Summarize
  group_by(year, surveyor_type) %>% 
  summarize(nsurveys=n_distinct(survey_id)) %>% 
  ungroup()

# Time series by depth
depth_ts <- data %>% 
  # MPA site only
  filter(!is.na(mpa)) %>% 
  # Add year
  mutate(year=lubridate::year(date)) %>% 
  # Summarize
  group_by(year, max_depth) %>% 
  summarize(nsurveys=n_distinct(survey_id)) %>% 
  ungroup()


# Plot data
################################################################################

# MPA regions
# CA/OR, Alder Creek, Pigeon Point, Point Conception, CA/MEX 
region_lats <- c(39.0, 37.18, 34.5)

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

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
  # Plot REEF sites
  geom_point(data=mpa_key, mapping=aes(x=long_dd, y=lat_dd, size=nsurveys, fill=nyrs), pch=21) +
  # Labels
  labs(x="", y="", tag="A") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  # Legend
  scale_size_continuous(name="# of surveys", trans="log10") +
  # scale_fill_discrete(name="Site type") +
  scale_fill_gradientn(name="# of years", colors=RColorBrewer::brewer.pal(9, "Blues")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Crop
  coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + theme1 +
  theme(axis.title.y=element_blank(),
        legend.position = c(0.8, 0.7),
        legend.key.size = unit(0.4, "cm"))
g1

# Plot data
g2 <- ggplot(habitat_ts, mapping=aes(x=year, y=nsurveys, fill=habitat)) +
  geom_bar(stat="identity", color="grey30", lwd=0.1) +
  # Labels
  labs(x="Year", y="# of surveys", tag="B") +
  scale_x_continuous(breaks=seq(1995, 2020, 5)) +
  # Legend
  scale_fill_discrete(name="Habitat") +
  # Theme
  theme_bw() + theme1 +
  theme(legend.position=c(0.23, 0.65),
        legend.key.size = unit(0.15, "cm"))
g2

# Plot data
g3 <- ggplot(depth_ts, mapping=aes(x=year, y=nsurveys, fill=max_depth)) +
  geom_bar(stat="identity", color="grey30", lwd=0.1) +
  # Labels
  labs(x="Year", y="# of surveys", tag="C") +
  scale_x_continuous(breaks=seq(1995, 2020, 5)) +
  # Legend
  scale_fill_ordinal(name="Max depth") +
  # Theme
  theme_bw() + theme1 +
  theme(legend.position=c(0.23, 0.65),
        legend.key.size = unit(0.15, "cm"))
g3

# Merge data
layout_matrix <- matrix(c(1,2, 
                          1,3), ncol=2, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, g3, layout_matrix=layout_matrix, widths=c(0.52, 0.48))

# Export
ggsave(g, filename=file.path(plotdir, "Fig5_reef_data.png"), 
       width=6.5, height=5.25, units="in", dpi=600)


