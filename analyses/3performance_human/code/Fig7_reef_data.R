
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(lubridate)
library(tidyverse)
library(countrycode)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data/"
reefdir <- file.path(basedir, "reef/processed")
traitdir <- file.path(basedir, "mpa_traits/processed")
plotdir <- "analyses/3performance_human/figures"
outputdir <- "analyses/3performance_human/output"

# Read MPA metadata
mpas <- readRDS(file.path(traitdir, "CA_MPA_metadata.Rds"))

# Read data
data <- readRDS(file=file.path(reefdir, "REEF_1994_2022_survey_metadata.Rds"))



# Survey coverage
################################################################################

# Build data
#####################################

# Build data
reef_coverage <- data %>% 
  # Reduce to MPAs
  filter(!is.na(mpa)) %>% 
  # Add MPA meta data
  left_join(mpas %>% select(mpa, region, type, mlpa)) %>% 
  # MPAs of interest
  filter(mlpa == "MLPA") %>% 
  # Add year, month, dummy date
  mutate(year=lubridate::year(date),
         month=lubridate::month(date),
         date_dummy=lubridate::ymd(paste(year, month, 1, sep="-"))) %>% 
  # Summarize
  group_by(region, mpa, date_dummy) %>% 
  summarize(nsurveys=n_distinct(survey_id)) %>% 
  ungroup() %>% 
  # Format regions
  mutate(region=recode_factor(region,
                       "North Coast"="North\nCoast",
                       "North Central Coast"="North\nCentral\nCoast",
                       "San Francisco Bay"="North\nCentral\nCoast",
                       "Central Coast"="Central\nCoast",
                       "South Coast"="South\nCoast"))

# MPA order
mpa_order <- reef_coverage %>% 
  group_by(region, mpa) %>% 
  summarize(nsurveys=sum(nsurveys)) %>% 
  ungroup() %>% 
  arrange(region, desc(nsurveys))


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
g <- ggplot(reef_coverage, aes(x=date_dummy, y=mpa %>% factor(., levels=mpa_order$mpa), fill=nsurveys)) +
  facet_grid(region~., space="free_y", scale="free_y") +
  geom_tile(color="grey30", lwd=0.05) +
  # Labels
  labs(x="Month", y="") +
  scale_x_date(breaks=seq(ymd("1995-01-01"), ymd("2020-01-01"), by="5 year") %>% ymd(), 
               date_labels="%Y") +
  # Legend
  scale_fill_gradientn(name="# of surveys", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(), trans="log2") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + theme1
g  

# Export plot
ggsave(g, filename=file.path(plotdir, "FigS4_reef_survery_coverage.png"), 
       width=6.5, height=5, units="in", dpi=600)


# Build data
################################################################################

# Build MPA data
range(data$date)
stats_full <- data %>% 
  # MPA site only
  filter(!is.na(mpa)) %>% 
  # Filter to date range
  filter(date>=ymd("2012-01-01") & date<=ymd("2021-12-31")) %>% 
  # Add year
  mutate(year=lubridate::year(date)) %>% 
  # Number of surveys within site
  group_by(mpa) %>% 
  summarize(nsurveys=n_distinct(survey_id),
            nyrs=n_distinct(year)) %>% 
  ungroup() %>% 
  # Add MPA types and coordinates
  left_join(mpas)

# Export
saveRDS(stats_full, file=file.path(outputdir, "reef_indicators.Rds"))
  
# Reduce to MPAs of interest
stats <- stats_full %>% 
  # Reduce to types of interest
  filter(mlpa=="MLPA")

# Time series by habitat type
habitat_ts <- data %>% 
  # MPA site only
  filter(!is.na(mpa)) %>% 
  # Add MPA meta data
  left_join(mpas %>% select(mpa, region, type, mlpa)) %>% 
  # MPAs of interest
  filter(mlpa == "MLPA") %>% 
  # Add year
  mutate(year=lubridate::year(date)) %>% 
  # Summarize
  group_by(year, habitat) %>% 
  summarize(nsurveys=n_distinct(survey_id)) %>% 
  ungroup() %>% 
  # Recode habitats
  mutate(habitat=recode(habitat,
                        "Sandy bottom"="Sand",
                        "Mud/silt bottom"="Mud/silt",
                        "Cobblestone/boulder field"="Cobble/boulder"),
         habitat=factor(habitat,
                        levels=c("Open ocean", "Sand", "Mud/silt",
                                 "Artificial reef", "Rocky reef", "Cobble/boulder", "Pinnacle", "Wall",
                                 "Eel grass", "Surf grass", "Bull kelp", "Kelp forest", "Mixed")))

# Habitat colors
hab_colors <- c("steelblue", "wheat1", "saddlebrown",
                RColorBrewer::brewer.pal(5, "Purples"), 
                RColorBrewer::brewer.pal(4, "YlGn"),
                "darkorange")

# Time series by surveyor type
surveyor_ts <- data %>% 
  # MPA site only
  filter(!is.na(mpa)) %>%
  # Add MPA meta data
  left_join(mpas %>% select(mpa, region, type, mlpa)) %>% 
  # MPAs of interest
  filter(mlpa == "MLPA") %>% 
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
  # Add MPA meta data
  left_join(mpas %>% select(mpa, region, type, mlpa)) %>% 
  # MPAs of interest
  filter(mlpa == "MLPA") %>% 
  # Add year
  mutate(year=lubridate::year(date)) %>% 
  # Summarize
  group_by(year, max_depth) %>% 
  summarize(nsurveys=n_distinct(survey_id)) %>% 
  ungroup()

# Stats for paper
nrow(data) # number of surveys
sum(stats$nsurveys) # number in protected areas
sum(stats$nsurveys) / nrow(data) *100

# Identify MPAs with zero engagement
mpas_zero <- mpas %>% 
  # MLPA MPAs
  filter(mlpa=="MLPA") %>% 
  # MPAs without engagement
  filter(!mpa %in% stats$mpa)

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
  geom_point(data=stats, mapping=aes(x=long_dd, y=lat_dd, size=nsurveys, fill=nyrs), pch=21) +
  # Plot zero MPAs
  geom_point(data=mpas_zero, mapping=aes(x=long_dd, y=lat_dd), pch="x", size=2.3) +
  # Labels
  labs(x="", y="", tag="A") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  # Legend
  scale_size_continuous(name="# of surveys", trans="log10") +
  # scale_fill_discrete(name="Site type") +
  scale_fill_gradientn(name="# of years", colors=RColorBrewer::brewer.pal(9, "Blues"), breaks=seq(0,10,2)) +
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
  # Reference line
  geom_vline(xintercept = 2011.5, linetype="dotted", color="grey30") +
  # Labels
  labs(x="Year", y="# of surveys", tag="B") +
  scale_x_continuous(breaks=seq(1995, 2020, 5)) +
  # Legend
  scale_fill_manual(name="Habitat", na.value="grey90", values=hab_colors) +
  # Theme
  theme_bw() + theme1 +
  theme(legend.position=c(0.16, 0.68),
        legend.key.size = unit(0.15, "cm"))
g2

# Plot data
g3 <- ggplot(depth_ts, mapping=aes(x=year, y=nsurveys, fill=max_depth)) +
  geom_bar(stat="identity", color="grey30", lwd=0.1) +
  # Reference line
  geom_vline(xintercept = 2011.5, linetype="dotted", color="grey30") +
  # Labels
  labs(x="Year", y="# of surveys", tag="C") +
  scale_x_continuous(breaks=seq(1995, 2020, 5)) +
  # Legend
  scale_fill_ordinal(name="Max depth", na.value="grey90", direction=-1) +
  # Theme
  theme_bw() + theme1 +
  theme(legend.position=c(0.14, 0.62),
        legend.key.size = unit(0.15, "cm"))
g3

# Merge data
layout_matrix <- matrix(c(1,2, 
                          1,3), ncol=2, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, g3, layout_matrix=layout_matrix, widths=c(0.52, 0.48))

# Export
ggsave(g, filename=file.path(plotdir, "Fig7_reef_data.png"), 
       width=6.5, height=5.25, units="in", dpi=600)


