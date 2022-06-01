

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
gisdir <- file.path(basedir, "gis_data/processed")
datadir <- file.path(basedir, "mpa_watch/processed")
plotdir <- "analyses/3performance_human/figures"

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Read MPA data
mpas_orig <- readRDS(file.path(basedir, "mpa_traits/processed", "CA_mpa_metadata.Rds"))
types_use <- c("SMR", "SMRMA", "SMCA", "SMCA (No-Take)")
mpas <- mpas_orig %>% 
  filter(type %in% types_use)

# To do list
# Cull based on start time and end time


# Build data
################################################################################

# Build data
data_orig <- readRDS(file.path(datadir, "MPA_Watch_2011_2022_surveys_ca_programs_wide.Rds"))
col_key <- readxl::read_excel(file.path(datadir, "column_key_ca_programs.xlsx"))

# Build data
data <- data_orig %>% 
  # Simplify
  select(survey_id, mpa, mpa_id, survey_type, date, duration_hr, total_activities:comments) %>% 
  # Remove invalid surveys
  filter(duration_hr > 0) %>% 
  # Gather
  gather(key="use", value="n_obs", 7:ncol(.)) %>% 
  # Left join
  left_join(col_key) %>% 
  # Reduce to data of interest
  filter(type!="Total" & consumptive %in% c("Consumptive", "Non-consumptive") & !grepl("estimate?", use)) %>% 
  # Convert number of observations to numeric
  mutate(n_obs=as.numeric(n_obs)) %>% 
  # Reduce
  filter(!is.na(n_obs) & n_obs>0)

# Examine survey coverage
################################################################################

# Examine survey coverage
survey_coverage <- data %>% 
  # Number of activies on each survey by location/type
  select(survey_id, mpa, mpa_id, survey_type, date) %>% 
  unique() %>% 
  # Add year, month, dummy date
  mutate(year=lubridate::year(date),
         month=lubridate::month(date),
         date_dummy=lubridate::ymd(paste(year, month, 1, sep="-"))) %>% 
  # Number of surveys
  group_by(year, month, date_dummy, mpa, mpa_id, survey_type) %>% 
  summarize(nsurveys=n_distinct(survey_id)) %>% 
  ungroup()

# Number of MPAs and control sites
n_distinct(data$survey_id)
n_distinct(survey_coverage$mpa[survey_coverage$survey_type=="MPA"])
n_distinct(survey_coverage$mpa[survey_coverage$survey_type=="Control"])

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

# Plot survey coverage
g <- ggplot(survey_coverage, aes(x=date_dummy, y=mpa, fill=nsurveys)) +
  facet_grid(survey_type~., space="free_y", scales="free_y") +
  geom_tile(color="grey30", lwd=0.05) +
  # Add vertical lines
  geom_vline(xintercept="")
  # Labels
  labs(x="Month", y="") +
  # Legend
  scale_fill_gradientn(name="# of surveys", colors=RColorBrewer::brewer.pal(9, "Blues"), trans="log2") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + theme1
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigSX_mpa_watch_survey_coverage.png"), 
       width=6.5, height=7, units="in", dpi=600)



# Build data
################################################################################

# Summarize median by MPA
data1 <- data %>% 
  # Number of activities on each survey by location/type
  group_by(survey_id, mpa, mpa_id, survey_type, date, duration_hr, consumptive) %>% 
  summarize(n_activities=sum(n_obs)) %>% 
  ungroup() %>% 
  # Number per hour 
  mutate(activities_per_hour=n_activities/duration_hr) %>% 
  # Calculate median across 
  group_by(mpa, mpa_id, survey_type, consumptive) %>% 
  summarize(nsurveys=n_distinct(survey_id),
            activities_per_hour=median(activities_per_hour)) %>% 
  ungroup() %>% 
  # Add coordinates
  left_join(mpas %>% select(mpa, long_dd, lat_dd)) %>% 
  # Remove control sites
  filter(survey_type=="MPA")

# Summarize trends over time by MPA
data_ts <-  data %>% 
  # Number of activities on each survey by location/type
  group_by(survey_id, mpa, mpa_id, survey_type, date, duration_hr, consumptive) %>% 
  summarize(n_activities=sum(n_obs)) %>% 
  ungroup() %>% 
  # Number per hour 
  mutate(activities_per_hour=n_activities/duration_hr) %>% 
  # Add data dummy
  # Add year, month, dummy date
  mutate(year=lubridate::year(date),
         month=lubridate::month(date),
         date_dummy=lubridate::ymd(paste(year, month, 1, sep="-"))) %>% 
  # Summarize by MPA and date
  group_by(mpa, mpa_id, survey_type, date_dummy, consumptive) %>% 
  summarize(activities_per_hour=median(activities_per_hour)) %>% 
  ungroup()


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
g1 <- ggplot(data=data1, mapping=aes(x=long_dd, y=lat_dd, size=activities_per_hour)) +
  facet_wrap(~consumptive) +
  # Plot regions
  geom_hline(yintercept=region_lats) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3, inherit.aes = F) +
  # Plot MPAs
  geom_point(pch=21) +
  # Labels
  labs(x="", y="") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  # Legend
  scale_size_continuous(name="Activities per hour") +
  # Crop
  coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + theme1 +
  theme(axis.title=element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        legend.position = c(0.9, 0.8))
g1

# Export figure
ggsave(g1, filename=file.path(plotdir, "Fig3_mpa_watch_data.png"), 
       width=6.5, height=5.5, units="in", dpi=600)

