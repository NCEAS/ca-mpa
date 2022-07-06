

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(lubridate)

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

# Read MPW watch data
data_orig <- readRDS(file.path(datadir, "MPA_Watch_2011_2022_surveys_ca_programs_wide.Rds"))
col_key <- readxl::read_excel(file.path(datadir, "column_key_ca_programs.xlsx"))

# MPA types
types_use <- c("SMR", "SMRMA", "SMCA (No-Take)", "SMCA")


# Build data
################################################################################

# Build data
data_wide <- data_orig %>% 
  # Reduce to MPAs
  filter(survey_type=="MPA") %>% 
  # Add MPA metadata
  left_join(mpas_orig %>% select(mpa, region, type), by="mpa") %>% 
  # Reduce to MPAs of interest
  filter(type %in% types_use) %>% 
  # Order MPA types
  mutate(type=factor(type, levels=types_use)) %>% 
  rename(mpa_type=type) %>% 
  # Simplify
  select(region, mpa_type, mpa, mpa_id, survey_id, survey_type,
         date, time_start2, time_end2, duration_hr, total_activities:comments) %>% 
  select(-comments) %>% 
  # Remove invalid surveys
  filter(duration_hr > 0)

# Inspect
freeR::complete(data_wide)

# Build data
data_long <- data_wide %>% 
  # Gather
  gather(key="activity_orig", value="activity_n", 11:ncol(.)) %>% 
  # Add column data
  left_join(col_key %>% select(activity_orig, activity, activity_type1, activity_type2, activity_type3, activity_type4), by="activity_orig") %>% 
  # Reduce
  filter(activity_type1=="Consumptive" & activity_type2!="Total" & activity_type4=="Active")

# Inspect
table(data_long$activity_type1)
table(data_long$activity_type2)
table(data_long$activity_type3)
table(data_long$activity_type4)
table(data_long$activity)

# Build data
data_long_act <- data_long %>% 
  # Convert number of activities to numeric
  mutate(activity_n=as.numeric(activity_n)) %>% 
  # Summarize by larger activity type
  group_by(region, mpa, mpa_id, mpa_type, survey_id, survey_type, date, duration_hr, 
           activity_type1, activity_type2, activity_type3) %>% 
  summarize(activity_n=sum(activity_n)) %>% 
  ungroup() %>% 
  # Compute rate
  mutate(activity_hr=activity_n/duration_hr) %>% 
  # Reduce to surveys 10-60 minutes
  filter(duration_hr>=0.15 & duration_hr<=1) %>% 
  # Recode MPA type
  mutate(mpa_type=recode(mpa_type, "SMRMA"="SMR"))

# Inspect (should be complete!)
freeR::complete(data_long_act)

# Build MPA specific stats
stats_mpa <- data_long_act %>% 
  # Summarize into broad type
  group_by(region, mpa, mpa_id, survey_id, survey_type, date, duration_hr, activity_type1) %>% 
  summarize(activity_n=sum(activity_n),
            activity_hr=sum(activity_hr),
            activity_yn=activity_n>0) %>% 
  ungroup() %>% 
  # Summarize by MPA
  group_by(mpa) %>% 
  summarize(nsurveys=n(),
            nsurveys_act=sum(activity_yn==T),
            psurveys=nsurveys_act/nsurveys,
            activity_hr=median(activity_hr[activity_yn==T])) %>% 
  ungroup() %>% 
  # Add coordinates
  left_join(mpas_orig %>% select(mpa, type, long_dd, lat_dd)) %>% 
  # Reduce to MPAs of interest
  filter(type %in% types_use) %>% 
  # Order MPA types
  mutate(type=factor(type, levels=types_use),
         type=recode(type, "SMRMA"="SMR"))

# Build network wide stats
# % of surveys with different activities
nsurveys_tot <- n_distinct(data_long_act$survey_id)
stats_network <- data_long_act %>% 
  # Summarize
  group_by(region, mpa_type, activity_type2, activity_type3) %>% 
  summarize(nsurveys=sum(activity_n>0), 
            psurveys=nsurveys/nsurveys_tot) %>% 
  ungroup()


# Plot data
################################################################################

# MPA regions
# CA/OR, Alder Creek, Pigeon Point, Point Conception, CA/MEX 
region_lats <- c(39.0, 37.18, 34.5)

# Theme
theme1 <-  theme(axis.text=element_text(size=5),
                 axis.title=element_text(size=6),
                 plot.title=element_blank(),
                 legend.text=element_text(size=5),
                 legend.title=element_text(size=6),
                 strip.text=element_text(size=6),
                 plot.tag=element_text(size=7),
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
  geom_point(data=stats_mpa,
             mapping=aes(x=long_dd, y=lat_dd, size=activity_hr, fill=psurveys, pch=type), 
             inherit.aes = F) +
  # Labels
  labs(x="", y="", tag="A") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  # Legend
  scale_size_continuous(name="Active consumptive\nactivities per hour") +
  scale_shape_manual(name="MPA type", values=c(21, 22, 23, 24)) +
  scale_fill_gradientn(name="% of surveys", colors=RColorBrewer::brewer.pal(9, "Blues"), labels=scales::percent) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", order=2), size=guide_legend(order=1)) +
  # Crop
  coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + theme1 +
  theme(axis.title.y=element_blank(),
        legend.position = c(0.8, 0.65),
        legend.key.size = unit(0.3, "cm"),
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g1

# Plot activity frequency
g2 <- ggplot(stats_network, aes(x=psurveys, y=activity_type2, fill=activity_type3)) +
  facet_grid(mpa_type~., scales="free_y", space="free_y") +
  geom_bar(stat="identity", position = "stack") +   
  # Labels
  labs(x="Percent of surveys", y="", tag="B") +
  scale_x_continuous(labels=scales::percent) +
  # Legend
  scale_fill_discrete(name="Type") +
  # Theme
  theme_bw() + theme1 +
  theme(legend.position = c(0.7, 0.8),
        legend.key.size=unit(0.3, "cm"))
g2

# Plot activities per hour
g3 <- ggplot(data_long_act %>% filter(activity_n>0), 
             aes(x=activity_hr, y=activity_type2)) +
  facet_grid(mpa_type~., scales="free_y", space="free_y") +
  geom_boxplot(outlier.shape=1, outlier.size=0.5, outlier.stroke = 0.15, lwd=0.15) +
  # Limits
  scale_x_continuous(lim=c(0,25), breaks=seq(0,25,5), labels=c("0", "5", "10", "15", "20", ">25")) +
  # Labels
  labs(x="Activities per hour", y="", tag="C") +
  # Theme
  theme_bw() + theme1 +
  theme(legend.position = "none",
        axis.text.y = element_blank())
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, ncol=3, widths=c(0.35, 0.65*0.6, 0.65*0.4))
g 

# Export
ggsave(g, filename=file.path(plotdir, "Fig3_mpa_watch_data_consump.png"), 
       width=6.5, height=3.5, units="in", dpi=600)


