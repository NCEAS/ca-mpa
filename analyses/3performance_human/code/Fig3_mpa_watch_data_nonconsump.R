

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(tidytext)
library(lubridate)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
gisdir <- file.path(basedir, "gis_data/processed")
datadir <- file.path(basedir, "mpa_watch/processed")
plotdir <- "analyses/3performance_human/figures"
outputdir <- "analyses/3performance_human/output"

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Read MPA data
mpas_orig <- readRDS(file.path(basedir, "mpa_traits/processed", "CA_mpa_metadata.Rds"))

# Read MPW watch data
data_orig <- readRDS(file.path(datadir, "MPA_Watch_2011_2022_surveys_ca_programs_wide.Rds"))
col_key <- readxl::read_excel(file.path(datadir, "column_key_ca_programs.xlsx"))

# MPA types
types_use <- c("SMR", "SMRMA", "SMCA", "SMCA (No-Take)")


# Build data
################################################################################

# Build data
data_wide <- data_orig %>% 
  # Add MPA metadata
  left_join(mpas_orig %>% select(mpa, region), by=c("site"="mpa")) %>%
  # Reduce to MPAs
  filter(site_type=="MPA") %>% 
  rename(mpa=site) %>% 
  # Simplify
  select(region, mpa,  survey_id, site_type,
         date, time_start2, time_end2, duration_hr, total_activities:comments) %>% 
  select(-comments) %>% 
  # Remove invalid surveys
  filter(duration_hr > 0)

# Inspect
freeR::complete(data_wide)

# Plot duration
g <- ggplot(data_wide, aes(x=duration_hr*60)) +
  geom_histogram(breaks=seq(0, max(data_wide$duration_hr)*60, 5))  +
  scale_x_continuous(breaks=seq(0, 4*60, 15), lim=c(0,4*60)) +
  # Breaks
  geom_vline(xintercept = c(10,60)) +
  # Labels
  labs(x="Survey duration (minutes)") +
  # Theme
  theme_bw()
g

# Plot start end/end times
survey_times <- data_wide %>% 
  select(survey_id, time_start2, time_end2) %>% 
  gather(key="timepoint", value="time", 2:3) %>% 
  mutate(timepoint=recode_factor(timepoint,
                                 "time_start2"="Start", "time_end2"="Finish"))
survey_time_cutoffs <- tibble(timepoint=factor(c("Start", "Finish"), levels=c("Start", "Finish")),
                              cutoff=c(19,7))
g <- ggplot(survey_times, aes(x=time, fill=timepoint)) +
  geom_density(alpha=0.5)  +
  # Breaks
  geom_vline(data=survey_time_cutoffs, mapping=aes(xintercept=cutoff, color=timepoint)) +
  # Labels
  labs(x="Time of day (24-hour clock)", y="Density") +
  # Theme
  theme_bw()
g

# Build data
data_long <- data_wide %>% 
  # Gather
  gather(key="activity_orig", value="activity_n", 10:ncol(.)) %>% 
  # Add column data
  left_join(col_key %>% select(activity_orig, activity, activity_type1, activity_type2, activity_habitat), by="activity_orig") %>% 
  # Order habitats
  mutate(activity_habitat=factor(activity_habitat, levels=c("Sandy", "Rocky", "Offshore")))

# Inspect
table(data_long$activity_type1)
table(data_long$activity_type2)
table(data_long$activity_habitat)
#table(data_long$activity)

# Build data
data_long_act <- data_long %>% 
  # Reduce to activities
  filter(activity_type1 %in% c("Non-consumptive") & activity_type2!="Total" & !grepl("(Y/N)", activity)) %>% 
  # Get rid of enforcement
  filter(activity_type2!="Enforcement") %>% 
  # Rename activities
  mutate(activity_type2=recode(activity_type2, 
                               "Beach recreation"="Beach recreation\n(e.g., walking, resting, playing)",
                               "Offshore recreation"="Offshore recreation\n(e.g., swimming, bodysurfing)",
                               "Board sports"="Board sports\n(e.g., surfing, bodyboarding)")) %>% 
  # Convert number of activities to numeric
  mutate(activity_n=as.numeric(activity_n)) %>% 
  # Summarize by larger activity type
  group_by(region, mpa, survey_id, site_type, date, duration_hr, activity_type2, activity_habitat) %>% 
  summarize(activity_n=sum(activity_n)) %>% 
  ungroup() %>% 
  # Compute rate
  mutate(activity_hr=activity_n/duration_hr) %>% 
  # Reduce to surveys 10-60 minutes
  filter(duration_hr>=0.15 & duration_hr<=1)

# Inspect (should be complete!)
freeR::complete(data_long_act)

# Build MPA specific stats
stats_full <- data_long_act %>% 
  # Summarize into broad type
  group_by(region, mpa, survey_id, site_type, date, duration_hr) %>% 
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
  left_join(mpas_orig %>% select(mpa, long_dd, lat_dd, type))

# Export
saveRDS(stats_full, file=file.path(outputdir, "mpa_watch_nonconsumptive_indicators.Rds"))

# Reduce
stats <- stats_full %>% 
  filter(type %in% types_use)

# Build network wide stats
# % of surveys with different activities
nsurveys_tot <- n_distinct(data_long_act$survey_id)
stats_network <- data_long_act %>% 
  # Summarize
  group_by(activity_habitat, activity_type2) %>% 
  summarize(nsurveys=sum(activity_n>0), 
            psurveys=nsurveys/nsurveys_tot) %>% 
  ungroup() %>% 
  # Order activities
  mutate(activity_type2=tidytext::reorder_within(activity_type2, -psurveys, activity_habitat))

# Format activity CPUE for plotting
data_long_act_plot <- data_long_act %>% 
  # Reduce to non-zero
  filter(activity_n>0) %>% 
  # Add psurveys so you can order
  left_join(stats_network %>% select(activity_habitat, activity_type2, psurveys)) %>% 
  # Order by psurveys
  mutate(activity_type2=tidytext::reorder_within(activity_type2, -psurveys, activity_habitat) %>% as.character(),
         activity_type2=factor(activity_type2, levels=levels(stats_network$activity_type2)))

# Build coverage data
coverage <- data_wide %>% 
  # Number of activies on each survey by location/type
  select(survey_id, mpa, site_type, date) %>% 
  unique() %>% 
  # Add year, month, dummy date
  mutate(year=lubridate::year(date),
         month=lubridate::month(date),
         date_dummy=lubridate::ymd(paste(year, month, 1, sep="-"))) %>% 
  # Number of surveys
  group_by(year, month, date_dummy, mpa, site_type) %>% 
  summarize(nsurveys=n_distinct(survey_id)) %>% 
  ungroup() %>% 
  # Add MPA meda data
  left_join(mpas_orig %>% select(mpa, region, type)) %>% 
  filter(type %in% types_use)

# Build coverage order
mpa_order <- coverage %>% 
  group_by(region, mpa) %>% 
  summarize(nsurveys=sum(nsurveys)) %>% 
  ungroup() %>% 
  arrange(region, desc(nsurveys))


# Coverage
################################################################################

# Theme
theme1 <-  theme(axis.text=element_text(size=6),
                 axis.text.y=element_text(size=5),
                 axis.title=element_text(size=8),
                 legend.text=element_text(size=6),
                 legend.title=element_text(size=7),
                 strip.text=element_text(size=7),
                 plot.tag=element_text(size=7),
                 # Gridlines
                 panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(), 
                 axis.line = element_line(colour = "black"),
                 # Legend
                 legend.background = element_rect(fill=alpha('blue', 0)))

# Plot survey coverage
g1 <- ggplot(coverage, aes(x=date_dummy, y=mpa %>% factor(., levels=mpa_order$mpa), fill=nsurveys)) +
  facet_grid(region~., space="free_y", scales="free_y") +
  geom_tile(color="grey30", lwd=0.05) +
  # Add vertical lines
  geom_vline(xintercept=c("2015-01-01", "2021-12-31") %>% lubridate::ymd(.), linetype="dashed") +
  # Labels
  labs(x="Month", y="", tag="A") +
  scale_x_date(date_breaks = "1 year", date_labels="%Y") +
  # Legend
  scale_fill_gradientn(name="# of surveys", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(), trans="log2") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + theme1 +
  theme(axis.title.y=element_blank())
g1

# Plot duration histogram
g2 <- ggplot(data_wide, aes(x=duration_hr*60)) +
  geom_histogram(breaks=seq(0, max(data_wide$duration_hr)*60, 5), fill="grey70")  +
  scale_x_continuous(breaks=seq(0, 4*60, 15), lim=c(0,4*60)) +
  # Breaks
  geom_vline(xintercept = c(10,60), linetype="dashed") +
  # Labels
  labs(x="Survey duration (minutes)", y="Number of surveys", tag="B") +
  # Theme
  theme_bw() + theme1
g2

# Plot survey start/finish times
g3 <- ggplot(survey_times, aes(x=time, fill=timepoint)) +
  geom_density(alpha=0.5)  +
  # Breaks
  geom_vline(data=survey_time_cutoffs, 
             mapping=aes(xintercept=cutoff, color=timepoint),
             linetype="dashed", show.legend = F) +
  # Labels
  labs(x="Time of day (24-hour clock)", y="Density", tag="C") +
  scale_x_continuous(breaks=seq(0,24,3)) +
  # Legend
  scale_fill_discrete(name="Time point") +
  # Theme
  theme_bw() + theme1 +
  theme(legend.position = c(0.15, 0.8),
        legend.key.size = unit(0.3, "cm"))
g3

# Merge
layout_matrix <- matrix(c(1,1,
                          2,3), byrow=T, ncol=2)
g <- gridExtra::grid.arrange(g1, g2, g3, layout_matrix=layout_matrix, heights=c(0.7, 0.3))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigS1_mpa_watch_survey_coverage.png"), 
       width=6.5, height=7, units="in", dpi=600)


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
                 legend.margin = margin(),
                 legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot() +
  # Plot regions
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot MPAs
  geom_point(data=stats,
             mapping=aes(x=long_dd, y=lat_dd, size=activity_hr, fill=psurveys), 
             pch=21, inherit.aes = F) +
  # Labels
  labs(x="", y="", tag="A") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  # Legend
  scale_size_continuous(name="Non-consumptive\nactivities per hour") +
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
g2 <- ggplot(stats_network, aes(x=psurveys, y=activity_type2, fill=activity_habitat)) +
  facet_grid(activity_habitat~., scales="free_y", space="free_y") +
  geom_bar(stat="identity", color="grey30", lwd=0.15) +   
  # Labels
  labs(x="Percent of surveys", y="", tag="B") +
  scale_x_continuous(labels=scales::percent) +
  scale_y_reordered() +
  # Legend
  scale_fill_manual(name="", values=c("gold", "slategray4", "steelblue2")) +
  # Theme
  theme_bw() + theme1 +
  theme(legend.position = "none")
g2

# Plot activities per hour
g3 <- ggplot(data_long_act_plot, aes(x=activity_hr, y=activity_type2, fill=activity_habitat)) +
  facet_grid(activity_habitat~., scales="free_y", space="free_y") +
  geom_boxplot(outlier.shape=1, outlier.size=0.5, outlier.stroke = 0.15, lwd=0.15, color="grey30") +
  # Limits
  scale_x_continuous(lim=c(0,75), breaks=seq(0,75,25), labels=c("0", "25", "50", ">75")) +
  scale_y_reordered() +
  # Labels
  labs(x="Activities per hour", y="", tag="C") +
  # Legend
  scale_fill_manual(name="", values=c("gold", "slategray4", "steelblue2")) +
  # Theme
  theme_bw() + theme1 +
  theme(legend.position = "none") +
  theme(axis.text.y = element_blank())
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, ncol=3, widths=c(0.35, 0.65*0.6, 0.65*0.4))
g 

# Export
ggsave(g, filename=file.path(plotdir, "Fig3_mpa_watch_data_nonconsum.png"), 
       width=6.5, height=3.5, units="in", dpi=600)




