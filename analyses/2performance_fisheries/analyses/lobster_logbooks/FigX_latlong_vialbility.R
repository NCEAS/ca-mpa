# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
plotdir <- "analyses/2performance_fisheries/analyses/lobster_logbooks/figures" 
outdir <- "analyses/2performance_fisheries/analyses/lobster_logbooks/output"

# Read logbook data
data_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/california/cdfw_data/data/confidential/lobster_logbooks/processed/CDFW_2000_2020_logbook_data.Rds")
data_xy_orig <- readRDS(file=file.path(outdir, "CDFW_2017_2020_lobster_logbook_data_w_latlong.Rds"))


# Merge data
################################################################################

# Lat/long logbooks
data_xy <- data_xy_orig %>% 
  select(logbook_id, block_id, lat_dd, long_dd, reliable_yn) %>% 
  unique()

# Merge
data <- data_orig %>% 
  # Add type
  left_join(data_xy) %>% 
  # Classify
  mutate(type=paste(location_type, reliable_yn, sep="-")) %>% 
  mutate(type=recode_factor(type,
                            "place-NA"="No lat/long",
                            "lat/long-NA"="Unreliable lat/long", 
                            "lat/long-no"="Unreliable lat/long", 
                            "lat/long-yes"="Reliable lat/long")) %>% 
  # SUmmariz
  group_by(year, type) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup()

table(data$type)

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot
g <- ggplot(data, aes(x=year, y=prop, fill=type)) +
  geom_bar(stat="identity", color="grey30") +
  # Labels
  labs(x="Year", y="Percent of logbooks") +
  scale_y_continuous(labels=scales::percent) +
  # Legend
  scale_fill_discrete(name="Location type") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_lobster_location_type.png"), 
       width=6.5, height=2.5, units="in", dpi=600)






