

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
datadir <- "analyses/3performance_human/output"
plotdir <- "analyses/3performance_human/figures"

# Read data
data_orig <- readRDS(file=file.path(datadir, "CA_MPA_human_use_indicators.Rds"))


# Build data
################################################################################

# MPAs of interest
types_use <- c("SMR", "SMRMA", "SMCA", "SMCA (No-Take)")

# Build data
data <- data_orig %>% 
  # Reduce to MPAs of interest
  filter(type %in% types_use) %>% 
  # Recode region
  mutate(region=plyr::revalue(region, c("San Francisco Bay"="North Central Coast"))) %>% 
  # Gather indicators
  gather(key="indicator", value="value", 9:ncol(.)) %>% 
  # Format indicators
  mutate(indicator=recode_factor(indicator,
                                 "npeople_50km"="Population size",
                                 "inat_observations_tot"="iNaturalist observations",  
                                 "inat_observers_tot"="iNaturalist observers", 
                                 "reef_surveys_tot"="REEF surveys",
                                 "nonconsump_hr"="MPA Watch (non-consumptive)", 
                                 "consump_hr"="MPA Watch (consumptive)",
                                 "npermits_tot"="Scientific permits")) %>% 
  # FReduce to only MPAs with data
  filter(!is.na(value)) %>% 
  # Order MPAs by value
  group_by(indicator) %>% 
  arrange(desc(value)) %>% 
  mutate(rank=1:n(),
         rank_perc=rank/max(rank),
         value_cum=cumsum(value),
         value_cum_prop=value_cum/max(value_cum)) %>% 
  ungroup()

# Indicator data
data_ind <- data %>% 
  filter(indicator!="Population size")

# Exactly proportional
data_prop <- tibble(indicator="Even contributions",
                    rank=1:max(data_ind$rank),
                    rank_perc=rank/max(rank),
                    value_cum_prop=seq(0,1,length.out=max(rank)))

# Reference data
data_ref <- data %>% 
  # Reduce
  filter(indicator=="Population size") %>% 
  # Simplify
  select(indicator, rank, rank_perc, value_cum_prop) %>% 
  # Add exactly proportional
  bind_rows(data_prop)
  

# Plot data 
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = c(0.75, 0.25),
                   legend.key.size = unit(0.3, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data_ind, aes(x=rank_perc, y=value_cum_prop, color=indicator)) +
  # Indicator lines
  geom_line() +
  # Reference lines
  geom_line(data=data_ref, aes(x=rank_perc, y=value_cum_prop, linetype=indicator), inherit.aes = F) +
  # Reference labels
  annotate(geom="text", x=0.1, y=0.9, label="More selective\nusers", hjust=0.5, size=2.7, color="grey30") +
  annotate(geom="text", x=0.47, y=0.6, label="Less selective\nusers", hjust=0.5, size=2.7, color="grey30") +
  # Labels
  labs(x="Standardized rank order\nof an MPA's contribution to network-wide performance", 
       y="Percent of\nnetwork-wide performance") +
  # scale_x_continuous(labels=scales::percent) +
  scale_y_continuous(labels=scales::percent) +
  # Legend
  scale_color_discrete(name="Human use indictor") +
  scale_linetype_manual(name="Reference indicator", values=c("dashed", "dotted")) +
  # Theme
  theme_bw() + my_theme
g

# Export figure
ggsave(g, filename=file.path(plotdir, "Fig8_human_use_accumulation.png"), 
       width=4.5, height=4.5, units="in", dpi=600)



