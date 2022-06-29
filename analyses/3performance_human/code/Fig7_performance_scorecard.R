

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
  # Scale metrics
  group_by(indicator) %>% 
  mutate(value_scaled=scale(value, center=T, scale=T)) %>% 
  ungroup()

# Derive MPA order
mpa_order <- data %>%
  filter(indicator=="Population size") %>% 
  arrange(region, desc(value))

# Order data
data_ordered <- data %>%
  mutate(mpa=factor(mpa, levels=mpa_order$mpa))


# Plot data - long
################################################################################

# Theme
theme1 <- theme(axis.text=element_text(size=5),
                axis.text.y=element_text(size=4.5),
                axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                axis.title=element_blank(),
                legend.text=element_text(size=6),
                legend.title=element_text(size=8),
                strip.text=element_text(size=7),
                plot.title=element_blank(),
                # Gridlines
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black"),
                # Legend
                legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data_ordered, aes(x=indicator, y=mpa, fill=value_scaled)) +
  # Facet
  facet_grid(region~., scales="free_y", space="free_y") +
  # Raster
  geom_tile(lwd=0.1, color="grey60") +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_fill_gradient2("Performance\n(scaled and centered)", 
                       midpoint = 0, low="darkred", high="navy", mid="white", na.value="grey50") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + theme1
g

# Export figure
ggsave(g, filename=file.path(plotdir, "Fig7_performance_scorecard_long.png"), 
       width=4.5, height=7.5, units="in", dpi=600)


# Plot data - wide
################################################################################

# Subset data
data1 <- data_ordered %>% 
  filter(region!="South Coast")
data2 <- data_ordered %>% 
  filter(region=="South Coast")

# Theme
theme1 <- theme(axis.text=element_text(size=6),
                axis.text.y=element_text(size=6),
                axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                axis.title=element_blank(),
                legend.text=element_text(size=6, angle = 90, vjust = 1, hjust=1),
                legend.title=element_text(size=7),
                strip.text=element_text(size=7),
                plot.title=element_blank(),
                # Gridlines
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black"),
                # Legend
                legend.key.size = unit(0.35, "cm"),
                legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot(data1, aes(x=indicator, y=mpa, fill=value_scaled)) +
  # Facet
  facet_grid(region~., scales="free_y", space="free_y") +
  # Raster
  geom_tile(lwd=0.1, color="grey60") +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_fill_gradient2("Performance\n(scaled and centered)", 
                       midpoint = 0, low="darkred", high="navy", mid="white", na.value="grey50") +  
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + theme1 +
  theme(legend.position = "none")
g1

# Plot data
g2 <- ggplot(data2, aes(x=indicator, y=mpa, fill=value_scaled)) +
  # Facet
  facet_grid(region~., scales="free_y", space="free_y") +
  # Raster
  geom_tile(lwd=0.1, color="grey60") +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_fill_gradient2("Performance\n(scaled and centered)", 
                       midpoint = 0, low="darkred", high="navy", mid="white", na.value="grey50") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + theme1 +
  theme(legend.position = "top")
g2

# Merge
g <-gridExtra::grid.arrange(g1, g2, nrow=1)
g

# Export figure
ggsave(g, filename=file.path(plotdir, "Fig7_performance_scorecard_wide.png"), 
       width=6.5, height=6.5, units="in", dpi=600)










