
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(MatchIt)
library(Polychrome)
library(cobalt)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
plotdir <- "analyses/3performance_human/figures"
datadir <- file.path(basedir, "counterfactuals")
outputdir <- "analyses/3performance_human/output"

# Load data
load(file.path(outputdir, "counterfactual_output.Rds"))

# Read original data
data_orig <- readRDS(file.path(datadir, "counterfactual_layers_shallow_epsg3309.Rds"))

# Get MPAs
mpas_orig <- wcfish::mpas_ca
mpas <- mpas_orig %>% 
  filter(type!="SMP")

# Read data
################################################################################

# Format data
data <- data_orig %>% 
  # Remove useless
  select(-inat_count) %>% 
  # Rename
  rename(cell_id=cell, 
         depth_m=bathymetry, 
         npeople_50km=population_density_50km,
         shore_dist_km=distance_shore,
         park_dist_km=distance_park, # entry point
         beach_dist_km=distance_beach, # beach access
         mpa_yn=mpa_bin, 
         nbeaches_600m=beach_access_count_600m, # beach access points
         nparks_600m=park_count_600m) %>% # park access points
  # Convert NAs to zeros
  mutate(npeople_50km=ifelse(is.na(npeople_50km), 0, npeople_50km),
         nparks_600m=ifelse(is.na(nparks_600m), 0, nparks_600m),
         nbeaches_600m=ifelse(is.na(nparks_600m), 0, nparks_600m)) %>% 
  # Format MPA yes/no
  mutate(mpa_yn=ifelse(is.na(mpa_yn), 0, mpa_yn)) %>% 
  # Format MPA id
  mutate(mpa_id=ifelse(is.nan(mpa_id), NA, mpa_id)) %>% 
  # Add MPA name
  mutate(mpa=mpas$name[mpa_id]) %>% 
  # Arrange
  select(cell_id, x_epsg3309, y_epsg3309, 
         mpa_yn, mpa_id, mpa,
         shore_dist_km, depth_m, npeople_50km, 
         park_dist_km, beach_dist_km,
         nparks_600m, nbeaches_600m)

# Build data for pre-matching balance
data_pre <- data %>% 
  # Reduce
  select(mpa_yn,
         shore_dist_km, depth_m, npeople_50km, 
         park_dist_km, beach_dist_km,
         nparks_600m, nbeaches_600m) %>% 
  # Format some
  mutate(mpa_yn=ifelse(mpa_yn==1, "MPA", "Non-MPA"), 
         beach_dist_km=beach_dist_km/1000,
         park_dist_km=park_dist_km/1000,
         npeople_50km=npeople_50km/1e6,
         depth_m=depth_m*-1) %>% 
  # Gather
  gather(key="variable", value="value", 2:ncol(.)) %>% 
  # Recode variables
  mutate(variable=recode_factor(variable,
                                "depth_m"="Depth (m)",
                                "shore_dist_km"="Distance from shore (km)",
                                "npeople_50km"="Millions of people\nwithin 50 km",
                                "nparks_600m"="Number of park\nentry points within 600 m",
                                "park_dist_km"="Distance to nearest\npark entry point (km)",
                                "nbeaches_600m"="Number of beach\naccess points within 600 m",
                                "beach_dist_km"="Distance to nearest\nbeach access point (km)"))

# Build data for post-matching balance
data_post <- matches %>% 
  # Reduce
  select(subclass, mpa_yn, 
         shore_dist_km, depth_m, npeople_50km, 
         park_dist_km, beach_dist_km,
         nparks_600m, nbeaches_600m) %>% 
  # Format some
  mutate(mpa_yn=ifelse(mpa_yn==1, "MPA", "Non-MPA"), 
         beach_dist_km=beach_dist_km/1000,
         park_dist_km=park_dist_km/1000,
         npeople_50km=npeople_50km/1e6,
         depth_m=depth_m*-1) %>% 
  # Gather
  gather(key="variable", value="value", 3:ncol(.)) %>% 
  # Recode variables
  mutate(variable=recode_factor(variable,
                                "depth_m"="Depth (m)",
                                "shore_dist_km"="Distance from shore (km)",
                                "npeople_50km"="Millions of people\nwithin 50 km",
                                "nparks_600m"="Number of park\nentry points within 600 m",
                                "park_dist_km"="Distance to nearest\npark entry point (km)",
                                "nbeaches_600m"="Number of beach\naccess points within 600 m",
                                "beach_dist_km"="Distance to nearest\nbeach access point (km)"))

# Create difference boxplot
data_post_sd <- data_post %>% 
  # Spread
  mutate(mpa_yn=recode(mpa_yn, 
                       "MPA"="mpa",
                       "Non-MPA"="counterfactual")) %>% 
  spread(key="mpa_yn", value="value") %>% 
  rowwise() %>% 
  mutate(avg=mean(mpa, counterfactual), 
         pdiff=(mpa-counterfactual)/avg*100)

# Plot data
################################################################################

# Theme
theme1 <- theme(axis.text=element_text(size=6),
                axis.title=element_text(size=7),
                legend.text=element_text(size=6),
                legend.title=element_text(size=),
                strip.text=element_text(size=6),
                plot.title=element_text(size=8),
                plot.tag=element_text(size=8),
                # Gridlines
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black"),
                # Legend
                legend.key.size=unit(0.2, "cm"),
                legend.background = element_rect(fill=alpha('blue', 0)))

# Plot pre-match balance
g1 <- ggplot(data_pre %>% sample_frac(0.1), aes(x=value, fill=mpa_yn)) +
  facet_wrap(~variable, ncol=1, scales="free") +
  geom_density(alpha=0.5) +
  # Labels
  labs(x="", y="Density", tag="A", title="Pre-match balance") +
  scale_fill_discrete(name="") +
  # Theme
  theme_bw() + theme1 +
  theme(legend.position = c(0.7, 0.98))
#g1

# Plot post-match balance
g2 <- ggplot(data_post %>% sample_frac(0.1), aes(x=value, fill=mpa_yn)) +
  facet_wrap(~variable, ncol=1, scales="free") +
  geom_density(alpha=0.5) +
  # Labels
  labs(x="", y="Density", tag="B", title="Post-match balance") +
  scale_fill_discrete(name="") +
  # Theme
  theme_bw() + theme1 +
  theme(legend.position = "none")
#g2

# Plot 
g3 <- ggplot(data_post_sd %>% filter(!is.nan(pdiff)), aes(x=mpa, y=counterfactual)) +
  facet_wrap(~variable, ncol=1, scales="free") +
  geom_point(pch=1, alpha=0.5, size=0.7) +
  # Reference line
  geom_abline(slope=1) +
  # Labels
  labs(x="MPA cell value", y="Non-MPA cell value", tag="C", title=" ") +
  # Theme
  theme_bw() + theme1
# g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, ncol=3)
#g

# Export
ggsave(g, filename=file.path(plotdir, "FigS19_counterfactual_balance.png"), 
       width=5.5, height=7.5, units="in", dpi=600)




