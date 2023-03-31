

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(patchwork)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data" #Chris
datadir <- file.path(basedir, "monitoring/processed_data/community_climate_derived_data/statewide_data")
plotdir <- "analyses/5community_climate_ecology/figures"

# Read data
data_orig <- read.csv(file.path(datadir, "MPA_centroid_distances_with_traits.csv"), as.is=T)

# Read MPA metadata
mpas_data <- readRDS(file.path(basedir, "mpa_traits/processed/CA_mpa_metadata.Rds"))


# Format data
################################################################################

# Build data
data <- data_orig %>%
  # Rename
  janitor::clean_names() %>% 
  # Reduce
  select(mpa, habitat, process, distance) %>% 
  # Arrange process
  mutate(process=factor(process, levels=c("Resistance", "Recovery"))) %>% 
  # Arrange habitats
  mutate(habitat=recode_factor(habitat,
                               "Rocky intertidal"="Rocky\nintertidal",
                               "Kelp forest inverts and algae"="Kelp forest\ninverts/algae",
                               "Kelp forest fishes"="Kelp forest\nfishes")) %>% 
  # Format MPA names
  mutate(mpa=stringr::str_to_title(mpa),
         mpa=gsub("Smr", "SMR", mpa),
         mpa=gsub("Smca", "SMCA", mpa),
         mpa=recode(mpa, 
                    "Point Vicente SMCA"="Point Vicente SMCA (No-Take)",
                    "Campus Point SMCA"="Campus Point SMCA (No-Take)",
                    "Blue Cavern Onshore SMCA"="Blue Cavern Onshore SMCA (No-Take)")) %>% 
  # Add region
  left_join(mpas_data %>% select(mpa, region, lat_dd), by="mpa") %>% 
  # Arrange MPAs
  mutate(region=recode_factor(region,
                              "North Central Coast"="North",
                              "Central Coast"="Central",
                              "South Coast"="South")) %>% 
  arrange(desc(lat_dd)) %>% 
  mutate(mpa=factor(mpa, levels = unique(mpa) %>% rev())) %>% 
  # Add simulates perf shift
  mutate(dist_perc=runif(n=n(), -0.55, 0.55),
         dist_perc=ifelse(habitat=="Rocky\nintertidal", NA, dist_perc))


# Plot data
################################################################################

# Base theme
base_theme <- theme(axis.text=element_text(size=7),
                    axis.title=element_text(size=8),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=7),
                    strip.text=element_text(size=7),
                    plot.tag=element_text(size=9),
                    plot.title=element_blank(),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.key.size = unit(0.3, "cm"),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Schematic theme
schem_theme <- theme_minimal() +
               theme(legend.position="none", 
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     plot.title = element_text(size=7),
                     axis.text=element_text(size=6),
                     plot.tag=element_text(size=9),
                     axis.title = element_blank(),
                     axis.text.x = element_blank(),
                     axis.text.y=element_text(color=c("#377EB8", "#E41A1C")))

# Colors
RColorBrewer::brewer.pal(2, "Set1")

# Plot schematic 1
toy1 <- tibble(site=factor(c("MPA", "Reference"), levels=c("Reference", "MPA")),
               distance=c(0.5, 0.7))
schem1 <- ggplot(toy1, aes(y=site, yend=site, xend=distance, color=site)) +
  geom_segment(x=0, arrow = arrow(length=unit(0.30, "cm"))) +
  # Labels
  labs(title="MPA prevents shifts") +
  scale_color_manual(values=c("#377EB8", "#E41A1C")) +
  # Limits
  lims(x=c(0, 0.8)) +
  # Theme
  schem_theme
schem1

# Plot schematic 1
toy2 <- tibble(site=factor(c("MPA", "Reference"), levels=c("Reference", "MPA")),
               distance=c(0.7, 0.5))
schem2 <- ggplot(toy2, aes(y=site, yend=site, xend=distance, color=site)) +
  geom_segment(x=0, arrow = arrow(length=unit(0.30, "cm"))) +
  # Labels
  labs(title="MPA exacerbates shifts") +
  scale_color_manual(values=c( "#377EB8", "#E41A1C")) +
  # Limits
  lims(x=c(0, 0.8)) +
  # Theme
  schem_theme
schem2

# Plot data
g1 <- ggplot(data, aes(x=habitat, y=mpa, size=distance, fill=dist_perc)) +
  facet_grid(region~process, space="free_y", scale="free_y") +
  geom_point(pch=21) +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_size_continuous(name="Shift distance\n(smaller = more resilient)") +
  scale_fill_gradient2(name="% of shift\nprevented (red)\nor exacerbated (blue)",
                       midpoint=0, high="#E41A1C", low="#377EB8", mid="white") +
  guides(size = guide_legend(order = 1),
         fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title = element_blank(),
        axis.text = element_text(size=6),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g1

# Merge
layout_matrix <- matrix(c(1,2,
                          3,3), ncol=2, byrow=T)
g1_full <- gridExtra::grid.arrange(schem1, schem2, g1, 
                                   layout_matrix=layout_matrix,
                                   heights=c(0.1, 0.9))
g1_full


# Export
ggsave(g1_full, filename=file.path(plotdir, "Fig5_resistance_recovery_statewide.png"), 
       width=6.5, height=6.5, units="in", dpi=600)



