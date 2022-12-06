

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(patchwork)

# Directories
#basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
basedir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/community_climate_derived_data/"
gisdir <- file.path(basedir, "gis_data/processed")
plotdir <- "analyses/5community_climate_ecology/figures"


# Build fake data
################################################################################

# Build four-corner data
habitats <- c("Rocky intertidal", "Kelp forest\nfishes", "Kelp forest\ninvertebrates/algae", "Rocky reef", "Deep reef")
habitats_long <- c("Rocky intertidal", "Kelp forest fishes", "Kelp forest invertebrates/algae", "Rocky reef", "Deep reef")
habitats_short <- c("Rocky intertidal", "Kelp forest fishes", "Kelp forest inv/alg", "Rocky reef", "Deep reef")
guilds <- c("Cold temperate", "Warm temperate", "Subtropical", "Tropical", "Cosmopolitan")
indicators <- c("SST", "MOCI", "CUTI", "BEUTI")
data <- expand.grid(habitat=habitats,
                    guild=guilds, 
                    indicator=indicators) %>% 
  mutate(beta=(runif(n=n())-0.5) * 2)
data2 <- expand.grid(habitat=habitats_short,
                    guild=guilds, 
                    indicator=indicators) %>% 
  mutate(beta=(runif(n=n())-0.5) * 2)

# Build composition data
comp_data <- expand.grid(year=2007:2020,
                         habitat=habitats_long,
                         guild=guilds) %>% 
  mutate(n=runif(n=n())) %>% 
  group_by(habitat, year) %>% 
  mutate(perc=n/sum(n)) %>% 
  ungroup() %>% 
  arrange(habitat, year, guild)

#load real data
corner_dat <- load(file.path(basedir,"four_corner_output.rda"))
comp_dat1 <- load(file.path(basedir,"comp_data.rda"))

# Plot data
################################################################################

# Theme
base_theme <-  theme(axis.text=element_text(size=7),
                     axis.title=element_text(size=8),
                     legend.text=element_text(size=7),
                     legend.title=element_text(size=8),
                     strip.text=element_text(size=8),
                     plot.tag=element_text(size=9),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.key = element_rect(fill=alpha('blue', 0)),
                     legend.background = element_rect(fill=alpha('blue', 0)))


# Plot data
g <- ggplot(coef_out, aes(x=`Environmental Variables`, y=`Thermal affinity`, fill=Beta)) +
  facet_wrap(~Group, nrow=1) +
  geom_raster() +
  # Legend
  scale_fill_gradient2(name="Coefficient",
                       midpoint=0,
                       low="darkred", high="navy", mid="white") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.4, "cm"),
        strip.text = element_text(size=6, face="bold", vjust=0, hjust=0),
        strip.background = element_rect(color=NA, fill=NA),
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g
  
# Export
#ggsave(g, filename=file.path(plotdir, "Fig5_four_corner_mockup.png"), 
#       width=6.5, height=2.25, units="in", dpi=600)


# Plot data
################################################################################

# Plot composition
g1 <- ggplot(comp_data, aes(x=year, y=group_total, fill=thermal_affinity)) +
  facet_wrap(~group, ncol=1) +
  geom_bar(stat="identity", position = position_fill(reverse = TRUE)) +
  # Refence lines
  geom_vline(xintercept = c(2013.5, 2016.5), linetype="dashed") +
  # Labels
  labs(x="Year\n", y="Percent of community", tag="A") +
  scale_x_continuous(breaks=2007:2020) +
  scale_y_continuous(labels=scales::percent) +
  # Legend
  scale_fill_manual(name="",
                    values=c(RColorBrewer::brewer.pal(4, "Spectral") %>% rev(), "grey70")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.3, "cm"),
        legend.margin = margin(-5,0,5,0), # 3 is to align x-axis of panels
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g1

# Plot four corner
g2 <- ggplot(coef_out, aes(x=`Environmental Variables`, y=`Thermal affinity`, fill=Beta)) +
  facet_wrap(~Group, ncol=1) +
  geom_raster() +
  # Labels
  labs(x="Indicator", y="Thermal affinity", tag="B") +
  # Legend
  scale_fill_gradient2(name="Beta",
                       midpoint=0,
                       low="darkred", high="navy", mid="white") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.3, "cm"),
        legend.margin = margin(-5,0,0,0),
        # axis.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, ncol=2, widths=c(0.66,  0.34))
g

# Export
#ggsave(g, filename=file.path(plotdir, "Fig5_species_composition_mockup.png"), 
#       width=6.5, height=6.5, units="in", dpi=600)










