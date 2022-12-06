

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(patchwork)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
gisdir <- file.path(basedir, "gis_data/processed")
plotdir <- "analyses/5community_climate_ecology/figures"


# Build fake data
################################################################################

# Build data
habitats <- c("Rocky intertidal", "Kelp forest\nfishes", "Kelp forest\ninvertabrates/algae", "Rocky reef", "Deep reef")
guilds <- c("Cold temperate", "Warm temperate", "Subtropical", "Tropical", "Cosmopolitan")
indicators <- c("SST", "MOCI", "CUTI", "BEUTI")
data <- expand.grid(habitat=habitats,
                    guild=guilds, 
                    indicator=indicators) %>% 
  mutate(beta=(runif(n=n())-0.5) * 2)


# Plot data
################################################################################

# Theme
base_theme <-  theme(axis.text=element_text(size=6),
                     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                     axis.title=element_blank(),
                     legend.text=element_text(size=6),
                     legend.title=element_text(size=7),
                     strip.text = element_text(size=6, face="bold", vjust=0, hjust=0),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Stip
                     strip.background = element_rect(color=NA, fill=NA),
                     # Legend
                     legend.position = "bottom",
                     legend.key.size = unit(0.4, "cm"),
                     legend.key = element_rect(fill=alpha('blue', 0)),
                     legend.background = element_rect(fill=alpha('blue', 0)))


# Plot data
g <- ggplot(data, aes(x=indicator, y=guild, fill=beta)) +
  facet_wrap(~habitat, nrow=1) +
  geom_raster() +
  # Legend
  scale_fill_gradient2(name="Coefficient",
                       midpoint=0,
                       low="darkred", high="navy", mid="white") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme
g
  
# Export
ggsave(g, filename=file.path(plotdir, "Fig5_four_corner_mockup.png"), 
       width=6.5, height=2.25, units="in", dpi=600)





