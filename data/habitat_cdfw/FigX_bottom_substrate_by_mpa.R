

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
indir <- file.path(basedir, "habitat_cdfw/raw")
outdir <- file.path(basedir, "habitat_cdfw/processed")
plotdir <- "data/habitat_cdfw/figures"

# Get MPAs
mpas <- wcfish::mpas_ca %>% 
  sf::st_drop_geometry()

# Read data
data_orig <- readRDS(file=file.path(outdir, "bottom_substrate_by_mpa.Rds"))


# Read data
################################################################################

# Habitats
habitats <- c("Soft (0-30m)", 
              "Soft (30-100m)",
              "Soft (100-200m)",
              "Soft (200-3000m)",
              "Hard (0-30m)",
              "Hard (30-100m)",
              "Hard (100-200m)",
              "Hard (200-3000m)",
              "Unmapped")

# Habitat colors
hab_colors <- c(paste0("khaki", 1:4),
                paste0("tan", 1:4),
                "grey80")

# Format data
data <- data_orig %>% 
  # Add region
  left_join(mpas %>% select(name, region), by=c("mpa"="name"))  %>% 
  filter(region!="SFBSR") %>% 
  mutate(region=recode_factor(region,
                              "SCSR"="South",
                              "CCSR"="Central",
                              "NCCSR"="North Central",
                              "NCSR"="North")) %>% 
  # Format habitat
  mutate(habitat=factor(habitat, levels=habitats))

# Theme
my_theme <-  theme(axis.text.y=element_text(size=7),
                   axis.text.x=element_text(size=5, angle = 90, vjust = 0.5, hjust=1),
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
                   legend.key.size = unit(0.3, "cm"),
                   legend.position = "top",
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data, aes(x=mpa, y=prop, fill=habitat)) +
  facet_grid(.~region, scales="free_x", space="free_x") +
  geom_bar(stat="identity", color="grey30", lwd=0.2, position = position_fill(reverse = TRUE)) +
  # Labels
  labs(y="Habitat coverage", x="") +
  scale_y_continuous(labels=scales::percent) +
  # Legend
  scale_fill_manual(name="Bottom substrate (depth)",
                    values=hab_colors) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_bottom_substrate_by_mpa.png"), 
       width=10, height=5, units="in", dpi=600)












