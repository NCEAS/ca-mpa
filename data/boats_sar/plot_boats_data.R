

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
gisdir <- file.path(basedir, "gis_data/processed")
indir <- file.path(basedir, "boats_sar/raw")
outdir <- file.path(basedir, "boats_sar/processed")
plotdir <- "data/boats_sar/figures"

# Read data
mpas <- readRDS(file.path(gisdir, "CA_MPA_polygons.Rds"))
mpas_df <- mpas %>% sf::st_drop_geometry()
data_orig <- readRDS(file=file.path(outdir, "CA_SAR_boats_data_example.Rds"))


# Build data
################################################################################

# Build data
data <- data_orig %>% 
  # Number of boats per MPA
  filter(!is.na(mpa)) %>% 
  count(mpa) %>% 
  # Add MPA info
  left_join(mpas_df %>% select(name, region, type, area_sqkm), by=c("mpa"="name")) %>% 
  mutate(region=recode_factor(region,
                              "NCSR"="North",
                              "NCCSR"="North Central",
                              "SFBSR"="SF Bay",
                              "CCSR"="Central",
                              "SCSR"="South")) %>% 
  # Calculate boat density
  mutate(density=n/area_sqkm) %>% 
  # Arrange
  arrange(desc(n)) %>% 
  mutate(mpa=factor(mpa, levels=mpa))


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text.y=element_text(size=5),
                   axis.text.x=element_text(size=6),
                   axis.title=element_text(size=7),
                   plot.tag = element_text(size=7),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.5, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot(data, aes(y=mpa, x=n, fill=region)) +
  geom_bar(stat='identity') +
  # Labels
  labs(x="Number of boats", y="", tag="A") +
  # Legend
  scale_fill_ordinal(name="Region") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position=c(0.7, 0.8))
g1 

# Plot data
g2 <- ggplot(data, aes(y=mpa, x=density, fill=region)) +
  geom_bar(stat='identity') +
  # Labels
  labs(x="Density of boats (boats/sqkm)", y="", tag="B") +
  # Legend
  scale_fill_ordinal(name="Region") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none", 
        axis.text.y=element_blank())
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, widths=c(0.65, 0.35), nrow=1)

# Export
ggsave(g, filename=file.path(plotdir, "sar_boats_by_mpa.png"), 
       width=6.5, height=4.5, units="in", dpi=600)









