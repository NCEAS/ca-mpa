

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
plotdir <- "data/habitat_cdfw/figures/individual_mpas"

# Read data
substrate_orig <- raster::raster(file.path(outdir, "CA_bottom_substrate_10m.tiff")) 

# Get MPAs
mpas <- wcfish::mpas_ca %>% 
  sf::st_transform(crs=raster::crs(substrate_orig))

# Export
mpa_stats <- readRDS(file=file.path(outdir, "bottom_substrate_by_mpa.Rds"))

# Get land
# usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
land <- rnaturalearth::ne_countries(country=c("United States of America", "Mexico"),
                                    scale="large", returnclass = "sf") %>% 
  sf::st_transform(crs=raster::crs(substrate_orig))

# Get state waters
state_waters_line <- readRDS(file.path(basedir, "gis_data/processed", "CA_state_waters_polyline.Rds")) %>% 
  sf::st_transform(crs=raster::crs(substrate_orig))

# Habitats
habitats <- c("Soft (0-30m)", 
              "Soft (30-100m)",
              "Soft (100-200m)",
              "Soft (200-3000m)",
              "Hard (0-30m)",
              "Hard (30-100m)",
              "Hard (100-200m)",
              "Hard (200-3000m)")

# Habitat colors
hab_colors <- c(paste0("khaki", 1:4),
                paste0("tan", 1:4))

# Habitats
habitats2 <- c("Soft (0-30m)", 
              "Soft (30-100m)",
              "Soft (100-200m)",
              "Soft (200-3000m)",
              "Hard (0-30m)",
              "Hard (30-100m)",
              "Hard (100-200m)",
              "Hard (200-3000m)",
              "Unmapped")

# Habitat colors
hab_colors2 <- c(paste0("khaki", 1:4),
                paste0("tan", 1:4),
                "white")


# Plotting function
################################################################################

mpa <- mpas$name[1]
plot_data <- function(mpa){
  
  # MPA
  mpa_do <- mpa
  
  # Find MPA
  mpa_sf_do <- mpas %>% 
    filter(name==mpa_do)
  
  # Derive boundaries
  mpa_bbox <- sf::st_bbox(mpa_sf_do)
  bbox_buffer <- 1000
  
  # Map bbox
  bbox_map <- c(xmin=mpa_bbox$xmin-bbox_buffer, 
                xmax=mpa_bbox$xmax+bbox_buffer,
                ymin=mpa_bbox$ymin-bbox_buffer, 
                ymax=mpa_bbox$ymax+bbox_buffer)
  
  # Raster
  substrate_ras_crop <- raster::crop(x=substrate_orig, y=raster::extent(bbox_map))
  substrate_ras_crop_df <- substrate_ras_crop %>% 
    # Convert to raster
    raster::as.data.frame(xy=T) %>% 
    # Rename
    setNames(c("x", "y", "habitat")) %>% 
    # Reduce to cells with habitat
    filter(!is.na(habitat)) %>% 
    # Recode habitat
    mutate(habitat=as.character(habitat),
           habitat=recode(habitat,
                          "1"="Soft (0-30m)", 
                          "2"="Soft (30-100m)",
                          "3"="Soft (100-200m)",
                          "4"="Soft (200-3000m)",
                          "5"="Hard (0-30m)",
                          "6"="Hard (30-100m)",
                          "7"="Hard (100-200m)",
                          "8"="Hard (200-3000m)") %>% factor(., levels=habitats))
  
  # Theme
  my_theme <-  theme(axis.text=element_text(size=6),
                     axis.title=element_blank(),
                     legend.text=element_text(size=7),
                     legend.title=element_text(size=8),
                     plot.title=element_text(size=9),
                     axis.text.y = element_text(angle = 90, hjust = 0.5),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.background = element_rect(fill=alpha('blue', 0)))
  
  
  # Plot data
  g1 <- ggplot() +
    # Plot land
    geom_sf(data=land, fill="grey80", color="white", lwd=0.3) +
    # Plot habitat
    geom_raster(data=substrate_ras_crop_df, mapping=aes(x=x, y=y, fill=habitat)) +
    # Plot state waters
    geom_sf(data=state_waters_line, color="grey40", lwd=0.6) +
    # Plot MPA of interest
    geom_sf(data=mpa_sf_do, fill=NA, color="black",  lwd=0.8) +
    # Labels
    labs(x="", y="", title=mpa_do) +
    # Legend
    scale_fill_manual(name="Bottom substrate (depth)",
                      values=hab_colors, drop=FALSE) +
    # Crop
    coord_sf(xlim = bbox_map[1:2], ylim = bbox_map[3:4]) +
    # Theme
    theme_bw() + my_theme 
  g1
  
  # Barplot
  #########################################
  
  # Find MPA
  sdata <- mpa_stats %>% 
    filter(mpa==mpa_do) %>% 
    mutate(habitat=factor(habitat, levels=habitats2))
  
  # Theme
  my_theme2 <-  theme(axis.text=element_text(size=7),
                     axis.title=element_text(size=8),
                     legend.text=element_text(size=7),
                     legend.title=element_text(size=8),
                     axis.text.y = element_text(angle = 90, hjust = 0.5),
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
  g2 <- ggplot(sdata, aes(x=prop, y="", fill=habitat)) +
    geom_bar(stat="identity", color="grey30", lwd=0.2) +
    # Labels
    labs(y="", x="Habitat coverage (%)") +
    scale_x_continuous(labels=scales::percent) +
    # Legend
    scale_fill_manual(name="Bottom substrate (depth)",
                      values=hab_colors2, drop=F) +
    # Theme
    theme_bw() + my_theme2
  g2
  
  # Merge
  g <- gridExtra::grid.arrange(g1, g2, ncol=1, heights=c(0.7, 0.3))
  g
  
}

# Loop through and plot
################################################################################

# MPAs
mpas_do <- mpas$name

# Loop through
i <- 1
for(i in 1:length(mpas_do)){
  
  # MPA
  mpa_do <- mpas_do[i]
  
  # Plot
  g <- plot_data(mpa_do)
  
  # Export
  ggsave(g, filename=file.path(plotdir, paste0(gsub("/", "-", mpa_do), ".png")), 
         width=6.5, height=5.5, units="in", dpi=600)
  
}






