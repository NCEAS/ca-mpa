
# Plot map
# mpa <- mpas[1]
plot_map <- function(mpa){
  
  # MPA
  mpa_do <- mpa
  
  # Find MPA
  mpa_sf_do <- mpas_sf %>% 
    filter(name==mpa_do)
  
  # Derive boundaries
  mpa_bbox <- sf::st_bbox(mpa_sf_do)
  bbox_buffer <- 1000
  
  # Map bbox
  bbox_map <- c(xmin=mpa_bbox$xmin-bbox_buffer, 
                xmax=mpa_bbox$xmax+bbox_buffer,
                ymin=mpa_bbox$ymin-bbox_buffer, 
                ymax=mpa_bbox$ymax+bbox_buffer)
  
  # Habitats
  habitats <- c("Soft (0-30m)", 
                "Soft (30-100m)",
                "Soft (100-200m)",
                "Soft (200-3000m)",
                "Hard (0-30m)",
                "Hard (30-100m)",
                "Hard (100-200m)",
                "Hard (200-3000m)")
  
  # Raster
  substrate_ras_crop <- raster::crop(x=substrate_ras, y=raster::extent(bbox_map))
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
  my_theme <-  theme(axis.text=element_text(size=7),
                     axis.title=element_text(size=8),
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
  
  # Habitat colors
  hab_colors <- c(paste0("khaki", 1:4),
                  paste0("tan", 1:4))
                  
  
  # Plot data
  g <- ggplot() +
    # Plot land
    geom_sf(data=land, fill="grey80", color="white", lwd=0.3) +
    # Plot habitat
    geom_raster(data=substrate_ras_crop_df, mapping=aes(x=x, y=y, fill=habitat)) +
    # Plot state waters
    geom_sf(data=state_waters_line, color="grey40", lwd=0.1) +
    # Plot MPA of interest
    geom_sf(data=mpa_sf_do, color="black", fill=NA) +
    # Labels
    labs(x="", y="", title=mpa_do) +
    # Legend
    scale_fill_manual(name="Bottom substrate (depth)",
                      values=hab_colors, drop=FALSE) +
    # Crop
    coord_sf(xlim = bbox_map[1:2], ylim = bbox_map[3:4]) +
    # Theme
    theme_bw() + my_theme 
  g
  
}



