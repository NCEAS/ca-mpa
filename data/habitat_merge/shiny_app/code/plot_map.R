
# Plot map
# mpa <- mpas[15]
plot_map <- function(mpa, mpas_sf, substrate_orig, land, state_waters_line){
  
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
  
  # Raster
  data <- substrate_orig %>% 
    filter(mpa==mpa_do)
  
  # Plot data
  ###########################
  
  # Theme
  my_theme <-  theme(axis.text=element_text(size=9),
                     axis.title=element_blank(),
                     legend.text=element_text(size=12),
                     legend.title=element_text(size=14),
                     plot.title=element_text(size=14),
                     axis.text.y = element_text(angle = 90, hjust = 0.5),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.position="bottom",
                     legend.background = element_rect(fill=alpha('blue', 0)))
  
  
  # Plot data
  g <- ggplot() +
    # Plot land
    geom_sf(data=land, fill="grey80", color="white", lwd=0.3) +
    # Plot habitat
    geom_raster(data=data, mapping=aes(x=x, y=y, fill=substrate)) +
    # Plot state waters
    geom_sf(data=state_waters_line, color="grey40", lwd=0.6) +
    # Plot MPA of interest
    geom_sf(data=mpa_sf_do, fill=NA, color="black",  lwd=0.8) +
    # Labels
    labs(x="", y="", title=mpa_do) +
    # Legend
    scale_fill_manual(name="Bottom substrate",
                      values=c("gold", "goldenrod4"), drop=FALSE) +
    # Crop
    coord_sf(xlim = bbox_map[1:2], ylim = bbox_map[3:4]) +
    # Theme
    theme_bw() + my_theme 
  g
  
}



