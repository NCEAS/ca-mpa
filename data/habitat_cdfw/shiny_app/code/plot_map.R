
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
  bbox_buffer <- 0.05
  
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
  
  # Plot data
  g <- ggplot() +
    # Plot land
    geom_sf(data=land, fill="grey80", color="white", lwd=0.3) +
    # Plot state waters
    geom_sf(data=state_waters_line, color="grey40", lwd=0.1) +
    # Plot MPA of interest
    geom_sf(data=mpa_sf_do, color="black", fill=NA) +
    # Labels
    labs(x="", y="", title=mpa_do) +
    # Crop
    coord_sf(xlim = c(mpa_bbox$xmin-bbox_buffer, mpa_bbox$xmax+bbox_buffer),
             ylim = c(mpa_bbox$ymin-bbox_buffer, mpa_bbox$ymax+bbox_buffer)) +
    # Theme
    theme_bw() + my_theme 
  g
  
}



