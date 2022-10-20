
# Plot map
# mpa <- mpas[1]
plot_barplot <- function(mpa){
  
  # MPA
  mpa_do <- mpa
  
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
                  "white")
  
  # Find MPA
  sdata <- substrate_by_mpa %>% 
    filter(mpa==mpa_do) %>% 
    mutate(habitat=factor(habitat, levels=habitats))
  
  # Theme
  my_theme <-  theme(axis.text=element_text(size=13),
                     axis.title=element_text(size=14),
                     legend.text=element_text(size=12),
                     legend.title=element_text(size=14),
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
  g <- ggplot(sdata, aes(x=prop, y="", fill=habitat)) +
    geom_bar(stat="identity", color="grey30", lwd=0.2) +
    # Labels
    labs(y="", x="Habitat coverage (%)") +
    scale_x_continuous(labels=scales::percent) +
    # Legend
    scale_fill_manual(name="Bottom substrate (depth)",
                      values=hab_colors, drop=F) +
    # Theme
    theme_bw() + my_theme 
  g
  
}



