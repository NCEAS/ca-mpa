

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
outdir <- "analyses/5community_climate_ecology/output"
plotdir <- "analyses/5community_climate_ecology/figures"


# Ellipse helper function
################################################################################

# Function to create ellipse
create_ellipse_df <- function(l, h, x1, y1, theta_deg, n = 100) {
  
  # Convert theta from degrees to radians
  theta <- theta_deg * pi / 180
  
  # Generate angles evenly spaced between 0 and 2*pi
  angles <- seq(0, 2 * pi, length.out = n)
  
  # Compute x and y coordinates for each angle
  x <- x1 + l * cos(angles) * cos(theta) - h * sin(angles) * sin(theta)
  y <- y1 + l * cos(angles) * sin(theta) + h * sin(angles) * cos(theta)
  
  # Create the data frame
  ellipse_df <- data.frame(x = x, y = y)
  
  # Return
  return(ellipse_df)
  
}

# Example usage
l <- 5  # length
h <- 3  # height
x1 <- 0  # centroid x-coordinate
y1 <- 0  # centroid y-coordinate
theta_deg <- 45  # angle in degrees

# Run function
ellipse_data <- create_ellipse_df(l, h, x1, y1, theta_deg, n = 100)


# Simulate outside ellipses
################################################################################

# Outside ellipse parameters
outside_par <- tibble(site_type="Outside", 
                      period=c("Before", "During", "After"),
                      x1=c(0.6, 1, 1.5),
                      y1=c(0.5, 1, 1.4),
                      #l=rep(0.8,3),
                      h=rep(0.3,3),
                      l = c(0.6, 0.6, 0.6),  # Adjusted length (narrower)
                      #h = c(0.2, 0.2, 0.2),  # Adjusted height (shorter)
                      theta=c(70, 50, 30))

# Build outside ellipses
x <- 1
outside_ell <- purrr::map_df(1:nrow(outside_par), function(x){
  
  # Simulate
  period <- outside_par$period[x]
  df <-  create_ellipse_df(l=outside_par$l[x], 
                           h=outside_par$h[x], 
                           x1=outside_par$x1[x], 
                           y1=outside_par$y1[x], 
                           theta_deg=outside_par$theta[x], 
                           n = 100) %>% 
    mutate(site_type="Outside", 
           period=period) %>% 
    select(site_type, period, x, y)
  
})

# Ellipses for each scenario
scenarios <- c("Heatwave impact", "No MPA benefit", "MPA resistance", "MPA recovery")
outside_ell_all <- purrr::map_df(1:length(scenarios), function(x){
  
  # Simulates
  scenario <- scenarios[x]
  df <- outside_ell %>% 
    mutate(scenario=scenario) %>% 
    select(scenario, everything())
  
})

# Centroids for each scenario
outside_ell_pts_all <- purrr::map_df(1:length(scenarios), function(x){
  
  # Simulates
  scenario <- scenarios[x]
  df <- outside_par %>% 
    mutate(scenario=scenario) %>% 
    select(scenario, everything())
  
})

# Simulate inside ellipses
################################################################################

# Inside ellipse parameters
inside_par <- tibble(site_type="Inside", 
                     scenario=c(rep("Heatwave impact", 3),
                                rep("No MPA benefit", 3),
                                rep("MPA resistance", 3),
                                rep("MPA recovery", 3)),
                     period=rep(c("Before", "During", "After"), 4),
                     x1=c(0.3, 0.4, 0.65,
                          # 0.4, 0.8, 1.2,
                          0.4, 0.6, 1.2,  # Adjusted x1 (shifted left) for middle ellipse
                           0.35, 0.38, 0.33,
                           0.4, 0.7, 0.45),
                      y1=c(1.5, 1.6, 1.7,
                           #0.7, 1.2, 1.7,
                           0.7, 1.2, 1.7,  # Adjusted y1 (shifted up) for miffle ellipse
                           0.8, 0.82, 0.78, 
                           0.6, 1.2, 0.5),
                      #l=0.8,
                     l = 0.6, #adjust length narrower
                      h=0.3,
                      theta=rep(c(70, 50, 30), 4))

# Build inside ellipses
x <- 1
inside_ell <- purrr::map_df(1:nrow(inside_par), function(x){
  
  # Simulate
  period <- inside_par$period[x]
  scenario <- inside_par$scenario[x]
  df <-  create_ellipse_df(l=inside_par$l[x], 
                           h=inside_par$h[x], 
                           x1=inside_par$x1[x], 
                           y1=inside_par$y1[x], 
                           theta_deg=inside_par$theta[x], 
                           n = 100) %>% 
    mutate(scenario=scenario,
           site_type="Inside", 
           period=period) %>% 
    select(scenario, site_type, period, x, y)
  
})



# Merge inside/outside ellipses
################################################################################

# Merge ellipses
data_sim <- bind_rows(outside_ell_all, inside_ell) %>% 
  mutate(scenario=factor(scenario, levels=scenarios),
         period=factor(period, levels=c("Before", "During", "After")),
         site_type=factor(site_type, levels=c("Inside", "Outside")))

# Merge ellipse centroids
data_sim_pts <- bind_rows(outside_ell_pts_all, inside_par) %>% 
  mutate(scenario=factor(scenario, levels=scenarios),
         period=factor(period, levels=c("Before", "During", "After")),
         site_type=factor(site_type, levels=c("Inside", "Outside")))

# Save simulated data
save(data_sim, data_sim_pts, 
     file=file.path(outdir, "simulated_ellipses_new.Rdata"))


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   plot.tag = element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.5, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Plot data
g <- ggplot(data_sim, aes(x=x, y=y, color=period, linetype=site_type)) +
  facet_wrap(~scenario, nrow=1) +
  # Ellipses
  geom_path(linewidth=0.2) +
  geom_point(data_sim_pts, mapping=aes(x=x1, y=y1, color=period, shape=site_type), size=1.2) +
  # Labels
  labs(x="NMDS1", y="NMDS2", tag="A", title="Potential MPA outcomes") +
  # Legends
  scale_linetype_manual(name="Site type", values=c("solid", "dotted"), drop=F) +
  scale_shape_manual(name="Site type", values=c(16, 17), drop=F) +
  scale_color_manual(name="Period", values=c("darkgreen", "orange", "purple")) +
  # Theme
  theme_bw() + my_theme
g

# Export
#ggsave(g, filename=file.path(plotdir, "Fig2_conceptual_schematic.png"), 
 #      width=6.5, height=2.25, units="in", dpi=600)







