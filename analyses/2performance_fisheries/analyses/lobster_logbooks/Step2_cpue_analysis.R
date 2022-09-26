

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
plotdir <- "analyses/2performance_fisheries/analyses/lobster_logbooks/figures" 
outdir <- "analyses/2performance_fisheries/analyses/lobster_logbooks/output"


# Read data
data_orig <- readRDS(file=file.path(outdir, "CDFW_2017_2020_lobster_logbook_data_w_latlong.Rds"))


# Build data
################################################################################

# Logbooks use
data <- data_orig %>% 
  # Reliable points
  filter(reliable_yn=="yes") %>% 
  # Compute CPUE
  mutate(trap_nights=n_traps_pulled * n_nights,
         cpue=n_kept/trap_nights)

# Filter outliers
data1 <- data %>% 
  filter(cpue<=5 & dist_km<=30)


# Only MPAs with > 100 obs
nobs_thresh <- 200
data2 <- data1 %>% 
  group_by(mpa) %>% 
  mutate(mpa_nobs=n()) %>% 
  ungroup() %>% 
  filter(mpa_nobs>=nobs_thresh)
n_distinct(data2$mpa)


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   strip.text=element_text(size=7),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data, aes(x=dist_km, y=cpue)) +
  geom_point(pch=21, color="grey50", alpha=0.5) +
  geom_smooth(method = "gam", fill="grey40", color="black") +
  # Labels
  labs(x="Distance to nearest MPA (km)", y="Catch per trap-night") +
  # Limits
  # scale_x_continuous(lim=c(0, 5)) +
  # scale_y_continuous(lim=c(0, 5)) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_spillover_w_outliers.png"), 
       width=3.5, height=3.5, units="in", dpi=600)



# Plot data
g <- ggplot(data1, aes(x=dist_km, y=cpue)) +
  geom_point(pch=21, color="grey50", alpha=0.5) +
  geom_smooth(method = "gam", fill="grey40", color="black") +
  # Labels
  labs(x="Distance to nearest MPA (km)", y="Catch per trap-night") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_spillover_fewer_outliers.png"), 
       width=3.5, height=3.5, units="in", dpi=600)


# Plot data
g <- ggplot(data2, aes(x=dist_km, y=cpue)) +
  facet_wrap(~mpa, ncol=6, scales="free",
             labeller = labeller(mpa = label_wrap_gen(width = 16))) +
  geom_point(pch=21, color="grey50", alpha=0.5) +
  geom_smooth(method = "gam", fill="grey40", color="black", ) +
  # Labels
  labs(x="Distance to nearest MPA (km)", y="Catch per trap-night") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_spillover_fewer_outliers_by_mpa.png"), 
       width=6.5, height=6.5, units="in", dpi=600)



