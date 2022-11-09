

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
datadir <- "analyses/3performance_human/output"
plotdir <- "analyses/3performance_human/figures"

# Build data
################################################################################

# Indicators
indicators <- c("MPA Watch non-consumptive activities", 
                "MPA Watch consumptive activities",
                "iNaturalist observers", 
                "iNaturalist observations",
                "eBird observers",
                "REEF surveys")

# Means
lrr_means <- c(0.3, 
               -0.5,
               0.22,
               0.21,
               0.24,
               0.5)

# # of MPAs
n <- 125

# Simulate data
x <- 1
data <- purrr::map_df(1:length(indicators), function(x){
  
  # Params
  i <- indicators[x]
  lrr_avg <- lrr_means[x]
  lrrs <- rnorm(n=n, mean = lrr_avg, sd=0.2)
  df <- tibble(metric=i,
               lrr=lrrs)
  
})





# Plot data 
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   axis.title.y=element_blank(),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = c(0.75, 0.25),
                   legend.key.size = unit(0.3, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Plot data
g <- ggplot(data, aes(y=metric, x=lrr)) +
  geom_boxplot(fill="grey80", color="grey30", outlier.color = NA) +
  geom_jitter(height = 0.2, alpha=0.3) +
  # Reference line
  geom_vline(xintercept = 0, linetype="dashed") +
  # Text reference
  annotate(geom="text", x=-1, y=1, label="More engagement\nin non-MPAs", hjust=0, size=2.4) +
  annotate(geom="text", x=1.4, y=1, label="More engagement\nin MPAs", hjust=1, size=2.4) +
  # Labels
  labs(x="Log-response ratio\n(MPAs vs. non-MPA counterfactuals)", y="") +
  # Theme
  theme_bw() + my_theme
g

# Export figure
ggsave(g, filename=file.path(plotdir, "Fig13_mpa_nonmpa_comparison.png"), 
       width=5.5, height=2.5, units="in", dpi=600)



