
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
datadir <- "analyses/2performance_fisheries/data"
plotdir <- "analyses/2performance_fisheries/figures"

# Read data
data_orig <- readxl::read_excel(file.path(datadir, "fisheries.xlsx"))


# Build data
################################################################################

# Build data
types <- c("None\ndetected", "Preemptive\nfishing", "Displacement", "Spillover")
data <- expand.grid(fishery=data_orig$fishery,
                    mpa_impact=types) %>% 
  left_join(data_orig) %>% 
  select(type, fishery, mpa_impact) %>% 
  mutate(nregions=sample(1:4, nrow(.), replace=T) %>% as.character() %>% as.factor())

  


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=7),
                   plot.title=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Treatment
g <- ggplot(data, aes(x=mpa_impact, y=fishery, fill=nregions)) +
  facet_grid(type~., scales="free_y", space="free_y") +
  geom_tile() +
  # Labels
  labs(x="MPA impact", y="Fishery", title="Simulated results: proof of concept only") + 
  # Legend
  scale_fill_ordinal(name="# of regions") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_fishery_results_.png"), 
       width=5, height=4, units="in", dpi=600)

