

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

# Read data
data_orig <- readRDS(file=file.path(datadir, "CA_MPA_human_use_indicators.Rds"))


# Build data
################################################################################

# Indicators of interest
indicators_use <- c(#"npeople_50km", 
                    "nonconsump_hr", "consump_hr", 
                    "inat_observers_n", "ebird_observers_n", "reef_n",
                    "permits_n", "citations_n")

# Build data
data <- data_orig %>% 
  # Reduce to MPAs of interest
  filter(mlpa=="MLPA") %>% 
  # Reduce to indicators of interest
  select(indicators_use ) %>% 
  # Rename
  rename('REEF\nsurveys'="reef_n",
         "Scientific\npermits"="permits_n",
         "Citations"="citations_n",
         "eBird\nobservers"="ebird_observers_n",
         "iNaturalist\nobservers"="inat_observers_n",
         "MPA Watch\nnon-consumptive"="nonconsump_hr",
         "MPA Watch\nconsumptive"="consump_hr")


# Plot data
################################################################################

# Theme
theme1 <-  theme(axis.text=element_text(size=6),
                 axis.title=element_text(size=6.5),
                 plot.title=element_blank(),
                 strip.text=element_text(size=6.5),
                 # Gridlines
                 panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(), 
                 axis.line = element_line(colour = "black"))

# Plot data
g <- GGally::ggpairs(data, upper = list(continuous = GGally::wrap("cor", size=2.3))) +
  # Theme
  theme_bw() + theme1
g

# Export
ggsave(g, filename=file.path(plotdir, "FigS18_indicator_correlation.png"), 
       width=6.5, height=6.5, units="in", dpi=600)



