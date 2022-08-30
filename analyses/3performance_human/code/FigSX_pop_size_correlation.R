

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
traitdir <- file.path(basedir, "mpa_traits/processed")
gisdir <- file.path(basedir, "census_data/processed") 
datadir <- "analyses/3performance_human/output"
plotdir <- "analyses/3performance_human/figures"

# Read data
mpas <- readRDS(file=file.path(traitdir, "CA_mpa_metadata.Rds"))
data_orig <- readRDS(file=file.path(gisdir, "MPA_population_within_multiple_buffers.Rds"))


# Build data
################################################################################

# Build data
buffer_km <- unique(data_orig$buffer_km)
x <- buffer_km[4]
data <- purrr::map_df(buffer_km, function(x){
  
  # Population size to evaluate
  df1 <- data_orig %>% 
    # Filter
    filter(buffer_km==x) %>% 
    # Simplify
    select(mpa, npeople)
  
  # Reference population size
  df2 <- data_orig %>% 
    # Filter
    filter(buffer_km==50) %>% 
    # Simplify
    select(mpa, npeople) %>% 
    rename(npeople50km=npeople) %>% 
    # Add evaluated
    left_join(df1, by="mpa") %>% 
    # Reduce to MPAS of interest
    left_join(mpas %>% select(mpa, mlpa), by="mpa") %>% 
    filter(mlpa=="MLPA")
  
  # Plot check
  # plot(npeople ~ npeople50km, df2)
  
  # Compute correlation
  r2 <- cor(df2$npeople50km, df2$npeople, method="pearson")
    
  # Record
  df_out <- tibble(buffer_km=x,
                   r2=r2)
  
})


# Plot data
################################################################################

# Theme
theme1 <-  theme(axis.text=element_text(size=7),
                 axis.title=element_text(size=8),
                 # Gridlines
                 panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(), 
                 axis.line = element_line(colour = "black"),
                 # Legend
                 legend.background = element_rect(fill=alpha('blue', 0)))

# Plot
g <- ggplot(data %>% filter(buffer_km>=10), aes(x=buffer_km, y=r2)) +
  geom_point() +
  # Labels
  labs(x="Buffer size (km)\nused to calculate neighboring population density", y="Pearson's correlation (r2)\nbetween population size within 50 km and alternative distances") +
  # Axes
  scale_x_continuous(breaks=seq(0,100,10), lim=c(0,100)) +
  scale_y_continuous(breaks=seq(0,1, 0.2), lim=c(0,1)) +
  # Theme
  theme_bw() + theme1
g

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_pop_size_correlation.png"), 
       width=4, height=4, units="in", dpi=600)


