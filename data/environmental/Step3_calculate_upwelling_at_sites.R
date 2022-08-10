

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
plotdir <- file.path(basedir, "environmental/figures")
inputdir <- file.path(basedir, "environmental/raw")
outputdir <- file.path(basedir, "environmental/processed")

# Read monitoring sites
sites <- readRDS(file.path(basedir, "monitoring/monitoring_sites_clean.Rds"))

# Read CUTI/BEUTI
upwelling <- readRDS(file.path(outputdir, "1988_2022_cuti_beuti_daily.Rds"))


# Build upwelling time series
################################################################################

# Loop through sites and grab upwelling info
x <- 1
data_orig <- purrr::map_df(1:nrow(sites), function(x){
  
  # Subset row
  site_info <- sites[x,]
  site_lat <- site_info$lat_dd
  site_lat_bin <- round( site_lat)
  
  # Build data
  sdata <- filter(upwelling) %>% 
    # Reduce to site lat bin
    filter(lat_dd_bin==site_lat_bin ) %>% 
    # Add site info
    mutate(site=site_info$site, 
           habitat=site_info$habitat,
           mpa=site_info$mpa,
           site_type=site_info$site_type) %>% 
    # Arrange
    select(habitat, site, site_type, mpa, everything())
  
})

# Export data
saveRDS(data_orig, file.path(outputdir, "1988_2022_cuti_beuti_daily_by_monitoring_site.Rds"))

