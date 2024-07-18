
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)

# Directories
basedir <- "/Users/cfree/Library/CloudStorage/GoogleDrive-cfree@ucsb.edu/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
outdir <- file.path(basedir, "landings")

# MPAs
mpas_orig <- wcfish::mpas_ca
mpas <- mpas_orig %>% 
  sf::st_as_sf() %>% 
  filter(mlpa_yn==T)

# Read landings data
landings_orig <- readRDS("/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/landings_receipts/processed/1980_2022_landings_receipts.Rds")

# Read old one for comparison
old <- readRDS("analyses/2performance_fisheries/analyses/blocks/data/pre_mpa_fishing_pressure_by_mpa.Rds")

# Build data
################################################################################

# Landings stats
stats <- landings_orig %>% 
  group_by(block_id, year) %>% 
  summarize(landings_lbs=sum(landings_lbs, na.rm=T)) %>% 
  ungroup()

# Build data
x <- 3
data <- purrr::map_df(1:nrow(mpas), function(x){
  
  # MPA traits
  mdata <- mpas %>% 
    select(name, implementation_year, block_id, area_sqkm) %>% 
    slice(x) %>% 
    sf::st_drop_geometry()
    
  # Pre-years
  yr2 <- mdata$implementation_year -1
  yr1 <- yr2 - 9
  pre_years <- yr1:yr2
  
  # Average landings
  landings_lb_avg <- stats %>% 
    filter(block_id==mdata$block_id & year %in% pre_years) %>% 
    pull(landings_lbs) %>% 
    mean()
  
  # Record
  out <- mdata %>% 
    mutate(pre_years=paste(yr1, yr2, sep="-"),
           landings_lbs=landings_lb_avg,
           landings_lbs_sqkm=landings_lbs/area_sqkm)
  
})


# Export data
################################################################################

# Export data
saveRDS(data, file.path(outdir, "pre_mpa_fishing_pressure_by_mpa.Rds"))




