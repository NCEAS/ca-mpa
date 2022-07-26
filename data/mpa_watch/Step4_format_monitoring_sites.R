

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data/"
indir <- file.path(basedir, "mpa_watch/raw")
outdir <- file.path(basedir, "mpa_watch/processed")
plotdir <- "data/mpa_watch/figures"

# Read MPA data
mpas <- readRDS(file.path(basedir, "mpa_traits/processed", "CA_mpa_metadata.Rds"))

# Read data
sites_orig <- sf::st_read(file.path(indir, "Monitoring_sites_shapefile", "MPAsControlSites_2022-05-09.shp"))

# Read monitoring data
data_orig <- readRDS(file=file.path(outdir, "MPA_Watch_2011_2022_surveys_ca_programs_wide.Rds"))


# Format data
################################################################################

# Extract site info from data
sites_in_data <- data_orig %>% 
  # Reduce to unique
  select(program, site_id, site, site_type, survey_site, survey_site_type, survey_site_id) %>% 
  unique() %>% 
  # Arrange
  arrange(program, site_id, survey_site_id)


# Export data
write.csv(sites_in_data, file=file.path(outdir, "mpa_watch_survey_sites.csv"), row.names=F)

# 
# # Confirm unique
# anyDuplicated(sites_in_data$survey_site_id) # site IDs are unique
# anyDuplicated(sites_in_data$survey_site) # site names are unique
# 
# # Inspect types
# table(sites_orig$TYPE)
# sites_orig$NAME
# 
# # Format data
# sites <- sites_orig %>% 
#   # Rename
#   janitor::clean_names("snake") %>% 
#   rename(site_id=id, site_name=name, site_type=type) %>% 
#   # Recode site type
#   mutate(site_type=ifelse(site_type=="Control Site", "Control", "MPA")) %>% 
#   # Add site meta data
#   left_join(sites_in_data, by=c("site_id"="site_id")) %>% 
#   # Drop geometry
#   sf::st_drop_geometry()
# 
# 
# # Inspect
# table(sites$site_type)
# 
# 
# 
