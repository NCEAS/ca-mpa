

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(rinat)
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data/"
indir <- file.path(basedir, "ebird/intermediate_clip")
outdir <- file.path(basedir, "ebird/processed")
gisdir <- file.path(basedir, "gis_data/processed")
plotdir <- "data/ebird/figures"

# Build data
################################################################################

# Files to merge
files2merge <- list.files(indir)

# Loop through files
x <- files2merge[2]
data <- purrr::map_df(files2merge, function(x){
  
  # Read data
  fdata <- readRDS(file.path(indir, x)) %>% 
    select(record_id, survey_id, survey_date, observer_id, lat_dd, long_dd)
  
})

# Export data
saveRDS(data, file=file.path(outdir, "CA_ebird_data_inside_state_water_buffer.Rds"))




