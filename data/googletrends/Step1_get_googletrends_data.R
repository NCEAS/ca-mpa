

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(gtrendsR)
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
outdir <- file.path(basedir, "googletrends/processed")
plotdir <- "googletrends/figures"

# Read MPA data
mpas <- readRDS(file.path(basedir, "mpa_traits/processed/CA_mpa_metadata.Rds"))

# Format data
################################################################################

# Loop through MPAs
x <- "Point Lobos"
for(i in 149:150){
  
  # Get data
  print(i)
  mpa_do <- mpas$mpa_full[i]
  mdata_list <- try(gtrendsR::gtrends(keyword=mpa_do, geo="", time="all", gprop="web", low_search_volume=T))
  
  # Extract data of interest
  if(inherits(mdata_list, "try-error")){
    mdata <- tibble(date=NA,
                    hits=NA, 
                    keyword=i, 
                    geo=NA, 
                    time=NA,
                    gpop=NA, 
                    category=NA)
  }else{
    mdata <- mdata_list$interest_over_time
  }
  
  # Merge
  if(i==149){mdata_out <- mdata}else{rbind(mdata_out, mdata)}
  
}

# Format data
data <- mdata_out %>% 
  # Reduce
  filter(!is.na(hits)) %>% 
  # Rename
  rename(mpa_full=keyword) %>% 
  # Add region
  left_join(mpas %>% select(mpa_full, mpa, region, type)) %>% 
  # Arrange
  select(region, mpa, mpa_full, type, date, hits) %>% 
  arrange(region, mpa, date)

# Plot data
g <- ggplot(data, aes(x=date))




