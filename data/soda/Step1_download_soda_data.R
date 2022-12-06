
# Clear workspace
rm(list = ls())

# Read data
################################################################################

# Packages
library(ncdf4)
library(tidyverse)

# Define directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data/"
indir <- file.path(basedir, "soda/raw")
outdir <- file.path(basedir, "soda/processed")
plotdir <- "data/soda/figures"

# DOWNLOAD DATA
################################################################################


# THREDDS server
# http://apdrc.soest.hawaii.edu/thredds/dodsC/las/soda_pop2.2.4/catalog.html?dataset=las/soda_pop2.2.4/data_apdrc.soest.hawaii.edu_dods_public_data_SODA_soda_pop2.2.4.jnl
url <- "http://apdrc.soest.hawaii.edu/thredds/wms/las/soda_pop2.2.4/data_apdrc.soest.hawaii.edu_dods_public_data_SODA_soda_pop2.2.4.jnl?service=WMS&version=1.3.0&request=GetCapabilities"
nc_open(url)

# Build data to loop through
# http://dsrs.atmos.umd.edu/DATA/soda_2.2.4/SODA_2.2.4_187101.cdf
data <- data.frame(year=as.character(sort(rep(1871:2010,12))), 
                   month=as.character(rep(1:12, length(1871:2010))), done=NA)
data$month <- str_pad(data$month, 2, pad="0")

# Loop through data
for(i in 1:nrow(data)){
  
  # Build URL
  year <- data$year[i]
  month <- data$month[i]
  filename <- paste("SODA_2.2.4_", year, month, ".cdf", sep="")
  url <- paste("http://dsrs.atmos.umd.edu/DATA/soda_2.2.4/", filename, sep="")
  
  # Download file at URL
  download.file(url, paste(sodadir, filename, sep="/"), method="auto",
                quiet=T, mode="wb", cacheOK = TRUE)
  
  # Record data as downloaded
  data$done[i] <- "yes"
  print(i)
  
}




