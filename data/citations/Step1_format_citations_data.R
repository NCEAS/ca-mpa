
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data/"
indir <- file.path(basedir, "citations/raw")
outdir <- file.path(basedir, "citations/processed")
plotdir <- "data/citations/figures"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "NCEAS_CitationDataRequest_7.26.22.xlsx"))

# Read MPA traits
mpas <- readRDS(file.path(basedir, "mpa_traits/processed/CA_mpa_metadata.Rds"))

# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(mpa=mpa_name, region=bioregion) %>% 
  # Remove totals
  select(-c(grand_total)) %>% 
  filter(mpa!="Grand Total") %>% 
  # Gather
  gather(key="year", value="ncitations", 3:ncol(.)) %>% 
  # Format
  mutate(year=gsub("x", "", year) %>% as.numeric()) %>% 
  # Fix MPA names
  mutate(mpa=recode(mpa, 
                    "Blue Caverns Offshore SMCA"="Blue Cavern Offshore SMCA",                              
                    "Blue Caverns Onshore SMCA (No-Take)"="Blue Cavern Onshore SMCA (No-Take)",                      
                    # As indicated by Amanda Van Diggelen
                    "Bolsa Chica MPA - Unidentified"="Bolsa Chica MPA - unidentified",
                    "Egg (Devil's Slide Rock) to Devil's Slide Special Closure"="Egg (Devil's Slide) Rock to Devil's Slide Special Closure", 
                    # Confirmed by Amanda Van Diggelen
                    "Farnsworth SMCA"="Farnsworth SMCA - unidentified",                                         
                    "Goelta Slough SMCA (No-Take)"="Goleta Slough SMCA (No-Take)",                             
                    "Lover's Point - Julia Platt SMR"="Lovers Point - Julia Platt SMR",
                    # Confirmed by Amanda Van Diggelen
                    "Ten Mile SMCA"="Ten Mile SMR")) %>% 
  # Add region
  select(-region) %>% 
  left_join(mpas %>% select(mpa, region, type), by="mpa") %>% 
  mutate(region=as.character(region),
         region=ifelse(mpa%in% c("Bolsa Chica MPA - unidentified", "Farnsworth SMCA - unidentified"), "South Coast", region)) %>% 
  # Add zeros for NA values
  mutate(ncitations=ifelse(is.na(ncitations), 0, ncitations)) %>% 
  # Arrange
  select(region, mpa, type, everything())

# Inspect
table(data$region)
freeR::complete(data)

# Check names
mpas_in_data <- sort(unique(data$mpa))
mpas_in_data[!mpas_in_data %in% mpas$mpa]

# Export
saveRDS(data, file=file.path(outdir, "2016_2021_citations.Rds"))





