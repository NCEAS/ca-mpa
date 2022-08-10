

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
inputdir <- file.path(basedir, "monitoring")

# Read monitoring sites
load(file.path(inputdir, "site_locations.Rda"))

# Read MPA traits
mpas <- readRDS(file=file.path(basedir, "mpa_traits/processed/CA_mpa_metadata.Rds"))


# Format data
################################################################################

# Format sites
sites <- site_locations %>% 
  # Rename
  rename(long_dd=lon, lat_dd=lat, habitat=group, site_type=mpa_designation, mpa=affiliated_mpa) %>% 
  # Format lat/long
  mutate(long_dd=as.numeric(long_dd)) %>% 
  # Format habitat
  mutate(habitat=recode(habitat, 
                        "ccfrp"="Rocky reef",
                        "deep_reef"="Deep reef",
                        "kelp"="Kelp",
                        "Rocky intertidal"="rocky",
                        "surf-zone"="Surf zone")) %>% 
  # Format site type
  mutate(site_type=ifelse(site_type=="ref", "Reference", "MPA")) %>% 
  # Format MPA name
  mutate(mpa=stringr::str_to_title(mpa),
         mpa=gsub("Smr", "SMR", mpa),
         mpa=gsub("Smca", "SMCA", mpa),
         mpa=recode(mpa, 
                    "Ano Nuevo SMR"="Año Nuevo SMR",                   
                    "Arrow Point To Lion Head Point SMCA"="Arrow Point to Lion Head Point SMCA",
                    "Blue Cavern Onshore SMCA"="Blue Cavern Onshore SMCA (No-Take)",          
                    "Bodega Head "="Bodega Head SMR/SMCA", # is this right? both SMR/SMCA or one?              
                    "Campus Point SMCA"="Campus Point SMCA (No-Take)",                 
                    "Cape Mendocino SMCA"="South Cape Mendocino SMR",  # Is this correct?              
                    "Laguna Beach SMCA"="Laguna Beach SMCA (No-Take)",                  
                    "Mackerricher SMCA"="MacKerricher SMCA",                
                    "N Farallon Islands SMR"="North Farallon Islands SMR",            
                    "N/A"="",                           
                    "None"="",                             
                    "Piedras Blancas SMCA/ Smcr"="Piedras Blancas SMR/SMCA",     
                    "Point Arena "="Point Arena SMR/SMCA", # is this right? both SMR/SMCA or one?                       
                    "Point Vicente SMCA"="Point Vicente SMCA (No-Take)",                
                    "Soquel Canyon SMR"="Soquel Canyon SMCA",                 
                    "Swamis SMCA"="Swami's SMCA"),
         mpa=ifelse(mpa=="", NA, mpa))   %>%                 
                    # "Trinidad SMR"="")) %>% 
  # Fill in missing site names
  mutate(site_temp=make.unique(paste(mpa, site_type, sep="-"), sep="-")) %>% 
  mutate(site=ifelse(is.na(site), site_temp, site)) %>% 
  # Remove unimportant columns
  select(-c(mpa_class, site_temp))
  

# Inspect MPA names
mpas_in_data <- sort(unique(sites$mpa))  
mpas_in_data[!mpas_in_data %in% mpas$mpa]

# Inspect
freeR::complete(sites)

# Are site ids unique?
freeR::which_duplicated(sites$site)

# Export data
saveRDS(sites, file.path(inputdir, "monitoring_sites_clean.Rds"))
