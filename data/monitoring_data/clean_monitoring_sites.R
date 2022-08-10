

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

# Step 1. Format data initially
#######################################

# Format sites
sites1 <- site_locations %>% 
  # Rename
  rename(long_dd=lon, lat_dd=lat, habitat=group, site_type=mpa_designation, mpa_orig=affiliated_mpa) %>% 
  # Format lat/long
  mutate(long_dd=as.numeric(long_dd)) %>% 
  # Format habitat
  mutate(habitat=recode(habitat, 
                        "ccfrp"="Rocky reef",
                        "deep_reef"="Deep reef",
                        "kelp"="Kelp",
                        "rocky"="Rocky intertidal",
                        "surf-zone"="Surf zone")) %>% 
  # Format site type
  mutate(site_type=ifelse(site_type=="ref", "Reference", "MPA")) %>% 
  # Format MPA name
  mutate(mpa=stringr::str_to_title(mpa_orig),
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
                    # "Trinidad SMR"=""
                    "Swamis SMCA"="Swami's SMCA"),
         mpa=ifelse(mpa=="", NA, mpa)) %>%
  # Arrange
  select(-mpa_class) %>% 
  select(habitat, mpa, mpa_orig, site, site_type, everything())

# Inspect MPA names
mpas_in_data <- sort(unique(sites1$mpa))  
mpas_in_data[!mpas_in_data %in% mpas$mpa]

# Inspect
freeR::complete(sites1)

# Are site ids unique?
freeR::which_duplicated(sites1$site)


# Step 2. Format deep reef and other
#######################################

# Format deep reef
sites_deep <- sites1 %>% 
  # Reduce to deep reef
  filter(habitat=="Deep reef") %>%
  # Average by site
  group_by(habitat, mpa, mpa_orig, site_type) %>% 
  summarize(lat_dd=mean(lat_dd),
            long_dd=mean(long_dd)) %>% 
  ungroup() %>% 
  # Add site name
  mutate(site=paste(habitat, mpa, site_type, sep="-")) %>% 
  # Arrange
  select(habitat, mpa, mpa_orig, site, site_type, everything())

# Format other
sites_other <- sites1 %>% 
  # Reduce to deep reef
  filter(habitat!="Deep reef") %>% 
  # Fill in  missing site names
  mutate(site_temp=make.unique(paste(habitat, mpa, site_type, sep="-"), sep="-")) %>%
  mutate(site=ifelse(is.na(site), site_temp, site))


# Step 3. Merge and format again
#######################################

# Merge
sites <- bind_rows(sites_other, sites_deep) %>% 
  # Average coordinates of duplicated sites: "LECHUZA", "POINT_ARENA_REFERENCE_3"
  group_by(habitat, mpa, mpa_orig, site, site_type) %>% 
  summarize(lat_dd=mean(lat_dd),
            long_dd=mean(long_dd)) %>% 
  ungroup()
  
# Inspect
freeR::complete(sites)

# Are site ids unique?
freeR::which_duplicated(sites$site)

# Export data
saveRDS(sites, file.path(inputdir, "monitoring_sites_clean.Rds"))





