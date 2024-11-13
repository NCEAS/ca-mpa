# Export the habitat data within 1000m of the monitoring sites
# Cori Lopazanski
# June 2024


# Setup   ----------------------------------------------------------------------
library(tidyverse)
library(sf)

bio.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/biotic"
sub.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/substrate"
gdb.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Nearshore_Zones_and_Habitat.gdb" 
com.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/combined"
sim.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/habitat_sim"


# Read substrate attributes and CRS --------------------------------------------
att <- readRDS(file.path(sub.dir, "West_Coast_USA_Nearshore_CMECS_Substrate_Habitat_Attributes.Rds")) %>% 
  filter(State == "CA")

crs_info <- st_crs(st_read(file.path(sub.dir, "substrate_ca/sections/substrate_section_23.gpkg"), quiet = TRUE))


# Read ccfrp site buffers  ----------------------------------------------------
# Created here: data/monitoring_data/processing_code/archive/clean_monitoring_sites.R
sites <- readRDS("/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/habitat_sim/ccfrp_site_buffers.Rds")

# Transform sites
sites <- st_transform(sites, crs = crs_info)

# Merge sites
sites_merged <- st_union(sites)

# Intersect merged site footprint and substrate --------------------------------

# For substrate:
intersect_sections <- function(section){
  print(paste0("Section: ", section))
  sect <- st_read(file.path(sub.dir, "substrate_ca/sections", paste0("substrate_section_", section, ".gpkg")))
  print("Read complete")
  section_intersect <- st_intersection(sect, sites_merged)
  print("Intersection complete")
  saveRDS(section_intersect, file.path(sim.dir, "substrate_intersect", paste0("substrate_intersect_", section, ".Rds")))
  print("Save complete")
}

sections <- unique(att$PMEP_Section)
sections <- c("23", "30", "31", "32", "33", "53", "40", "41")
# 50 & 52 has error, multisurface geometry most likely

lapply(sections, intersect_sections)

intersect_sections(section = '23')

# Find intersections by site + export ------------------------------------------

intersect_by_site <- function(section){
  print(paste0("Section: ", section))
  sub <- readRDS(file.path(sim.dir,"substrate_intersect", paste0("substrate_intersect_", section, ".Rds")))
  print("Read section complete.")
  sub_sites <- st_intersection(sub, sites)
  print("Intersection complete.")
  sub_sites$area <- st_area(sub_sites)
  print("Area calculation complete.")
  saveRDS(sub_sites, file.path(sim.dir, paste0("substrate_mpas/substrate_mpas_section_", section, ".Rds")))
}


sections <- c("23", "30", "31", "32", "33", "53", "40", "41")
lapply(sections, intersect_by_site)
intersect_by_site(section = '41')


# Clean details by site + export ------------------------------------------
site_columns <- c("mpa", "PMEP_Section", "PMEP_Zone")

section <- "31"

clean_substrate <- function(section){
  substrate <- readRDS(file.path(sim.dir, paste0("substrate_mpas/substrate_mpas_section_", section, ".Rds"))) %>% 
    rename(geometry = Shape) %>% 
    filter(!CMECS_SC_Category_Code == "9.9.9.9.9") %>% 
    group_by(across(all_of(site_columns)), CMECS_SC_Category_Code, CMECS_SC_Category, CMECS_SC_Code, CMECS_SC_Name) %>% 
    summarize(geometry = st_union(geometry), .groups = 'drop') 
  
  saveRDS(substrate,  file.path(sim.dir, paste0("substrate_clean/substrate_clean_", section, ".Rds")))
}

sections <- c("23", "30", "31", "32", "33", "53", "40", "41")
lapply(sections, clean_substrate)

