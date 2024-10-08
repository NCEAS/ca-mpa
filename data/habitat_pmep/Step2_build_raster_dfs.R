# Summarize habitat rasters into single dataframe
# Cori Lopazanski
# February 2024


# Setup --------------------------------------------------------------------------------
# Packages
library(tidyverse)
library(sf)
library(terra)

# Directories
sync.dir <- "/home/shares/ca-mpa/data/sync-data"
sub.dir  <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/substrate/substrate_rasters"
bio.dir  <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/biotic/biotic_rasters"
pro.dir  <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed"


# Create raster for depth zones -------------------------------------------------------
zones_ca <- read_sf(dsn = file.path(sync.dir, "habitat_pmep/PMEP_Nearshore_Zones_and_Habitat.gdb"), 
                    layer = 'West_Coast_USA_Nearshore_Zones') %>% 
  st_zm() %>% # convert to xy
  filter(State == "CA") %>% 
  mutate(PMEP_Zone = as.numeric(PMEP_Zone))

raster <- raster::raster(zones_ca, resolution = 24, crs = st_crs(zones_ca))

zones_raster <- fasterize::fasterize(zones_ca, raster, field = "PMEP_Zone")

zones_df <- as.data.frame(zones_raster, xy = T)

zones_df <- zones_df %>% 
  filter(!is.na(layer)) %>% 
  rename(zone = layer)

saveRDS(zones_df, file.path("/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/combined", "PMEP_Zones_24mRes_Full.Rds"))

# Create raster for MPAs -------------------------------------------------------------
raster <- raster::raster(zones_ca, resolution = 24, crs = st_crs(zones_ca))

mpas <- readRDS(file.path(sync.dir, "/gis_data/processed/CA_MPA_polygons.Rds")) %>% 
  st_transform(., st_crs(zones_ca)) %>%   # transform to match CRS of substrate data
  mutate(name = as.factor(name)) 

mpa_raster <- fasterize::fasterize(mpas, raster, field = "name")

mpa_df <- as.data.frame(mpa_raster, xy = T)

mpa_match <- mpas %>% 
  select(name) %>% 
  mutate(layer = as.integer(name))

mpa_df <- mpa_df %>% 
  left_join(mpa_match) %>%
  select(x, y, mpa_name = name)

rm(mpa_match, mpas)

saveRDS(mpa_df, file.path("/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/combined", "MPAs_24mRes_Full.Rds"))

# Combine and export dataframe for each section --------------------------------------

# SUBSTRATE ---------------

combine_substrate <- function(section_input, section_num){
  rock <- rast(file.path(sub.dir, paste("PMEP_Substrate_1.1_24mRes_Section", section_input, ".tif", sep = "")))
  names(rock) <- "rock_substrate"
  
  soft <- rast(file.path(sub.dir, paste("PMEP_Substrate_1.2_24mRes_Section", section_input, ".tif", sep = "")))
  names(soft) <- "unconsolidated_substrate"
  
  coar <- rast(file.path(sub.dir, paste("PMEP_Substrate_1.2.1_24mRes_Section", section_input, ".tif", sep = "")))
  names(coar) <- "coarse_unconsolidated_substrate"
  
  fine <- rast(file.path(sub.dir, paste("PMEP_Substrate_1.2.2_24mRes_Section", section_input, ".tif", sep = "")))
  names(fine) <- "fine_unconsolidated_substrate"
  
  layers <- c(rock, soft, coar, fine)
  
  layers_df <- as.data.frame(layers, xy = T) 
  
  layers_df <- layers_df %>% 
    mutate(section = section_num) %>% 
    left_join(., zones_df)
  
  saveRDS(layers_df, file.path(sub.dir, paste("PMEP_Substrate_24mRes_Section", section_input, ".Rds", sep = "")))
}

combine_substrate(section_input = "30", section = 30)
combine_substrate(section_input = "31", section = 31)
combine_substrate(section_input = "33", section = 33)
combine_substrate(section_input = "40", section = 40)
combine_substrate(section_input = "41", section = 41)

## Section 23 ------
rock <- rast(file.path(sub.dir, "PMEP_Substrate_1.1_24mRes_Section23.tif"))
names(rock) <- "rock_substrate"

soft <- rast(file.path(sub.dir, "PMEP_Substrate_1.2_24mRes_Section23.tif")) 
names(soft) <- "unconsolidated_substrate"

fine <- rast(file.path(sub.dir, "PMEP_Substrate_1.2.2_24mRes_Section23.tif")) 
names(fine) <- "fine_unconsolidated_substrate"

layers <- c(rock, soft, fine)
layers_df <- as.data.frame(layers, xy = T)

layers_df <- layers_df %>% 
  mutate(coarse_unconsolidated_substrate = NA)

layers_df <- layers_df %>% 
  select(x, y,
         rock_substrate, 
         unconsolidated_substrate,
         coarse_unconsolidated_substrate,
         fine_unconsolidated_substrate) %>% 
  mutate(section = 23) %>% 
  left_join(., zones_df)

saveRDS(layers_df, file.path(sub.dir, "PMEP_Substrate_24mRes_Section23.Rds"))



## Section 32 ------
rock <- rast(file.path(sub.dir, "PMEP_Substrate_1.1_24mRes_Section32.tif"))
names(rock) <- "rock_substrate"

soft <- rast(file.path(sub.dir, "PMEP_Substrate_1.2_24mRes_Section32.tif")) 
names(soft) <- "unconsolidated_substrate"

layers <- c(rock, soft)
layers_df <- as.data.frame(layers, xy = T)

layers_df <- layers_df %>% 
  mutate(coarse_unconsolidated_substrate = NA,
         fine_unconsolidated_substrate = NA) %>% 
  mutate(section = 32) %>% 
  left_join(., zones_df)

saveRDS(layers_df, file.path(sub.dir, "PMEP_Substrate_24mRes_Section32.Rds"))


# BIOTIC ------------------------------------------------------------------

combine_biotic <- function(section_input, section_num){
  aqveg <- rast(file.path(bio.dir, paste("PMEP_Biotic_AquaticVegetationBed_24mRes_Section", section_input, ".tif", sep = "")))
  names(aqveg) <- "aquatic_vegetation_bed"
  
  faun <- rast(file.path(bio.dir, paste("PMEP_Biotic_FaunalBed_24mRes_Section", section_input, ".tif", sep = "")))
  names(faun) <- "faunal_bed"
  
  kelp <- rast(file.path(bio.dir, paste("PMEP_Biotic_Kelp_24mRes_Section", section_input, ".tif", sep = "")))
  names(kelp) <- "kelp"
  
  layers <- c(aqveg, kelp, faun)
  
  layers_df <- as.data.frame(layers, xy = T) %>% 
    mutate(section = section_num) %>% 
    left_join(., zones_df)
  
  saveRDS(layers_df, file.path(bio.dir, paste("PMEP_Biotic_24mRes_Section", section_input, ".Rds", sep = "")))
  
}

combine_biotic(section_input = "30", section_num = 30)
combine_biotic(section_input = "31", section_num = 31)
combine_biotic(section_input = "32", section_num = 32)
combine_biotic(section_input = "33", section_num = 33)
combine_biotic(section_input = "40", section_num = 40)
combine_biotic(section_input = "41", section_num = 41)


## Section 23 ------
aqveg <- rast(file.path(bio.dir, "PMEP_Biotic_AquaticVegetationBed_24mRes_Section23.tif"))
names(aqveg) <- "aquatic_vegetation_bed"

kelp <- rast(file.path(bio.dir, "PMEP_Biotic_Kelp_24mRes_Section23.tif"))
names(kelp) <- "kelp"

layers <- c(aqveg, kelp)

layers_df <- as.data.frame(layers, xy = T) %>% 
  mutate(faunal_bed = NA) %>% 
  mutate(section = 23) %>% 
  left_join(., zones_df)

saveRDS(layers_df, file.path(bio.dir, "PMEP_Biotic_24mRes_Section23.Rds"))


# Create raster for MPAs -------------------------------------------------------------
raster <- raster::raster(zones_ca, resolution = 24, crs = st_crs(zones_ca))


mpas <- readRDS(file.path(sync.dir, "/gis_data/processed/CA_MPA_polygons.Rds")) %>% 
  st_transform(., st_crs(zones_ca)) %>%   # transform to match CRS of substrate data
  mutate(name = as.factor(name)) 

mpa_raster <- fasterize::fasterize(mpas, raster)

mpa_df <- as.data.frame(mpa_raster, xy = T)

mpa_match <- mpas %>% 
  select(name) %>% 
  mutate(layer = as.integer(name))

mpa_df <- mpa_df %>% 
  left_join(mpa_match) %>%
  select(x, y, mpa_name = name)

rm(mpa_match, mpas)

saveRDS(mpa_df, file.path("/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/combined", "MPAs_24mRes_Full.Rds"))

crs(mpas)
crs(mpa_raster)

library(tmap)
tmap_mode("view")
tm_shape(mpa_raster) +
  tm_raster() +
tm_shape(mpas) +
  tm_borders(col = "black") 


# Combine biotic + substrate + MPA by section ---------------------------------------

combine_rasters <- function(section){
  # Read sub and bio section dataframes
  sub <- readRDS(file.path(sub.dir, paste("PMEP_Substrate_24mRes_Section", section, ".Rds", sep = "")))
  bio <- readRDS(file.path(bio.dir, paste("PMEP_Biotic_24mRes_Section", section, ".Rds", sep = "")))
  
  # Combine
  combined <- full_join(sub, bio) %>% 
    left_join(., mpa) %>% 
    mutate(mpa_binary = if_else(is.na(mpa_name), NA, 1)) %>%  # create binary MPA presence column
    select(x, y, section, mpa_binary, mpa_name, everything())
  
  # Save RDS to combined folder
  saveRDS(combined, file.path(pro.dir, "combined",
                              paste("PMEP_Combined_24mRes_Section", section, ".Rds", sep = "")))
}

combine_rasters(section = "23")
combine_rasters(section = "30")
combine_rasters(section = "31")
combine_rasters(section = "32")
combine_rasters(section = "33")
combine_rasters(section = "40")
combine_rasters(section = "41")

