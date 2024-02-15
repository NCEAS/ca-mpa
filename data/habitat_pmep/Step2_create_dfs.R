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

# Combine and export dataframe for each section --------------------------------------

# SUBSTRATE ---------------

## Section 23 ------
rock <- rast(file.path(sub.dir, "PMEP_Substrate_1.1_24mRes_Section23.tif"))
names(rock) <- "1.1 Rock Substrate"

soft <- rast(file.path(sub.dir, "PMEP_Substrate_1.2_24mRes_Section23.tif")) 
names(soft) <- "1.2 Unconsolidated Mineral Substrate"

fine <- rast(file.path(sub.dir, "PMEP_Substrate_1.2.2_24mRes_Section23.tif")) 
names(fine) <- "1.2.2 Fine Unconsolidated Substrate"

layers <- c(rock, soft, fine)
layers_df <- as.data.frame(layers, xy = T)

layers_df <- layers_df %>% 
  mutate(`1.2.1 Coarse Unconsolidated Substrate` = NA)

layers_df <- layers_df %>% 
  select(x, y, 
         `1.1 Rock Substrate`, 
         `1.2 Unconsolidated Mineral Substrate`,
         `1.2.1 Coarse Unconsolidated Substrate`,
         `1.2.2 Fine Unconsolidated Substrate`) %>% 
  mutate(section = 23) %>% 
  left_join(., zones_df)

saveRDS(layers_df, file.path(sub.dir, "PMEP_Substrate_24mRes_Section23.Rds"))


## Section 30 ------
rock <- rast(file.path(sub.dir, "PMEP_Substrate_1.1_24mRes_Section30.tif"))
names(rock) <- "1.1 Rock Substrate"

soft <- rast(file.path(sub.dir, "PMEP_Substrate_1.2_24mRes_Section30.tif")) 
names(soft) <- "1.2 Unconsolidated Mineral Substrate"

coar <- rast(file.path(sub.dir, "PMEP_Substrate_1.2.1_24mRes_Section30.tif")) 
names(coar) <- "1.2.1 Coarse Unconsolidated Substrate"

fine <- rast(file.path(sub.dir, "PMEP_Substrate_1.2.2_24mRes_Section30.tif")) 
names(fine) <- "1.2.2 Fine Unconsolidated Substrate"

layers <- c(rock, soft, coar, fine)
layers_df <- as.data.frame(layers, xy = T) %>% 
  mutate(section = 30) %>% 
  left_join(., zones_df)

saveRDS(layers_df, file.path(sub.dir, "PMEP_Substrate_24mRes_Section30.Rds"))


## Section 31 ------
rock <- rast(file.path(sub.dir, "PMEP_Substrate_1.1_24mRes_Section31.tif"))
names(rock) <- "1.1 Rock Substrate"

soft <- rast(file.path(sub.dir, "PMEP_Substrate_1.2_24mRes_Section31.tif")) 
names(soft) <- "1.2 Unconsolidated Mineral Substrate"

coar <- rast(file.path(sub.dir, "PMEP_Substrate_1.2.1_24mRes_Section31.tif")) 
names(coar) <- "1.2.1 Coarse Unconsolidated Substrate"

fine <- rast(file.path(sub.dir, "PMEP_Substrate_1.2.2_24mRes_Section31.tif")) 
names(fine) <- "1.2.2 Fine Unconsolidated Substrate"

layers <- c(rock, soft, coar, fine)
layers_df <- as.data.frame(layers, xy = T) %>% 
  mutate(section = 31) %>% 
  left_join(., zones_df)

saveRDS(layers_df, file.path(sub.dir, "PMEP_Substrate_24mRes_Section31.Rds"))

## Section 32 ------
rock <- rast(file.path(sub.dir, "PMEP_Substrate_1.1_24mRes_Section32.tif"))
names(rock) <- "1.1 Rock Substrate"

soft <- rast(file.path(sub.dir, "PMEP_Substrate_1.2_24mRes_Section32.tif")) 
names(soft) <- "1.2 Unconsolidated Mineral Substrate"

layers <- c(rock, soft)
layers_df <- as.data.frame(layers, xy = T)

layers_df <- layers_df %>% 
  mutate(`1.2.1 Coarse Unconsolidated Substrate` = NA,
         `1.2.2 Fine Unconsolidated Substrate` = NA) %>% 
  mutate(section = 32) %>% 
  left_join(., zones_df)

saveRDS(layers_df, file.path(sub.dir, "PMEP_Substrate_24mRes_Section32.Rds"))

## Section 33 ------
rock <- rast(file.path(sub.dir, "PMEP_Substrate_1.1_24mRes_Section33.tif"))
names(rock) <- "1.1 Rock Substrate"

soft <- rast(file.path(sub.dir, "PMEP_Substrate_1.2_24mRes_Section33.tif")) 
names(soft) <- "1.2 Unconsolidated Mineral Substrate"

coar <- rast(file.path(sub.dir, "PMEP_Substrate_1.2.1_24mRes_Section33.tif")) 
names(coar) <- "1.2.1 Coarse Unconsolidated Substrate"

fine <- rast(file.path(sub.dir, "PMEP_Substrate_1.2.2_24mRes_Section33.tif")) 
names(fine) <- "1.2.2 Fine Unconsolidated Substrate"

layers <- c(rock, soft, coar, fine)
layers_df <- as.data.frame(layers, xy = T) %>% 
  mutate(section = 33) %>% 
  left_join(., zones_df)

saveRDS(layers_df, file.path(sub.dir, "PMEP_Substrate_24mRes_Section33.Rds"))


## Section 40 ------
rock <- rast(file.path(sub.dir, "PMEP_Substrate_1.1_24mRes_Section40.tif"))
names(rock) <- "1.1 Rock Substrate"

soft <- rast(file.path(sub.dir, "PMEP_Substrate_1.2_24mRes_Section40.tif")) 
names(soft) <- "1.2 Unconsolidated Mineral Substrate"

coar <- rast(file.path(sub.dir, "PMEP_Substrate_1.2.1_24mRes_Section40.tif")) 
names(coar) <- "1.2.1 Coarse Unconsolidated Substrate"

fine <- rast(file.path(sub.dir, "PMEP_Substrate_1.2.2_24mRes_Section40.tif")) 
names(fine) <- "1.2.2 Fine Unconsolidated Substrate"

layers <- c(rock, soft, coar, fine)
layers_df <- as.data.frame(layers, xy = T) %>% 
  mutate(section = 40) %>% 
  left_join(., zones_df)

saveRDS(layers_df, file.path(sub.dir, "PMEP_Substrate_24mRes_Section40.Rds"))


## Section 41 ------
rock <- rast(file.path(sub.dir, "PMEP_Substrate_1.1_24mRes_Section41.tif"))
names(rock) <- "1.1 Rock Substrate"

soft <- rast(file.path(sub.dir, "PMEP_Substrate_1.2_24mRes_Section41.tif")) 
names(soft) <- "1.2 Unconsolidated Mineral Substrate"

coar <- rast(file.path(sub.dir, "PMEP_Substrate_1.2.1_24mRes_Section41.tif")) 
names(coar) <- "1.2.1 Coarse Unconsolidated Substrate"

fine <- rast(file.path(sub.dir, "PMEP_Substrate_1.2.2_24mRes_Section41.tif")) 
names(fine) <- "1.2.2 Fine Unconsolidated Substrate"

layers <- c(rock, soft, coar, fine)
layers_df <- as.data.frame(layers, xy = T) %>% 
  mutate(section = 41) %>% 
  left_join(., zones_df)

saveRDS(layers_df, file.path(sub.dir, "PMEP_Substrate_24mRes_Section41.Rds"))

# BIOTIC ------------------------------------------------------------------

## Section 23 ------
aqveg <- rast(file.path(bio.dir, "PMEP_Biotic_AquaticVegetationBed_24mRes_Section23.tif"))
names(aqveg) <- "aquatic_vegetation_bed"

#faun <- rast(file.path(bio.dir, "PMEP_Biotic_FaunalBed_24mRes_Section23.tif"))

kelp <- rast(file.path(bio.dir, "PMEP_Biotic_Kelp_24mRes_Section23.tif"))
names(kelp) <- "kelp"

layers <- c(aqveg, kelp)

layers_df <- as.data.frame(layers, xy = T) %>% 
  mutate(faunal_bed = NA) %>% 
  mutate(section = 23) %>% 
  left_join(., zones_df)

saveRDS(layers_df, file.path(bio.dir, "PMEP_Biotic_24mRes_Section23.tif"))


## Section 30 ------
aqveg <- rast(file.path(bio.dir, "PMEP_Biotic_AquaticVegetationBed_24mRes_Section30.tif"))
names(aqveg) <- "aquatic_vegetation_bed"

faun <- rast(file.path(bio.dir, "PMEP_Biotic_FaunalBed_24mRes_Section30.tif"))
names(faun) <- "faunal_bed"

kelp <- rast(file.path(bio.dir, "PMEP_Biotic_Kelp_24mRes_Section30.tif"))
names(kelp) <- "kelp"

layers <- c(aqveg, kelp, faun)

layers_df <- as.data.frame(layers, xy = T) %>% 
  mutate(section = 30) %>% 
  left_join(., zones_df)

saveRDS(layers_df, file.path(bio.dir, "PMEP_Biotic_24mRes_Section30.Rds"))


## Section 31 ------
aqveg <- rast(file.path(bio.dir, "PMEP_Biotic_AquaticVegetationBed_24mRes_Section31.tif"))
names(aqveg) <- "aquatic_vegetation_bed"

faun <- rast(file.path(bio.dir, "PMEP_Biotic_FaunalBed_24mRes_Section31.tif"))
names(faun) <- "faunal_bed"

kelp <- rast(file.path(bio.dir, "PMEP_Biotic_Kelp_24mRes_Section31.tif"))
names(kelp) <- "kelp"

layers <- c(aqveg, kelp, faun)

layers_df <- as.data.frame(layers, xy = T) %>% 
  mutate(section = 31) %>% 
  left_join(., zones_df)

saveRDS(layers_df, file.path(bio.dir, "PMEP_Biotic_24mRes_Section31.Rds"))

## Section 32 ------
aqveg <- rast(file.path(bio.dir, "PMEP_Biotic_AquaticVegetationBed_24mRes_Section32.tif"))
names(aqveg) <- "aquatic_vegetation_bed"

faun <- rast(file.path(bio.dir, "PMEP_Biotic_FaunalBed_24mRes_Section32.tif"))
names(faun) <- "faunal_bed"

kelp <- rast(file.path(bio.dir, "PMEP_Biotic_Kelp_24mRes_Section32.tif"))
names(kelp) <- "kelp"

layers <- c(aqveg, kelp, faun)

layers_df <- as.data.frame(layers, xy = T) %>% 
  mutate(section = 32) %>% 
  left_join(., zones_df)

saveRDS(layers_df, file.path(bio.dir, "PMEP_Biotic_24mRes_Section32.Rds"))


## Section 33 ------
aqveg <- rast(file.path(bio.dir, "PMEP_Biotic_AquaticVegetationBed_24mRes_Section33.tif"))
names(aqveg) <- "aquatic_vegetation_bed"

faun <- rast(file.path(bio.dir, "PMEP_Biotic_FaunalBed_24mRes_Section33.tif"))
names(faun) <- "faunal_bed"

kelp <- rast(file.path(bio.dir, "PMEP_Biotic_Kelp_24mRes_Section33.tif"))
names(kelp) <- "kelp"

layers <- c(aqveg, kelp, faun)

layers_df <- as.data.frame(layers, xy = T) %>% 
  mutate(section = 33) %>% 
  left_join(., zones_df)

saveRDS(layers_df, file.path(bio.dir, "PMEP_Biotic_24mRes_Section33.Rds"))

## Section 40 ------
aqveg <- rast(file.path(bio.dir, "PMEP_Biotic_AquaticVegetationBed_24mRes_Section40.tif"))
names(aqveg) <- "aquatic_vegetation_bed"

faun <- rast(file.path(bio.dir, "PMEP_Biotic_FaunalBed_24mRes_Section40.tif"))
names(faun) <- "faunal_bed"

kelp <- rast(file.path(bio.dir, "PMEP_Biotic_Kelp_24mRes_Section40.tif"))
names(kelp) <- "kelp"

layers <- c(aqveg, kelp, faun)

layers_df <- as.data.frame(layers, xy = T) %>% 
  mutate(section = 40) %>% 
  left_join(., zones_df)

saveRDS(layers_df, file.path(bio.dir, "PMEP_Biotic_24mRes_Section40.Rds"))

## Section 41 ------
aqveg <- rast(file.path(bio.dir, "PMEP_Biotic_AquaticVegetationBed_24mRes_Section41.tif"))
names(aqveg) <- "aquatic_vegetation_bed"

faun <- rast(file.path(bio.dir, "PMEP_Biotic_FaunalBed_24mRes_Section41.tif"))
names(faun) <- "faunal_bed"

kelp <- rast(file.path(bio.dir, "PMEP_Biotic_Kelp_24mRes_Section41.tif"))
names(kelp) <- "kelp"

layers <- c(aqveg, kelp, faun)

layers_df <- as.data.frame(layers, xy = T) %>% 
  mutate(section = 41) %>% 
  left_join(., zones_df)

saveRDS(layers_df, file.path(bio.dir, "PMEP_Biotic_24mRes_Section41.Rds"))

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
