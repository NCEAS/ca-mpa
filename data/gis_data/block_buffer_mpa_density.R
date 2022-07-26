## Calculate MPA Density 
## Cori Lopazanski; June 13 2022

# Setup ------------------------------------------------------------------------

## Clear workspace
rm(list = ls())

##Packages
library(tidyverse)

## Directories
base.dir <- "/Volumes/GoogleDrive-105151121202188525604/Shared drives/NCEAS MPA network assessment/MPA Network Assessment: Working Group Shared Folder/data/sync-data" # Cori Local
gis.dir <- file.path(base.dir, "gis_data/processed")
out.dir <- file.path(gis.dir, "CA_block_polygons_buffers")
plot.dir <- "data/gis_data/figures"

  
## Read Data --------------------------------------------------------------------

### Read Block Stats
block_stats <- wcfish::blocks

### Read MPA Data
mpas <- readRDS(file.path(gis.dir, "CA_MPA_polygons.Rds"))


## Simplify --------------------------------------------------------------------

# Only look at SMRs/SMCAs and FMRs, FMCAs
types_use <- c("SMR", "SMRMA", "SMCA", "SMCA (No-Take)", "FMR", "FMCA")
mpas_use <- mpas %>% 
  filter(type %in% types_use)

# Simplify MPAs/blocks
mpas_simple <- mpas_use %>%
  select(name)

# Simplify blocks
block_stats_simple <- block_stats %>%
  select(block_id)


## Calculate MPA Densities -----------------------------------------------------

### 0.5k -----------------------------------------------------
## Read Buffer
buf <- readRDS(file.path(out.dir, "CA_block_polygons_0.5km_buffer.Rds"))

## Simplify block buffer
buf_simple <- buf %>% 
  select(block_id)

## Calculate new block area (with buffer)
buf_area <-buf_simple %>% 
  st_area() %>% 
  as.numeric() %>% 
  measurements::conv_unit(., "m2", "km2")

## Find intersections with MPAs
data1 <- sf::st_intersection(x= buf_simple, y = mpas_simple)

## Calculate MPA area
intersection_areas <- data1 %>%
  sf::st_area() %>% 
  as.numeric() %>% 
  measurements::conv_unit(., "m2", "km2")

data2 <- data1 %>%
  mutate(buffer_mpa_area_km2 = intersection_areas)

# Compute buffered block stats
data3 <- data2 %>%
  sf::st_drop_geometry() %>%
  group_by(block_id) %>%
  dplyr::summarize(buffer_mpa_n=n_distinct(name),
                   buffer_mpas=paste(name, collapse=", "),
                   buffer_mpa_km2=sum(buffer_mpa_area_km2)) %>%
  ungroup() 

# Add block stats to block dataframe
buffer_0.5k <- block_stats_simple %>% 
  sf::st_drop_geometry() %>% 
  mutate(buffer_block_area_km2 = buf_area) %>% 
  left_join(., data3)

# Export as RDS
saveRDS(buffer_0.5k, file.path(out.dir, "CA_block_polygons_0.5km_buffer_mpa_density.Rds"))


### 1k -----------------------------------------------------
## Read Buffer
buf <- readRDS(file.path(out.dir, "CA_block_polygons_1km_buffer.Rds"))

## Simplify block buffer
buf_simple <- buf %>% 
  select(block_id)

## Calculate new block area (with buffer)
buf_area <-buf_simple %>% 
  st_area() %>% 
  as.numeric() %>% 
  measurements::conv_unit(., "m2", "km2")

## Find intersections with MPAs
data1 <- sf::st_intersection(x= buf_simple, y = mpas_simple)

## Calculate MPA area
intersection_areas <- data1 %>%
  sf::st_area() %>% 
  as.numeric() %>% 
  measurements::conv_unit(., "m2", "km2")

data2 <- data1 %>%
  mutate(buffer_mpa_area_km2 = intersection_areas)

# Compute buffered block stats
data3 <- data2 %>%
  sf::st_drop_geometry() %>%
  group_by(block_id) %>%
  dplyr::summarize(buffer_mpa_n=n_distinct(name),
                   buffer_mpas=paste(name, collapse=", "),
                   buffer_mpa_km2=sum(buffer_mpa_area_km2)) %>%
  ungroup() 

# Add block stats to block dataframe
buffer_1k <- block_stats_simple %>% 
  sf::st_drop_geometry() %>% 
  mutate(buffer_block_area_km2 = buf_area) %>% 
  left_join(., data3)

# Export as RDS
saveRDS(buffer_1k, file.path(out.dir, "CA_block_polygons_1km_buffer_mpa_density.Rds"))

### 3k -----------------------------------------------------

## Read Buffer
buf <- readRDS(file.path(out.dir, "CA_block_polygons_3km_buffer.Rds"))

## Simplify block buffer
buf_simple <- buf %>% 
  select(block_id)

## Calculate new block area (with buffer)
buf_area <-buf_simple %>% 
  st_area() %>% 
  as.numeric() %>% 
  measurements::conv_unit(., "m2", "km2")

## Find intersections with MPAs
data1 <- sf::st_intersection(x= buf_simple, y = mpas_simple)

## Calculate MPA area
intersection_areas <- data1 %>%
  sf::st_area() %>% 
  as.numeric() %>% 
  measurements::conv_unit(., "m2", "km2")

data2 <- data1 %>%
  mutate(buffer_mpa_area_km2 = intersection_areas)

# Compute buffered block stats
data3 <- data2 %>%
  sf::st_drop_geometry() %>%
  group_by(block_id) %>%
  dplyr::summarize(buffer_mpa_n=n_distinct(name),
                   buffer_mpas=paste(name, collapse=", "),
                   buffer_mpa_km2=sum(buffer_mpa_area_km2)) %>%
  ungroup() 

# Add block stats to block dataframe
buffer_3k <- block_stats_simple %>% 
  sf::st_drop_geometry() %>% 
  mutate(buffer_block_area_km2 = buf_area) %>% 
  left_join(., data3)
  
# Export as RDS
saveRDS(buffer_3k, file.path(out.dir, "CA_block_polygons_3km_buffer_mpa_density.Rds"))

### 5k -----------------------------------------------------
## Read Buffer
buf <- readRDS(file.path(out.dir, "CA_block_polygons_5km_buffer.Rds"))

## Simplify block buffer
buf_simple <- buf %>% 
  select(block_id)

## Calculate new block area (with buffer)
buf_area <-buf_simple %>% 
  st_area() %>% 
  as.numeric() %>% 
  measurements::conv_unit(., "m2", "km2")

## Find intersections with MPAs
data1 <- sf::st_intersection(x= buf_simple, y = mpas_simple)

## Calculate MPA area
intersection_areas <- data1 %>%
  sf::st_area() %>% 
  as.numeric() %>% 
  measurements::conv_unit(., "m2", "km2")

data2 <- data1 %>%
  mutate(buffer_mpa_area_km2 = intersection_areas)

# Compute buffered block stats
data3 <- data2 %>%
  sf::st_drop_geometry() %>%
  group_by(block_id) %>%
  dplyr::summarize(buffer_mpa_n=n_distinct(name),
                   buffer_mpas=paste(name, collapse=", "),
                   buffer_mpa_km2=sum(buffer_mpa_area_km2)) %>%
  ungroup() 

# Add block stats to block dataframe
buffer_5k <- block_stats_simple %>% 
  sf::st_drop_geometry() %>% 
  mutate(buffer_block_area_km2 = buf_area) %>% 
  left_join(., data3)

# Export as RDS
saveRDS(buffer_5k, file.path(out.dir, "CA_block_polygons_5km_buffer_mpa_density.Rds"))

### 10k -----------------------------------------------------
## Read Buffer
buf <- readRDS(file.path(out.dir, "CA_block_polygons_10km_buffer.Rds"))

## Simplify block buffer
buf_simple <- buf %>% 
  select(block_id)

## Calculate new block area (with buffer)
buf_area <-buf_simple %>% 
  st_area() %>% 
  as.numeric() %>% 
  measurements::conv_unit(., "m2", "km2")

## Find intersections with MPAs
data1 <- sf::st_intersection(x= buf_simple, y = mpas_simple)

## Calculate MPA area
intersection_areas <- data1 %>%
  sf::st_area() %>% 
  as.numeric() %>% 
  measurements::conv_unit(., "m2", "km2")

data2 <- data1 %>%
  mutate(buffer_mpa_area_km2 = intersection_areas)

# Compute buffered block stats
data3 <- data2 %>%
  sf::st_drop_geometry() %>%
  group_by(block_id) %>%
  dplyr::summarize(buffer_mpa_n=n_distinct(name),
                   buffer_mpas=paste(name, collapse=", "),
                   buffer_mpa_km2=sum(buffer_mpa_area_km2)) %>%
  ungroup() 

# Add block stats to block dataframe
buffer_10k <- block_stats_simple %>% 
  sf::st_drop_geometry() %>% 
  mutate(buffer_block_area_km2 = buf_area) %>% 
  left_join(., data3)

# Export as RDS
saveRDS(buffer_10k, file.path(out.dir, "CA_block_polygons_10km_buffer_mpa_density.Rds"))

### 20k -----------------------------------------------------
## Read Buffer
buf <- readRDS(file.path(out.dir, "CA_block_polygons_20km_buffer.Rds"))

## Simplify block buffer
buf_simple <- buf %>% 
  select(block_id)

## Calculate new block area (with buffer)
buf_area <-buf_simple %>% 
  st_area() %>% 
  as.numeric() %>% 
  measurements::conv_unit(., "m2", "km2")

## Find intersections with MPAs
data1 <- sf::st_intersection(x= buf_simple, y = mpas_simple)

## Calculate MPA area
intersection_areas <- data1 %>%
  sf::st_area() %>% 
  as.numeric() %>% 
  measurements::conv_unit(., "m2", "km2")

data2 <- data1 %>%
  mutate(buffer_mpa_area_km2 = intersection_areas)

# Compute buffered block stats
data3 <- data2 %>%
  sf::st_drop_geometry() %>%
  group_by(block_id) %>%
  dplyr::summarize(buffer_mpa_n=n_distinct(name),
                   buffer_mpas=paste(name, collapse=", "),
                   buffer_mpa_km2=sum(buffer_mpa_area_km2)) %>%
  ungroup() 

# Add block stats to block dataframe
buffer_20k <- block_stats_simple %>% 
  sf::st_drop_geometry() %>% 
  mutate(buffer_block_area_km2 = buf_area) %>% 
  left_join(., data3)

# Export as RDS
saveRDS(buffer_20k, file.path(out.dir, "CA_block_polygons_20km_buffer_mpa_density.Rds"))

### 50k -----------------------------------------------------
## Read Buffer
buf <- readRDS(file.path(out.dir, "CA_block_polygons_50km_buffer.Rds"))

## Simplify block buffer
buf_simple <- buf %>% 
  select(block_id)

## Calculate new block area (with buffer)
buf_area <-buf_simple %>% 
  st_area() %>% 
  as.numeric() %>% 
  measurements::conv_unit(., "m2", "km2")

## Find intersections with MPAs
data1 <- sf::st_intersection(x= buf_simple, y = mpas_simple)

## Calculate MPA area
intersection_areas <- data1 %>%
  sf::st_area() %>% 
  as.numeric() %>% 
  measurements::conv_unit(., "m2", "km2")

data2 <- data1 %>%
  mutate(buffer_mpa_area_km2 = intersection_areas)

# Compute buffered block stats
data3 <- data2 %>%
  sf::st_drop_geometry() %>%
  group_by(block_id) %>%
  dplyr::summarize(buffer_mpa_n=n_distinct(name),
                   buffer_mpas=paste(name, collapse=", "),
                   buffer_mpa_km2=sum(buffer_mpa_area_km2)) %>%
  ungroup() 

# Add block stats to block dataframe
buffer_50k <- block_stats_simple %>% 
  sf::st_drop_geometry() %>% 
  mutate(buffer_block_area_km2 = buf_area) %>% 
  left_join(., data3)

# Export as RDS
saveRDS(buffer_50k, file.path(out.dir, "CA_block_polygons_50km_buffer_mpa_density.Rds"))


# Compile All -------------------------------------------------------------------

buffer_0.5k <- buffer_0.5k %>% 
  rename(buffer_0.5k_block_area_km2 = buffer_block_area_km2,
         buffer_0.5k_mpa_n = buffer_mpa_n,
         buffer_0.5k_mpas = buffer_mpas,
         buffer_0.5k_mpa_km2 = buffer_mpa_km2) 

buffer_1k <- buffer_1k %>% 
  rename(buffer_1k_block_area_km2 = buffer_block_area_km2,
         buffer_1k_mpa_n = buffer_mpa_n,
         buffer_1k_mpas = buffer_mpas,
         buffer_1k_mpa_km2 = buffer_mpa_km2) 

buffer_3k <- buffer_3k %>% 
  rename(buffer_3k_block_area_km2 = buffer_block_area_km2,
         buffer_3k_mpa_n = buffer_mpa_n,
         buffer_3k_mpas = buffer_mpas,
         buffer_3k_mpa_km2 = buffer_mpa_km2) 

buffer_5k <- buffer_5k %>% 
  rename(buffer_5k_block_area_km2 = buffer_block_area_km2,
         buffer_5k_mpa_n = buffer_mpa_n,
         buffer_5k_mpas = buffer_mpas,
         buffer_5k_mpa_km2 = buffer_mpa_km2) 

buffer_10k <- buffer_10k %>% 
  rename(buffer_10k_block_area_km2 = buffer_block_area_km2,
         buffer_10k_mpa_n = buffer_mpa_n,
         buffer_10k_mpas = buffer_mpas,
         buffer_10k_mpa_km2 = buffer_mpa_km2) 

buffer_20k <- buffer_20k %>% 
  rename(buffer_20k_block_area_km2 = buffer_block_area_km2,
         buffer_20k_mpa_n = buffer_mpa_n,
         buffer_20k_mpas = buffer_mpas,
         buffer_20k_mpa_km2 = buffer_mpa_km2) 

buffer_50k <- buffer_50k %>% 
  rename(buffer_50k_block_area_km2 = buffer_block_area_km2,
         buffer_50k_mpa_n = buffer_mpa_n,
         buffer_50k_mpas = buffer_mpas,
         buffer_50k_mpa_km2 = buffer_mpa_km2) 

mpa_density <- left_join(buffer_0.5k, buffer_1k) %>% 
  left_join(., buffer_3k) %>% 
  left_join(., buffer_5k) %>% 
  left_join(., buffer_10k) %>% 
  left_join(., buffer_20k) %>% 
  left_join(., buffer_50k)


saveRDS(mpa_density, file.path(out.dir, "CA_block_polygons_buffer_mpa_density.Rds"))


