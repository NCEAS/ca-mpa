

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(raster)
library(terrainr)
library(tidyverse)

# Directories
basedir <- "/Users/cfree/Library/CloudStorage/GoogleDrive-cfree@ucsb.edu/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data" #Chris
indir <- file.path(basedir, "habitat_stanford/raw")
outdir <- file.path(basedir, "habitat_stanford/processed")

# Read full to use as template
data_all <- raster::raster(file.path(basedir, "habitat_cdfw/processed/CA_bottom_substrate_10m.tiff"))

# Read data
data_s_orig <- sf::st_read(file.path(indir, "HAB_SCSR_PredictedSubstrateWDepth.shp"))
data_c_orig <- sf::st_read(file.path(indir, "HAB_CCSR_PredictedSubstrate.shp"))
data_n_orig <- sf::st_read(file.path(indir, "HAB_NCSR_PredictedSubstrateWDepth.shp"))

# Projections
wgs84 <- sf::st_crs("+proj=longlat +datum=WGS84")

# NAD83 / California Albers = EPSG:3310
sf::st_crs(data_s_orig)


# Format data
################################################################################

# Format south
data_s <- data_s_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  setNames(c("id", "substrate1", "depth", "source", "substrate_depth", "substrate2", 
             "length1", "length2", "area", "geometry")) %>% 
  # Format
  mutate(substrate1=recode(substrate1, "unknown"="Unknown"),
         substrate2=recode(substrate2, "unknown"="Unknown"),
         region="Southern") %>% 
  # If 1 and 2 agree or if 2 is unknown, set to 1
  mutate(substrate=ifelse(substrate1==substrate2 | substrate2=="Unknown", substrate1, "error"))


# Inspect
str(data_s)
freeR::complete(data_s)
head(data_s)
table(data_s$id)
table(data_s$substrate)
table(data_s$substrate1)
table(data_s$substrate2)
table(data_s$depth)
table(data_s$substrate_depth)
table(data_s$source)


# Format central
data_c <- data_c_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  setNames(c("source", "depth", "zone", "substrate_depth", "substrate", "length", "area", "geometry")) %>% 
  # Format
  mutate(region="Central",
         source=as.character(source),
         substrate=stringr::str_to_sentence(substrate))

# Inspect
str(data_c)
freeR::complete(data_c)
table(data_c$source)
table(data_c$substrate)
table(data_c$depth)
table(data_c$substrate_depth)

# Format northern
data_n <- data_n_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  setNames(c("substrate",      
            "substrate_depth", 
            "depth", 
            "substrate2",   
            "substrate_depth2", 
            "grid",    
            "length",
            "id",
            "area",
            "length2",
            "length3",
            "area2",
            "geometry")) %>% 
  # Format
  mutate(region="Northern",
         substrate=stringr::str_to_title(substrate),
         depth)

# Inspect
str(data_n)
names(data_n)
freeR::complete(data_n)
table(data_n$substrate)
table(data_n$substrate2)
table(data_n$substrate_depth)
table(data_n$substrate_depth2)
table(data_n$depth)


# Merge data
################################################################################

# Merge data
data_sf <- bind_rows(data_s, data_c, data_n) %>%
  # Simplify
  select(region, substrate, depth) %>% 
  # Reformat depth
  mutate(depth=gsub(" ", "", depth)) %>% 
  # Remove unknown substrate
  filter(substrate!="Unknown") %>% 
  # Add numeric substrate
  mutate(substrate_code=recode(substrate, 
                               "Soft"="0",
                               "Hard"="1") %>% as.numeric()) %>% 
  # Arrange
  select(region, substrate, substrate_code, depth)

# Inspect
freeR::complete(data_sf)
table(data_sf$region)
table(data_sf$substrate)
table(data_sf$depth)

# Export
saveRDS(data_sf, file=file.path(outdir, "CA_substrate_polygon_stanford.Rds"))
sf::st_write(data_sf, dsn=file.path("~/Desktop", "CA_substrate_polygon_stanford.shp")) # sf won'twri

# # Bounding box
# bbox <- sf::st_bbox(data_sf)
# 
# # Template raster
# temp_ras <- raster::raster(res=10, 
#                            xmn=bbox$xmin,
#                            xmx=bbox$xmax,
#                            ymn=bbox$ymin,
#                            ymx=bbox$ymax, 
#                            crs='+init=EPSG:3310')

# Rasterize substrate polygon
substrate_ras <- fasterize::fasterize(sf=data_sf,
                                      raster=data_all,
                                      field="substrate_code",
                                      fun="first",
                                      background=NA)

# Plot
plot(substrate_ras)

# Export
raster::writeRaster(substrate_ras, filename = file.path(outdir, "CA_substrate_polygon_stanford_10m.tiff"), overwrite=T)








