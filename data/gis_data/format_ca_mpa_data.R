


# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
basedir <- "/Users/cfree/Library/CloudStorage/GoogleDrive-cfree@ucsb.edu/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
indir <- file.path(basedir, "gis_data/raw/CA_MPA_boundaries/ds582")
outdir <- file.path(basedir, "gis_data/processed")
plotdir <- "data/gis_data/figures"

# Read data
data_orig <- sf::st_read(file.path(indir, "ds582.shp"))

# Get MPA traits
traits_orig <- read.csv(file.path(basedir, "mpa_traits/processed/mpa_attributes_clean.csv"), as.is=T)

# Get more MPA trait (has block id)
traits_orig2 <- readRDS(file=file.path(basedir, "mpa_traits/processed/CA_mpa_metadata.Rds"))

# Projections
wgs84 <- sf::st_crs("+proj=longlat +datum=WGS84")


# Format traits
################################################################################

# Format trais 
traits <- traits_orig %>% 
  # Reduce
  select(name, implementation_date) %>% 
  # Format name
  mutate(name=stringr::str_to_title(name),
         name=gsub("Smca", "SMCA", name),
         name=gsub("Smrma", "SMRMA", name),
         name=gsub("Smr", "SMR", name),
         name=recode(name,
                     "Ano Nuevo SMR"="Año Nuevo SMR",
                     "Arrow Point To Lion Head Point SMCA"="Arrow Point to Lion Head Point SMCA",
                     "Batiquitos Lagoon SMCA"="Batiquitos Lagoon SMCA (No-Take)",            
                     "Blue Cavern Onshore SMCA"="Blue Cavern Onshore SMCA (No-Take)",           
                     "Bolsa Chica Basin SMCA"="Bolsa Chica Basin SMCA (No-Take)",              
                     "Campus Point SMCA"="Campus Point SMCA (No-Take)",               
                     "Casino Point SMCA"="Casino Point SMCA (No-Take)",                
                     "Estero De Limantour SMR"="Estero de Limantour SMR",           
                     "Estero De San Antonio SMRMA"="Estero de San Antonio SMRMA",       
                     "Famosa Slough SMCA"="Famosa Slough SMCA (No-Take)",                 
                     "Goleta Slough SMCA"="Goleta Slough SMCA (No-Take)",                  
                     "Laguna Beach SMCA"="Laguna Beach SMCA (No-Take)",                 
                     "Mackerricher SMCA"="MacKerricher SMCA",                  
                     "Point Vicente SMCA"="Point Vicente SMCA (No-Take)",                
                     "San Elijo Lagoon SMCA"="San Elijo Lagoon SMCA (No-Take)")) %>% 
  # Format date
  mutate(mlpa_yn=T,
         implementation_date=lubridate::mdy(implementation_date),
         implementation_year=lubridate::year(implementation_date))

# Check that they are in the data below
traits$name[!traits$name %in% data_orig$NAME] %>% sort()
  

# Format data
################################################################################

# Inspect attributes
data_df_orig <- data_orig %>%
  sf::st_drop_geometry()

# Compute centroids
centroids <- data_orig %>%
  sf::st_transform(crs=wgs84) %>%
  sf::st_centroid(data_orig) %>%
  sf::st_coordinates() %>%
  as.data.frame() %>%
  rename(long_dd=X, lat_dd=Y)

# Format data
data <- data_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(area_sqmi=area_sq_mi,
         region=study_regi,
         name_full=fullname,
         url=dfg_url,
         name_short=shortname,
         area_a=acres,
         area_ha=hectares) %>%
  # Add area (sqkm)
  mutate(area_sqkm=measurements::conv_unit(area_sqmi, "mi2", "km2")) %>%
  # Add centroids
  bind_cols(centroids) %>%
  # Add implementation dates
  left_join(traits, by=c("name")) %>% 
  mutate(mlpa_yn=ifelse(is.na(mlpa_yn), F, T)) %>% 
  # Add block id
  left_join(traits_orig2 %>% select(mpa, block_id), by=c("name"="mpa")) %>% 
  mutate(block_id=as.numeric(block_id)) %>% 
  # Arrange
  select(name, name_full, name_short, region, type, ccr, ccr_int, 
         mlpa_yn, implementation_year, implementation_date, block_id, 
         area_sqkm, area_sqmi, area_a, area_ha, long_dd, lat_dd, everything()) %>%
  select(-objectid) %>%
  # Reproject
  sf::st_transform(wgs84)

# Inspect
head(data)
str(data)
freeR::complete(data)

# Inspect more
table(data$region)
table(data$type)
table(data$ccr)
table(data$ccr_int)
table(data$mlpa_yn)

# Reduce to SMRs and SMCAs
mpas_use <- data %>%
  filter(type %in% c("SMR", "SMCA"))

# Export data
saveRDS(data, file.path(outdir, "CA_MPA_polygons.Rds"))
saveRDS(mpas_use, file.path(outdir, "CA_MPA_polygons_smrs_smcas.Rds"))
sf::write_sf(data, file.path(outdir, "CA_MPA_polygons.shp"))


# Plot data
################################################################################

# Get blocks
blocks <- wcfish::blocks

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Plot data
g <- ggplot() +
  # Plot blocks
  geom_sf(data=blocks, fill=NA, color="grey40", lwd=0.1) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3) +
  # Plot MPAs
  geom_sf(data=data, fill="red", color=NA) +
  # Labels
  labs(x="", y="") +
  # Crop
  coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw()
g




