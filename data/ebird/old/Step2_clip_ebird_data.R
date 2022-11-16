

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(rinat)
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data/"
indir <- file.path(basedir, "ebird/raw/webscraped")
outdir <- file.path(basedir, "ebird/processed")
gisdir <- file.path(basedir, "gis_data/processed")
plotdir <- "data/ebird/figures"

# Read MPAs
mpas <- readRDS(file.path(gisdir, "CA_MPA_polygons_0.1km_buffer.Rds"))
mpas_simple <- mpas %>% select(name)
mpas_simple_sp <- mpas_simple %>% as(., "Spatial")

# Read ebird data
data_orig <- readRDS(file=file.path(outdir, "CA_ebird_data.Rds"))


# Read data
################################################################################

# Convert to sf
data_sf <- data_orig %>%
  sf::st_as_sf(coords=c("long_dd", "lat_dd"), crs=sf::st_crs(mpas))

# Convert to sp
data_sp <- data_sf %>%
  as(., "Spatial")

# Find points inside MPAs
inside_which_mpa <- sp::over(data_sp, mpas_simple_sp)
inside_which_mpa_chr <- inside_which_mpa$name

# Convert to dataframe
data_sf_df <- data_sf %>%
  sf::st_drop_geometry() %>% 
  mutate(mpa=inside_which_mpa_chr) %>% 
  filter(!is.na(mpa))

# Export data
saveRDS(data_sf_df, file.path(outdir, "CA_ebird_data_inside_mpas_100m_buffer.Rds"))



