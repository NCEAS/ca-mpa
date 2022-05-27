

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
gisdir <- file.path(basedir, "gis_data/processed")
indir <- file.path(basedir, "boats_sar/raw")
outdir <- file.path(basedir, "boats_sar/processed")
plotdir <- "data/boats_sar/figures"

# Read data
list.files(indir)
load(file.path(indir, "SAR_California_clean.Rdata"))

# Read MPAs
mpas <- readRDS(file.path(gisdir, "CA_MPA_polygons.Rds"))
mpas_simple_sp <- mpas %>% 
  select(name) %>% 
  as(., "Spatial")


# Format data
################################################################################

# Format data
data <- SAR_California_clean %>% 
  # Simplify
  select(Day, detecttime, lon, lat,  size_class) %>% 
  # Rename
  rename(long_dd=lon, lat_dd=lat, datetime=detecttime, date=Day) %>% 
  # Format dates
  mutate(date=lubridate::ymd(date),
         datetime=lubridate::ymd_hms(datetime)) %>% 
  # Extract date info
  mutate(year=lubridate::year(date),
         month=lubridate::month(date),
         day_of_week=lubridate::wday(date, label=T)) %>% 
  # Arrange
  select(year, month, date, datetime, day_of_week, everything())

# Inspect
str(data)


# Record if and which MPAs boats fall into
################################################################################

# Convert df to sf
data_sf <- data %>%
  sf::st_as_sf(coords=c("long_dd", "lat_dd"), crs=sf::st_crs(mpas))

# Convert sf to sp
data_sp <- data_sf %>%
  as(., "Spatial")

# Find points inside MPAs
inside_which_mpa <- sp::over(data_sp, mpas_simple_sp)
inside_which_mpa_chr <- inside_which_mpa$name

# Record MPAs
data$mpa <- inside_which_mpa_chr


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "CA_SAR_boats_data_example.Rds"))






