

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
gisdir <- file.path(basedir, "gis_data/processed")
datadir <- file.path(basedir, "mpa_watch/processed")
plotdir <- "analyses/3performance_human/figures"

# Read data
state_waters_poly <- readRDS(file.path(gisdir, "CA_state_waters_polygons.Rds"))
state_waters_line <- readRDS(file.path(gisdir, "CA_state_waters_polyline.Rds"))
mpas_orig <- readRDS(file.path(gisdir, "CA_MPA_polygons.Rds"))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Reduce to MPAs of interest
sort(unique(mpas_orig$type))
types_use <- c("SMR", "SMRMA", "SMCA", "SMCA (No-Take)")
mpas <- mpas_orig %>% 
  filter(type %in% types_use)
mpas_df <- mpas %>% 
  sf::st_drop_geometry()


# Build data
################################################################################

# Build data
data_orig <- readRDS(file.path(datadir, "MPA_Watch_2011_2022_surveys_ca_programs_wide.Rds"))
col_key <- readxl::read_excel(file.path(datadir, "column_key_ca_programs.xlsx"))

# Build data
data <- data_orig %>% 
  # Simplify
  select(survey_id, mpa_name, mpa_id, date, duration_hr, total_activities:comments) %>% 
  # Remove invalid surveys
  filter(duration_hr > 0) %>% 
  # Gather
  gather(key="use", value="n_obs", 6:ncol(.)) %>% 
  # Left join
  left_join(col_key) %>% 
  # Reduce to data of interest
  filter(type!="Total" & consumptive %in% c("Consumptive", "Non-consumptive") & !grepl("estimate?", use)) %>% 
  # Convert number of observations to numeric
  mutate(n_obs=as.numeric(n_obs)) %>% 
  # Reduce
  filter(!is.na(n_obs) & n_obs>0)

# Summarize data
surveys <- data %>% 
  # Number of activies on each survey by location/type
  group_by(survey_id, mpa_name, mpa_id, date, duration_hr, location, consumptive) %>% 
  summarize(n_activities=sum(n_obs)) %>% 
  ungroup() %>% 
  # Number per hour 
  mutate(activities_per_hour=n_activities/duration_hr)




