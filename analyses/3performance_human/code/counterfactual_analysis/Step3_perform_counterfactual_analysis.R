
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(MatchIt)
library(Polychrome)
library(cobalt)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
plotdir <- "analyses/3performance_human/figures"
datadir <- file.path(basedir, "counterfactuals")
outputdir <- "analyses/3performance_human/output"

# Load data
load(file.path(outputdir, "counterfactual_output.Rds"))


# Read data
################################################################################

# Format matches
match_key <- matches %>% 
  # Rename
  rename(pair_id=subclass, cell_type=mpa_yn) %>% 
  # Simplify
  select(pair_id, cell_id, cell_type, mpa) %>% 
  # Fill in MPA gaps
  fill(mpa, .direction="down") %>% 
  # Format cell type
  mutate(cell_type=ifelse(cell_type==1, "MPA", "Counterfactual"))

# Confirm that each MPA has the same number of MPA and counterfactual cells
check <- match_key %>% 
  count(mpa, cell_type) %>% 
  spread(key="cell_type", value="n") %>% 
  mutate(check=MPA==Counterfactual)

# Clean environment
rm(matches, matched, data_use)

# Summarize indicators by the cell id
################################################################################

# Template raster
#################################

# Read matching data
data_orig <- readRDS(file.path(datadir, "counterfactual_layers_shallow_epsg3309.Rds"))

# Build raster template
ras_temp <- data_orig %>% 
  select(x_epsg3309, y_epsg3309, cell) %>% 
  raster::rasterFromXYZ(crs = raster::crs("+init=epsg:3309"))


# iNaturalist (fix so that n observers)
#################################

# Read data
inat_orig <- readRDS(file=file.path(basedir, "inaturalist/processed", "2000_2021_inaturalist_data.Rds"))

# Format data
inat <- inat_orig %>% 
  # Reduce to dates of interest
  filter(date_obs>=lubridate::ymd("2012-01-01") & date_obs<=lubridate::ymd("2021-12-31")) %>% 
  # Convert to sf
  sf::st_as_sf(coords=c("long_dd", "lat_dd"), crs=sf::st_crs("+proj=longlat +datum=WGS84")) %>% 
  # Reproject
  sf::st_transform(crs="+init=epsg:3309")
rm(inat_orig)

# Convert to sp
inat_sp <- inat %>% 
  sf::as_Spatial()

# Extract cell id for each point
cell_ids <- raster::extract(x=ras_temp, y=inat_sp)

# Summarize
inat_df <- inat %>% 
  sf::st_drop_geometry() %>% 
  mutate(cell_id=cell_ids) %>% 
  group_by(cell_id) %>% 
  summarize(nobservers=n_distinct(user_id)) %>% 
  ungroup()


# eBird
#################################

# Read data
ebird_orig <- readRDS(file=file.path(basedir, "ebird/processed", "CA_ebird_data_inside_state_water_buffer.Rds"))

# Format data
ebird <- ebird_orig %>% 
  # Reduce to dates of interest
  filter(survey_date>=lubridate::ymd("2012-01-01") & survey_date<=lubridate::ymd("2021-12-31")) %>% 
  # Must have coordinates
  filter(!is.na(lat_dd) & !is.na(long_dd)) %>% 
  # Convert to sf
  sf::st_as_sf(coords=c("long_dd", "lat_dd"), crs=sf::st_crs("+proj=longlat +datum=WGS84")) %>% 
  # Reproject
  sf::st_transform(crs="+init=epsg:3309")
rm(ebird_orig)

# Convert to sp
ebird_sp <- ebird %>% 
  sf::as_Spatial()

# Extract cell id for each point
cell_ids <- raster::extract(x=ras_temp, y=ebird_sp)

# Summarize
ebird_df <- ebird %>% 
  sf::st_drop_geometry() %>% 
  mutate(cell_id=cell_ids) %>% 
  group_by(cell_id) %>% 
  summarize(nobservers=n_distinct(observer_id)) %>% 
  ungroup()

# REEF
#################################

# Read data
reef_orig <- readRDS(file=file.path(basedir, "reef/processed", "REEF_1994_2022_survey_metadata.Rds"))

# Format data
reef <- reef_orig %>% 
  # Filter to date range
  filter(date>=lubridate::ymd("2012-01-01") & date<=lubridate::ymd("2021-12-31")) %>% 
  # Must have coordinates
  filter(!is.na(lat_dd) & !is.na(long_dd)) %>% 
  # Convert to sf
  sf::st_as_sf(coords=c("long_dd", "lat_dd"), crs=sf::st_crs("+proj=longlat +datum=WGS84")) %>% 
  # Reproject
  sf::st_transform(crs="+init=epsg:3309")

# Rasterize
reef_ras <- raster::rasterize(x=reef, y= ras_temp, field="site_name", fun = "count")

# Convert to df
reef_ras_df <- reef_ras %>% 
  # Convert to df
  raster::as.data.frame(xy=T) %>% 
  # Redcue to cells with data
  filter(!is.na(layer)) %>% 
  # Add cell id
  left_join(data_orig %>% select(x_epsg3309, y_epsg3309, cell), by=c("x"="x_epsg3309", "y"="y_epsg3309")) %>% 
  # Rename
  rename(reef_surveys_n=layer, cell_id=cell)


# Build data
################################################################################

# Build data
data <- match_key %>% 
  # Add iNat stats
  left_join(inat_df, by="cell_id") %>% 
  rename(inat_observers_n=nobservers) %>% 
  # eBird stats
  left_join(ebird_df, by="cell_id") %>% 
  rename(ebird_observers_n=nobservers) %>% 
  # Add REEF stats
  left_join(reef_ras_df %>% select(cell_id, reef_surveys_n), by="cell_id")

# Build stats
stats <- data %>% 
  group_by(mpa, cell_type) %>% 
  summarize(inat_observers_n=sum(inat_observers_n, na.rm=T),
            ebird_observers_n=sum(ebird_observers_n, na.rm=T),
            reef_surveys_n=sum(reef_surveys_n, na.rm=T))

# Build ratios
ratios <- stats %>% 
  # Gather
  gather(key="indicator", value="value", 3:ncol(.)) %>% 
  # Format indicator
  mutate(indicator=recode(indicator,
                          "reef_surveys_n"="REEF",
                          "ebird_observers_n"="eBird",
                          "inat_observers_n"="iNaturalist")) %>% 
  # Spread 
  rename(mpa_name=mpa) %>% 
  spread(key="cell_type", value="value") %>% 
  rename(mpa=MPA, counterfactual=Counterfactual) %>% 
  # Calculate ratio
  mutate(ratio=log(mpa / counterfactual))

# Plot
g <- ggplot(ratios, aes(y=indicator, x=ratio)) +
  geom_boxplot(fill="grey90") +
  # geom_violin(fill="grey90", draw_quantiles=0.5) +
  # Reference line
  geom_vline(xintercept=0) +
  # Labels
  labs(x="Log-response ratio\n(MPA vs. non-MPA counterfactuals)", y="") +
  # Theme
  theme_bw()
g

# Export data
saveRDS(data, file.path(outputdir, "counterfactual_results.Rds"))




