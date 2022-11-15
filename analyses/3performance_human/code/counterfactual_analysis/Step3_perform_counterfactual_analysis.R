
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


# Summarize indicators by the cell id
################################################################################

# Template raster
#################################

# Read matching data
data_orig <- readRDS(file.path(datadir, "counterfactual_layers_shallow_epsg3309.Rds"))

# Build raster template
ras_temp <- data_orig %>% 
  select(x_epsg3309, y_epsg3309, cell) %>% 
  raster::rasterFromXYZ(crs = crs("+init=epsg:3309"))


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

# Rasterize
inat_ras <- raster::rasterize(x=inat, y= ras_temp, field="user_name", fun = "count")

# Convert to df
inat_ras_df <- inat_ras %>% 
  # Convert to df
  raster::as.data.frame(xy=T) %>% 
  # Redcue to cells with data
  filter(!is.na(layer)) %>% 
  # Add cell id
  left_join(data_orig %>% select(x_epsg3309, y_epsg3309, cell), by=c("x"="x_epsg3309", "y"="y_epsg3309")) %>% 
  # Rename
  rename(inat_n=layer, cell_id=cell)


# eBird
#################################

# Read data
# ebird_orig <- readRDS(file=file.path(basedir, "ebird/processed", "CA_ebird_data.Rds"))


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
  rename(reef_n=layer, cell_id=cell)


# Build data
################################################################################

# Build data
data <- match_key %>% 
  # Add iNat stats
  left_join(inat_ras_df %>% select(cell_id, inat_n), by="cell_id") %>% 
  # Add REEF stats
  left_join(reef_ras_df %>% select(cell_id, reef_n), by="cell_id")

# Build stats
stats <- data %>% 
  group_by(mpa, cell_type) %>% 
  summarize(inat_n=sum(inat_n, na.rm=T),
            reef_n=sum(reef_n, na.rm=T))

# Build ratios
ratios <- stats %>% 
  # Gather
  gather(key="indicator", value="value", 3:ncol(.)) %>% 
  # Format indicator
  mutate(indicator=recode(indicator,
                          "reef_n"="REEF",
                          "inat_n"="iNaturalist")) %>% 
  # Spread 
  rename(mpa_name=mpa) %>% 
  spread(key="cell_type", value="value") %>% 
  rename(mpa=MPA, counterfactual=Counterfactual) %>% 
  # Calculate ratio
  mutate(ratio=log(mpa / counterfactual))

# Plot
g <- ggplot(ratios, aes(y=indicator, x=ratio)) +
  geom_boxplot(fill="grey90", draw_quantiles=0.5) +
  # geom_violin(fill="grey90", draw_quantiles=0.5) +
  # Reference line
  geom_vline(xintercept=0) +
  # Labels
  labs(x="Log-response ratio\n(MPA vs. non-MPA counterfactuals)", y="") +
  # Theme
  theme_bw()
g






