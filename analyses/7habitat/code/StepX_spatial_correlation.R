species <- "SPUL"
path <- "analyses/7habitat/output/2way-4region-with-depth-comb/kelp"

library(sf)
library(sp)
library(gstat)
library(spacetime)
library(lattice)
library(dplyr)
library(ggplot2)

# Load site locations and model results
sites <- readRDS(file.path("/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024", "site_locations_corrected.Rds"))
top_models <- readRDS(file.path(path, paste0(species, "_results.rds")))$models

# Load and merge data; data_sp must include spatial (geometry) and temporal (year) info
data_sp <- readRDS(file.path(path, paste0(species, "_models.rds")))$data_sp %>% 
  left_join(sites)

# Convert to an sf object and add residuals and a time column
data_sf <- st_as_sf(data_sp)
data_sf$resid <- residuals(top_models$top)
data_sf$time <- as.POSIXct(paste0(data_sf$year, "-01-01"), tz = "UTC")

#---------------------------------------------------------
# Aggregated Spatial Variogram (by site)
#---------------------------------------------------------
# Aggregate residuals by site (replace 'site_id' with your unique site identifier)
data_site <- data_sf %>%
  group_by(site) %>%
  summarise(avg_resid = mean(resid, na.rm = TRUE),
            geometry = st_union(geometry)) %>%
  st_as_sf()

# Convert to SpatialPointsDataFrame for gstat functions
data_site_sp <- as(data_site, "Spatial")

# Compute the semivariogram for aggregated residuals
variog_site <- variogram(avg_resid ~ 1, data = data_site_sp, cutoff = 10000, width = 500)
plot(variog_site, main = "Semivariogram of Aggregated Residuals")

# Fit a variogram model (spherical model as an example)
vgm_model <- fit.variogram(variog_site, model = vgm(psill = 1, model = "Sph", range = 1500, nugget = 0))
print(vgm_model)


unique_sites <- data_sf %>% 
  distinct(site, .keep_all = TRUE)

unique_coords <- st_coordinates(unique_sites)
dist_mat <- as.matrix(dist(unique_coords))
summary(apply(dist_mat, 1, function(x) sort(x)[2]))

library(sf)
library(sp)
library(gstat)
library(ggplot2)
library(dplyr)

# Assuming your data_sf is an sf object with columns: 
# - 'year' (numeric or factor)
# - 'resid' (model residuals)
# - a geometry column

# Compute a variogram for each year
years <- unique(data_sf$year)
variog_list <- list()

for (yr in years) {
  subset_sf <- data_sf[data_sf$year == yr, ]
  subset_sp <- as(subset_sf, "Spatial")
  # Compute the variogram for this year; adjust cutoff and width as needed
  vgm_year <- variogram(resid ~ 1, data = subset_sp, cutoff = 550, width = 50)
  vgm_year$year <- yr
  variog_list[[as.character(yr)]] <- vgm_year
}

variog_all <- do.call(rbind, variog_list)

# Plot the variograms for each year together
ggplot(variog_all, aes(x = dist, y = gamma, color = as.factor(year))) +
  geom_line() +
  labs(color = "Year",
       x = "Spatial Distance (m)",
       y = "Semivariance",
       title = "Yearly Variograms of Model Residuals")+
  facet_wrap(~year)

library(ncf)
# Extract coordinates from the sf object
coords <- st_coordinates(data_site)

# Compute the spline correlogram.
# 'resamp = 100' uses 100 bootstrap resamples to assess uncertainty (adjust as needed)
spline_cor <- spline.correlog(x = coords[,1], y = coords[,2], z = data_site$avg_resid, resamp = 100)

# Plot the spline correlogram
plot(spline_cor, main = "Spline Correlogram of Model Residuals",
     xlab = "Distance (m)", ylab = "Spatial Correlation")






