species <- "targeted"
path <- "analyses/7habitat/output/targeted/site-year"

library(sf)
library(sp)
library(gstat)
library(spacetime)
library(lattice)
library(tidyverse)
library(ncf)
library(spdep)

# Load site locations and model results
sites <- readRDS(file.path("/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024", "site_locations_corrected.Rds")) %>% 
  dplyr::select(site, geometry)

top_models <- readRDS(file.path(path, paste0(species, "_results.rds")))$models

# Load and merge data; data_sp must include spatial (geometry) and temporal (year) info
data_sp <- readRDS(file.path(path, paste0(species, "_models.rds")))$data_sp %>% 
  left_join(sites)

# Convert to an sf object and add residuals and a time column
data_sf <- st_as_sf(data_sp)
data_sf$resid <- residuals(top_models$top)
data_sf$time <- as.POSIXct(paste0(data_sf$year, "-01-01"), tz = "UTC")

# Aggregate residuals by site (replace 'site_id' with your unique site identifier)
data_site <- data_sf %>%
  group_by(site) %>%
  summarise(avg_resid = mean(resid, na.rm = TRUE),
            geometry = st_union(geometry)) %>%
  st_as_sf()

# Extract coordinates from the sf object
coords <- st_coordinates(data_site)

max.dist <- max(dist(coords)) / 2
spline_cor <- spline.correlog(x = coords[,1],
                              y = coords[,2],
                              z = data_site$avg_resid,
                              resamp = 100,
                              xmax = max.dist)

plot(spline_cor, main = "Spline Correlogram of Model Residuals",
     xlab = "Distance (m)", ylab = "Spatial Correlation")

# Calculae moran's I
# Create a nearest-neighbor list (using k = 5, adjust k if needed)
nb <- knn2nb(knearneigh(coords, k = 5))

# Convert neighbor list to a spatial weights list
lw <- nb2listw(nb, style = "W")

# Run the global Moran's I test
moran_result <- moran.test(data_site$avg_resid, lw)
print(moran_result)


# Using the neighbor list 'lw' from your previous analysis:
localMI <- localmoran(data_site$avg_resid, lw)

# Add the local Moran's I and its p-value to your data_site object
data_site$localI <- localMI[, "Ii"]
data_site$pvalue <- localMI[, "Pr(z != E(Ii))"]
data_site$adj_p <- p.adjust(data_site$pvalue, method = "BH")
# FDR (Benjamini–Hochberg) is often preferred because it’s less conservative than Bonferroni when you have many tests (e.g., 138 sites).

data_plot <- data_site %>% 
  left_join(data_sp %>% distinct(site, affiliated_mpa, region4, site_type))


# Plot local Moran's I to visualize hotspots
ggplot(data_plot %>% filter(adj_p < 0.05)) +
  geom_sf(aes(fill = localI)) +
  scale_fill_viridis_c() +
  labs(title = "Local Moran's I", fill = "Local I") + facet_wrap(~region4)


# Consider removing Point Dume SMCA:
data_adj <- data_sp %>% filter(!affiliated_mpa == "point dume smca")

m <- top_models$top

m2 <- update(m, data = data_adj)

summary(m2)

data_adj$resid <- residuals(m2)

# Aggregate residuals by site (replace 'site_id' with your unique site identifier)
data_site <- data_adj %>%
  group_by(site) %>%
  summarise(avg_resid = mean(resid, na.rm = TRUE),
            geometry = st_union(geometry)) %>%
  st_as_sf()

# Extract coordinates from the sf object
coords <- st_coordinates(data_site)

max.dist <- max(dist(coords)) / 2
spline_cor <- spline.correlog(x = coords[,1],
                              y = coords[,2],
                              z = data_site$avg_resid,
                              resamp = 100,
                              xmax = max.dist)

plot(spline_cor, main = "Spline Correlogram of Model Residuals",
     xlab = "Distance (m)", ylab = "Spatial Correlation")

summary(spline_cor)

# Calculae moran's I
# Create a nearest-neighbor list (using k = 5, adjust k if needed)
nb <- knn2nb(knearneigh(coords, k = 5))

# Convert neighbor list to a spatial weights list
lw <- nb2listw(nb, style = "W")

# Run the global Moran's I test
moran_result <- moran.test(data_site$avg_resid, lw)
print(moran_result)


# Using the neighbor list 'lw' from your previous analysis:
localMI <- localmoran(data_site$avg_resid, lw)

# Add the local Moran's I and its p-value to your data_site object
data_site$localI <- localMI[, "Ii"]
data_site$pvalue <- localMI[, "Pr(z != E(Ii))"]
data_site$adj_p <- p.adjust(data_site$pvalue, method = "BH")
# FDR (Benjamini–Hochberg) is often preferred because it’s less conservative than Bonferroni when you have many tests (e.g., 138 sites).

data_plot <- data_site %>% 
  left_join(data_sp %>% distinct(site, affiliated_mpa, region4, site_type))


# Plot local Moran's I to visualize hotspots
ggplot(data_plot %>% filter(adj_p < 0.05)) +
  geom_sf(aes(fill = localI)) +
  scale_fill_viridis_c() +
  labs(title = "Local Moran's I", fill = "Local I") + facet_wrap(~region4)


# OLD:
# # Convert to SpatialPointsDataFrame for gstat functions
# data_site_sp <- as(data_site, "Spatial")
# 
# # Compute the semivariogram for aggregated residuals
# variog_site <- variogram(avg_resid ~ 1, data = data_site_sp, cutoff = 10000, width = 500)
# plot(variog_site, main = "Semivariogram of Aggregated Residuals")
# 
# # Fit a variogram model (spherical model as an example)
# vgm_model <- fit.variogram(variog_site, model = vgm(psill = 1, model = "Sph", range = 1500, nugget = 0))
# print(vgm_model)


unique_sites <- data_sf %>% 
  distinct(site, .keep_all = TRUE)

unique_coords <- st_coordinates(unique_sites)
dist_mat <- as.matrix(dist(unique_coords))
summary(apply(dist_mat, 1, function(x) sort(x)[2]))


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



