# Examine spatial autocorrelation in top models
# Cori Lopazanski
# lopazanski@bren.ucsb.edu
# Dec 2024

# Setup ------------------------------------------------------------------------

library(sf)
library(sp)
library(gstat)
library(spacetime)
library(lattice)
library(tidyverse)
library(ncf)
library(spdep)

rm(list = ls())
gc()

# Load site locations and model results
sites <- readRDS(file.path("/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024", "site_locations_corrected.Rds")) %>% 
  dplyr::select(site, geometry)

# Read the top models 
path <- "~/ca-mpa/analyses/7habitat/output"
id <- "rock_targeted_rmsy"
id <- "kelp_targeted_my"

top_models <- readRDS(file.path(path, "results", paste0(id, "_results.rds")))$models

data_sp <- readRDS(file.path(path, "models", paste0(id, "_models.rds")))$data_sp %>% 
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

max.dist <- max(dist(coords)) / 20
spline_cor <- spline.correlog(x = coords[,1],
                              y = coords[,2],
                              z = data_site$avg_resid,
                              resamp = 100,
                              xmax = max.dist)

plot(spline_cor, main = NULL,
     xlab = "Distance (m)", ylab = "Spatial Correlation")

summary(spline_cor)

# Consider cross-correlogram with time
spline_cor_cross <- spline.correlog(
  x = st_coordinates(data_sf)[,1],  
  y = st_coordinates(data_sf)[,2],  
  z = data_sf$resid,  
  w = as.numeric(data_sf$time),  # Treat year as a second variable
  resamp = 100, 
  xmax = max(dist(st_coordinates(data_sf))) / 2,
  na.rm = TRUE  
)

plot(spline_cor_cross)

# Calculae moran's I
# Create a nearest-neighbor list (using k = 5, adjust k if needed)
nb <- knn2nb(knearneigh(coords, k = 5))

# Convert neighbor list to a spatial weights list
lw <- nb2listw(nb, style = "W")

# Run the global Moran's I test
moran_result <- moran.test(data_site$avg_resid, lw)
print(moran_result)

library(gt)
library(tidyr)

# Extract key values from the Moran's I result
moran_table <- data.frame(
  Metric = c("Moran's I", "Expectation", "Variance", "Z-Score", "p-Value"),
  Value = c(
    round(moran_result$estimate["Moran I statistic"], 3),
    round(moran_result$estimate["Expectation"], 3),
    round(moran_result$estimate["Variance"], 5),
    round(moran_result$statistic, 3),
    signif(moran_result$p.value, 3)
  )
)

# Convert to gt table
moran_gt <- moran_table %>%
  gt() %>%
  tab_header(title = "Global Moran's I Test Results") %>%
  cols_label(
    Metric = "Metric",
    Value = "Value"
  ) %>%
  fmt_number(columns = Value, decimals = 3) %>%
  tab_options(table.width = px(350))

# Print the table
moran_gt


# Using the neighbor list 'lw' from your previous analysis:
localMI <- localmoran(data_site$avg_resid, lw)

# Add the local Moran's I and its p-value to your data_site object
data_site$localI <- localMI[, "Ii"]
data_site$pvalue <- localMI[, "Pr(z != E(Ii))"]
data_site$adj_p <- p.adjust(data_site$pvalue, method = "BH")
# FDR (Benjamini–Hochberg) is often preferred because it’s less conservative than Bonferroni when you have many tests (e.g., 138 sites).

data_plot <- data_site %>% 
  left_join(data_sp %>% ungroup() %>% distinct(site, affiliated_mpa, region4, site_type)) %>% 
  mutate(adj_p = round(adj_p, 4))


# Plot local Moran's I to visualize hotspots
ggplot(data_plot %>% filter(adj_p < 0.05)) +
  geom_sf(aes(fill = localI)) +
  scale_fill_viridis_c() +
  labs(title = "Local Moran's I", fill = "Local I") + facet_wrap(~region4)

ggplot(data = data_plot %>% filter(affiliated_mpa == "piedras blancas smr")) + geom_sf(aes(geometry = geometry, fill = adj_p))

dist_matrix <- st_distance(data_site)

dist_df <- as.data.frame(as.table(as.matrix(dist_matrix)))

colnames(dist_df) <- c("site1", "site2", "distance_m")
dist_df$site1 <- data_site$site[as.numeric(dist_df$site1)]
dist_df$site2 <- data_site$site[as.numeric(dist_df$site2)]
dist_df$site1_type <- data_site$site_type[as.numeric(dist_df$site1)]
dist_df$site2_type <- data_site$site_type[as.numeric(dist_df$site2)]
dist_df <- dist_df %>% filter(site1 != site2)

ggplot(dist_df) +
  geom_density(aes(x = as.numeric(distance_m)))

# Consider removing Point Dume SMCA:
data_adj <- data_sp %>% 
  #filter(!affiliated_mpa == "point dume smca")
  filter(!site %in% c("BL29", "BL26",  "BL30", "BL31", "BL27")) # "BL34", "BL31",, "BL44"

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

max.dist <- max(dist(coords)) / 20
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
  left_join(data_sp %>% ungroup() %>%  distinct(site, affiliated_mpa, region4, site_type))

# OLD:
# # Convert to SpatialPointsDataFrame for gstat functions
data_site_sp <- as(data_site, "Spatial")

# Compute the semivariogram
variog_site <- variogram(avg_resid ~ 1, data = data_site_sp, cutoff = 20000, width = 500)

# Compare different variogram models
vgm_sph <- fit.variogram(variog_site, model = vgm(psill = 1, "Sph"))  # Spherical (current)
vgm_exp <- fit.variogram(variog_site, model = vgm("Exp"))  # Exponential
vgm_mat <- fit.variogram(variog_site, model = vgm("Mat"))  # Matérn

# Compare fit visually
plot(variog_site, model = vgm_sph, col = "red", main = "Variogram Model Comparison")
plot(variog_site, model = vgm_exp, add = TRUE, col = "blue")
plot(variog_site, model = vgm_mat, add = TRUE, col = "green")
legend("bottomright", legend = c("Spherical", "Exponential", "Matérn"), col = c("red", "blue", "green"), lty = 1)
# 

# # Compute the semivariogram for aggregated residuals
variog_site <- variogram(avg_resid ~ 1, data = data_site_sp, cutoff = 20000, width = 500)
plot(variog_site)


# # Fit a variogram model (spherical model as an example)
vgm_model <- fit.variogram(variog_site, model = vgm(psill = 1, model = "Exp"))
print(vgm_model)

# Compute space-time variogram
variog_space_time <- variogram(avg_resid ~ 1, data = data_site_sp, 
                               cutoff = 20000, width = 500, 
                               time = "year")  # Include time as a factor

# Plot the space-time variogram
plot(variog_space_time)

# Fit a space-time variogram model (e.g., spherical + temporal interaction)
vgm_space_time_model <- fit.variogram(variog_space_time, model = vgm("St", psill = 0.5, range = c(5000, 10), nugget = 0))
print(vgm_space_time_model)

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
  vgm_year <- variogram(resid ~ 1, data = subset_sp, cutoff = 10000, width = 500)
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
  facet_wrap(~year, scales = "free")



