# Examine spatial autocorrelation in top models
# Cori Lopazanski
# lopazanski@bren.ucsb.edu
# Dec 2024

# 1. Load top model and associated analysis dataset
# 2. Join site coordinates
# 3. Extract model residuals
# 4. Aggregate residuals to the site level
# 5. Test for spatial structure with spline correlograms and Moran's I
# 6. Map local Moran's I to identify clustered sites
# 7. Optionally refit after removing sparsely sampled sites

# Setup ------------------------------------------------------------------------

library(sf)
library(sp)
library(gstat)
library(spacetime)
library(lattice)
library(tidyverse)
library(ncf)
library(spdep)
library(gt)

rm(list = ls())
gc()

# Load Results & Tables --------------------------------------------------------

sites <- readRDS(file.path("/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024", "site_locations_corrected.Rds")) %>% 
  dplyr::select(site, geometry)

get_results <- function(habitat, re_string){
  results_file <- paste(habitat, re_string, "selection_results.rds", sep = "_")
  results <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/results", results_file)) 
  data_sp <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/data", paste(habitat, re_string, "data.rds", sep = "_"))) %>% left_join(sites, by = "site")
  results <- c(results, list(data_sp = data_sp))
  return(results)
}


#rock <- get_results("rock", "rmsy")
#kelp <- get_results("kelp", "rmsy")
#surf <- get_results("surf", "rm")

run_correlogs <- function(habitat, re_string){
  
  # Get the results - pull out the top models and data
  habitat_results <- get_results(habitat, re_string)

  # Add residuals from top model to the data
  data_sf <- st_as_sf(habitat_results$data_sp)
  data_sf$resid <- residuals(habitat_results$models$top)
  
  # Extract coordinates
  coords <- st_coordinates(data_sf)
  
  # Bind coordinates to the data
  data_sf <- data_sf %>%
    mutate(x = coords[,1],
           y = coords[,2])
  
  # Examine distances between sites associated with each MPA/Ref pair
  dists_mpa <- data_sf %>% 
    st_drop_geometry() %>% 
    group_by(affiliated_mpa) %>% 
    group_map(~ dist(cbind(.x$x, .x$y))) %>% 
    unlist()
  
  # Use that upper range to set the max distance
  print("Quantile distance: ")
  print(quantile(dists_mpa, probs = c(0.5, 0.75, 0.9)))
  
  # Function to get spline corellogram within each year
  run_correlog <- function(df) {
    
    if(nrow(df) < 10) return(NULL)
    
    q <- quantile(dists_mpa, 0.9)
    
    spline.correlog(
      x = df$x,
      y = df$y,
      z = df$resid,
      xmax = 20000,
      resamp = 100
    )
  }
  
  # Run spline correlograms for each year
  cor_list <- data_sf %>%
    st_drop_geometry() %>%
    group_split(year) %>% 
    lapply(run_correlog)
  
  # Append year as name for each one
  years <- data_sf %>% distinct(year) %>% pull(year)
  names(cor_list) <- years
  
  # Plot the correlograms
  par(mfrow = c(ceiling(sqrt(length(cor_list))),
                ceiling(length(cor_list)/ceiling(sqrt(length(cor_list))))))
  
  for (i in seq_along(cor_list)) {
    plot(cor_list[[i]],
         main = names(cor_list)[i],
         xlab = "Distance (m)",
         ylab = "Spatial Correlation")
  }
  
  extract_summary <- function(cor_obj) {
    if (is.null(cor_obj)) return(NULL)
    
    s <- summary(cor_obj)
    
    data.frame(
      est_cor = s$estimate[3],
      
      q025_cor = s$quantiles["0.025", "y"],
      q50_cor  = s$quantiles["0.5", "y"],
      q975_cor = s$quantiles["0.975", "y"],
      
      q025_dist = s$quantiles["0.025", "x"],
      q50_dist  = s$quantiles["0.5", "x"],
      q975_dist = s$quantiles["0.975", "x"]
    )
  }
  
  cor_summary <- do.call(rbind, lapply(cor_list, extract_summary))

  return(list(cor_list = cor_list,
              cor_summary = cor_summary))
  }



rock_spline <- run_correlogs("rock", "rmsy")
rock_spline$cor_summary


kelp_spline <- run_correlogs("kelp", "rmsy")
kelp_spline$cor_summary

surf_spline <- run_correlogs("surf", "rm")
# Doesn't work - look at average residuals across years; no one flagged that as concerning...


run_correlogs <- function(habitat, re_string){
  
  # Get the results - pull out the top models and data
  habitat_results <- get_results(habitat, re_string)
  
  # Add residuals from top model to the data
  data_sf <- st_as_sf(habitat_results$data_sp)
  data_sf$resid <- residuals(habitat_results$models$top)
  
  # Extract coordinates
  coords <- st_coordinates(data_sf)
  
  # Bind coordinates to the data
  data_site <- data_sf %>%
    mutate(x = coords[,1],
           y = coords[,2]) %>% 
    st_drop_geometry() %>%
    group_by(site) %>%
    summarise(resid = mean(resid, na.rm = TRUE),
              x = first(x),
              y = first(y))
  
  spline_cor <- spline.correlog(
    x = data_site$x,
    y = data_site$y,
    z = data_site$resid,
    resamp = 100,
    xmax = 30000)
  
 
  return(spline_cor)
}


rock_spline <- run_correlogs("rock", "rmsy")
plot(rock_spline)
summary(rock_spline)


kelp_spline <- run_correlogs("kelp", "rmsy")
plot(kelp_spline)
summary(kelp_spline)

surf_spline <- run_correlogs("surf", "rm")
plot(surf_spline)
summary(surf_spline)



# Convert to an sf object and add residuals and a time column
data_sf <- st_as_sf(data_sp)
data_sf$resid <- residuals(top_models$top)
data_sf$time <- as.POSIXct(paste0(data_sf$year, "-01-01"), tz = "UTC")

# Aggregate residuals by site
data_site <- data_sf %>%
  group_by(site) %>%
  summarise(avg_resid = mean(resid, na.rm = TRUE),
            geometry = st_union(geometry)) %>%
  st_as_sf()

# Extract coordinates from the sf object
set.seed('123')
coords <- st_coordinates(data_site)
hist(dist(coords), breaks = 1000, main = "Pairwise Distances", xlab = "Distance (m)")

if (habitat == "surf"){
  spline_cor <- spline.correlog(x = coords[,1],
                                y = coords[,2],
                                z = data_site$avg_resid,
                                resamp = 100,
                                xmax = 70000) # why?
} else {
  
  max.dist <- 20000
  spline_cor <- spline.correlog(x = coords[,1],
                                y = coords[,2],
                                z = data_site$avg_resid,
                                resamp = 100,
                                xmax = max.dist)
  
}

plot(spline_cor, main = NULL,
     xlab = "Distance (m)", ylab = "Spatial Correlation")

summary(spline_cor)


# Rocky reef says moderate autocorrelation to 3934m

# Consider cross-correlogram with time
# spline_cor_cross <- spline.correlog(
#   x = st_coordinates(data_sf)[,1],  
#   y = st_coordinates(data_sf)[,2],  
#   z = data_sf$resid,  
#   w = as.numeric(data_sf$time),  # Treat year as a second variable
#   resamp = 100, 
#   xmax = max(dist(st_coordinates(data_sf))) / 2,
#   na.rm = TRUE  
# )
# 
# plot(spline_cor_cross)

# Calculae moran's I
# Create a nearest-neighbor list 
nb <- knn2nb(knearneigh(coords, k = 5))

# Convert neighbor list to a spatial weights list
lw <- nb2listw(nb, style = "W")

# Run the global Moran's I test
moran_result <- moran.test(data_site$avg_resid, lw)
print(moran_result)

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
  geom_label(aes(geometry = geometry, label = site, color = site_type), stat = "sf_coordinates", size = 3, alpha = 0.7) +
  scale_fill_viridis_c() +
  labs(title = "Local Moran's I", fill = "Local I") + facet_wrap(~region4)

ggplot(data = data_plot %>% filter(affiliated_mpa == "piedras blancas smr")) + 
  geom_label(aes(geometry = geometry, label = site, color = site_type), stat = "sf_coordinates", size =3, alpha = 0.7)

ggplot(data = data_plot %>% filter(affiliated_mpa == "carrington point smr")) + 
  geom_label(aes(geometry = geometry, label = site, color = site_type), stat = "sf_coordinates", size =3, alpha = 0.7)


dist_matrix <- st_distance(data_site)

dist_df <- as.data.frame(as.table(as.matrix(dist_matrix))) 

colnames(dist_df) <- c("site1", "site2", "distance_m")
dist_df$site1 <- data_site$site[as.numeric(dist_df$site1)]
dist_df$site2 <- data_site$site[as.numeric(dist_df$site2)]
dist_df$site1_type <- data_site$site_type[as.numeric(dist_df$site1)]
dist_df$site2_type <- data_site$site_type[as.numeric(dist_df$site2)]
dist_df <- dist_df %>% filter(site1 != site2) %>% 
  left_join(site_visit, by = c("site1" = "site")) %>% 
  left_join(site_visit, by = c("site2" = "site"), suffix = c(".1", ".2"))

dist_df2 <- dist_df %>% 
  filter(distance_m < 2000)

ggplot(dist_df) +
  geom_density(aes(x = as.numeric(distance_m)))

site_visit <- data_sp %>% 
  group_by(affiliated_mpa, site_type, site) %>% 
  summarize(n = n(), .groups = 'drop')

# Consider removing Point Dume SMCA:
data_adj <- data_sp %>% 
  filter(!site %in% site_visit$site[site_visit$n <=2])
  #filter(!affiliated_mpa == "point dume smca")
 # filter(!site %in% c("BL29","BL31", "BL26", "BL27", "BL30", "BL43")) # "BL34", "BL31",, "BL44"  "BL30","BL27" ,"BL30", "BL31", "BL34", "BL44", "BL45"

m <- top_models$top

m2 <- update(m, formula = top_effects$formulas$model_formula_top, data = data_adj)

summary(m2)

data_adj$resid <- residuals(m2)

# Aggregate residuals by site
data_site <- data_adj %>%
  group_by(site) %>%
  summarise(avg_resid = mean(resid, na.rm = TRUE),
            geometry = st_union(geometry)) %>%
  st_as_sf()

# Extract coordinates from the sf object
coords <- st_coordinates(data_site)

max.dist <- 30000
spline_cor <- spline.correlog(x = coords[,1],
                              y = coords[,2],
                              z = data_site$avg_resid,
                              resamp = 100,
                              xmax = max.dist)

plot(spline_cor, xlab = "Distance (m)", ylab = "Spatial Correlation")

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

check_model(m)
check_model(m2)

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


# Residuals vs fitted plots for rock?
