# Matching
# Cori Lopazanski
# 13 July 2022

# List of covariates 
# 1. MPA Coverage 
# 2. Distance from Shore
# 3. Average Depth
# 4. Distance from Port
# 5. Area
# 6. Buffered MPA Density (not necessarily included in matching)
# 7. Historical Fishing Intensity (2000-2006)


# Setup --------------------------------------------------------------------------

## Clear workspace
rm(list = ls())

## Packages
library(tidyverse)
library(MatchIt)
library(Polychrome)
library(cobalt)

## Directories
### Full working group sync-data folder
base.dir <- "/Volumes/GoogleDrive-105151121202188525604/Shared drives/NCEAS MPA network assessment/MPA Network Assessment: Working Group Shared Folder/data/sync-data" # Cori Local

### Processed covariate GIS data
gis.dir <- file.path(base.dir, "gis_data", "processed")

### Processed covariate fisheries landings data
land.dir <- file.path("/Volumes/GoogleDrive-105151121202188525604/My Drive/Research/NCEAS - California MPA Working Group/fisheries-data/processed")


# Read Data --------------------------------------------------------------------




# Pre-matching covariate balance -----------------------------------------------

# Good balance = SMD and eCDF close to zero, variance ratios close to one
pre_match <- matchit(data = data,
                     block_treatment ~ block_area_km2 + block_mean_depth_fa + distance_to_shore_km + dist_to_port_km + total_lb_sqkm + block_lat_dd,
                     method = NULL,
                     distance = "glm")

summary(pre_match)

# Matching-----------------------------------------------

## 1. Genetic Matching ----
# 1:1 without replacement,
matched <- matchit(data = data,
                   block_treatment ~ block_area_km2 + block_mean_depth_fa + distance_to_shore_km + 
                     dist_to_port_km + block_lat_dd + total_lb_sqkm,
                   method = "genetic",
                   pop.size = 10000)
matched
summary(matched, un = FALSE)
bal.plot(matched, which = "both")
love.plot(matched, binary = "std", thresholds = c(m = .2))

## 2. Standard Propensity Score ----
matched.1 <- matchit(data = data,
                     block_treatment ~ block_area_km2 + block_mean_depth_fa + distance_to_shore_km + dist_to_port_km,
                     method = "nearest", #nearest neighbor matching
                     ratio = 1, # match each treatment block with one control block
                     distance = "glm", # logistic?
                     caliper = 0.20, # sd
                     replace = F
)
matched.1
summary(matched.1, un = F)

## 3. Mahalanobis ----
matched.3 <- matchit(data = data,
                     block_treatment ~ block_area_km2 + block_mean_depth_fa + distance_to_shore_km + dist_to_port_km,
                     ratio = 1, # match each treatment block with one control block
                     distance = "mahalanobis",
                     replace = F
)
matched.3
summary(matched.3)

## 4. Mahalanobis w/ PS Caliper 0.1 ----
# 1:1 nearest neighbor without replacement
# Mahalanobis matching with propensity score calipers of 0.1
# Estimated with logistic regression (caliper <distance> 0.026)
# 88 matches
matched.2 <- matchit(data = data,
                     block_treatment ~ block_area_km2 + block_mean_depth_fa + distance_to_shore_km + dist_to_port_km,
                     ratio = 1, # match each treatment block with one control block
                     distance = "glm", # logistic?
                     caliper = 0.10, # sd
                     mahvars = ~ block_area_km2 + block_mean_depth_fa + distance_to_shore_km + dist_to_port_km,
                     replace = F)
matched.2
summary(matched.2)
plot(matched.2)
plot(matched.2, type = "density")
plot(matched.2, type = "histogram")
print(matched.2)

matched.2.data <- get_matches(matched.2, data = data)


### Plots ------
p88 = createPalette(88,  c("#ff0000", "#00ff00", "#0000ff"))
swatch(p88)
names(p88) <- NULL

all_m2_data <- full_join(blocks_subset, matched.2.data) %>% 
  mutate(subclass = as.factor(subclass)) 


ggplot(data = all_data) +
  geom_sf(aes(fill = subclass)) +
  scale_fill_manual(values = p88, na.value = "grey50")


## 5. Mahalanobis w/ PS Calpier + Latitude ----
# Add latitude to try to get them closer together?
matched.2b <- matchit(data = data,
                      block_treatment ~ block_area_km2 + block_mean_depth_fa + distance_to_shore_km + dist_to_port_km + block_lat_dd,
                      ratio = 1, # match each treatment block with one control block
                      distance = "glm", # logistic?
                      caliper = 0.10, # sd
                      mahvars = ~ block_area_km2 + block_mean_depth_fa + distance_to_shore_km + dist_to_port_km + block_lat_dd,
                      replace = F)

matched.2b
summary(matched.2b)
plot(matched.2b)
plot(matched.2b, type = "density")
plot(matched.2b, type = "histogram")
print(matched.2b)

matched.2b.data <- get_matches(matched.2b, data = data)
bal.plot(pre_match)
bal.plot(matched.2b, which = "both")
love.plot(matched.2b, binary = "std", thresholds = c(m = .1))


p91 = createPalette(91,  c("#ff0000", "#00ff00", "#0000ff"))
swatch(p91)
names(p91) <- NULL

all_m2b_data <- full_join(blocks_subset, matched.2b.data) %>% 
  mutate(subclass = as.factor(subclass)) 

ggplot(data = all_m2b_data) +
  geom_sf(aes(fill = subclass)) +
  geom_sf_text(aes(label = subclass), size = 3) +
  scale_fill_manual(values = p91, na.value = "grey50") 

###----------------------------------------
# Adjust caliper for latitude because they still aint close
# Traditional mahalanobis?
matched.2c <- matchit(data = data,
                      block_treatment ~ block_area_km2 + block_mean_depth_fa + distance_to_shore_km + dist_to_port_km + block_lat_dd,
                      ratio = 1, # match each treatment block with one control block
                      distance = "mahalanobis",
                      caliper = c(block_area_km2 = 0.20,
                                  block_mean_depth_fa = 0.20,
                                  distance_to_shore_km = 0.20,
                                  dist_to_port_km = 0.20,
                                  block_lat_dd = 1), # sd and then value
                      std.caliper = c(TRUE, TRUE, TRUE, TRUE, FALSE), # std then value
                      # mahvars = ~ block_area_km2 + block_mean_depth_fa + distance_to_shore_km + dist_to_port_km + block_lat_dd,
                      replace = F)

matched.2c
summary(matched.2c, un = FALSE)
plot(matched.2c)

plot(summary(matched.2c), var.order = "unmatched")

plot(matched.2c, type = "density")
plot(matched.2c, type = "histogram")
print(matched.2c)

matched.2c.data <- get_matches(matched.2c, data = data)
matched.2c.all <- match.data(matched.2c)
bal.plot(matched.2c, which = "both")
love.plot(matched.2c, binary = "std", thresholds = c(m = .2))


p91 = createPalette(91,  c("#ff0000", "#00ff00", "#0000ff"))
swatch(p91)
names(p91) <- NULL

all_m2c_data <- full_join(blocks_subset, matched.2c.data) %>% 
  mutate(subclass = as.factor(subclass)) 

ggplot(data = all_m2c_data) +
  geom_sf(aes(fill = subclass)) +
  geom_sf_text(aes(label = subclass), size = 3) +
  scale_fill_manual(values = p91, na.value = "grey50") 

# Slightly more zoomed in version
ggplot(data = all_m2c_data) +
  geom_sf(aes(fill = subclass)) +
  geom_sf_text(aes(label = subclass), size = 3) +
  scale_fill_manual(values = p91, na.value = "grey50") +
  scale_y_continuous(limits = c(38, 42))

# Export this 2c version
#saveRDS(matched.2c.all, file.path(getwd(), "analyses", "2performance_fisheries",
#                                  "analyses", "blocks", 
#                                  "block_counterfactual_key.Rds"))

### CURRENT ------
# Adjust caliper for latitude because they still aint close
matched.2c <- matchit(data = data,
                      block_treatment ~ block_area_km2 + block_mean_depth_fa + distance_to_shore_km + dist_to_port_km + block_lat_dd,
                      ratio = 1, # match each treatment block with one control block
                      distance = "glm", # logistic?
                      caliper = c(0.20, block_lat_dd = 1), # sd and then value
                      std.caliper = c(TRUE, FALSE), # std then value
                      mahvars = ~ block_area_km2 + block_mean_depth_fa + distance_to_shore_km + dist_to_port_km + block_lat_dd,
                      replace = F)

matched.2c
summary(matched.2c)
plot(matched.2c)

plot(summary(matched.2c), var.order = "unmatched")

plot(matched.2c, type = "density")
plot(matched.2c, type = "histogram")
print(matched.2c)

matched.2c.data <- get_matches(matched.2c, data = data)
matched.2c.all <- match.data(matched.2c)
bal.plot(matched.2c, which = "both")
love.plot(matched.2c, binary = "std", thresholds = c(m = .2))


p91 = createPalette(91,  c("#ff0000", "#00ff00", "#0000ff"))
swatch(p91)
names(p91) <- NULL

all_m2c_data <- full_join(blocks_subset, matched.2c.data) %>% 
  mutate(subclass = as.factor(subclass)) 

ggplot(data = all_m2c_data) +
  geom_sf(aes(fill = subclass)) +
  geom_sf_text(aes(label = subclass), size = 3) +
  scale_fill_manual(values = p91, na.value = "grey50") 

# Slightly more zoomed in version
ggplot(data = all_m2c_data) +
  geom_sf(aes(fill = subclass)) +
  geom_sf_text(aes(label = subclass), size = 3) +
  scale_fill_manual(values = p91, na.value = "grey50") +
  scale_y_continuous(limits = c(38, 42))

ggplot(data = all_m2c_data) +
  geom_sf(aes(fill = subclass, show.legend = F)) +
  geom_sf_text(aes(label = subclass, show.legend = F), size = 3) +
  scale_fill_manual(values = p91, na.value = "grey50") +
  scale_y_continuous(limits = c(32.4, 35))
# Export this 2c version
#saveRDS(matched.2c.all, file.path(getwd(), "analyses", "2performance_fisheries",
#                                  "analyses", "blocks", 
#                                  "block_counterfactual_key.Rds"))



