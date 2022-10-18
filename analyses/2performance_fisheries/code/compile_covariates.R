# Compile Block Covariates
# Cori Lopazanski
# 13 July 2022

# List of covariates and their locations:
# 1. MPA Coverage 
# 2. Distance from Shore
# 3. Average Depth
# 4. Distance from Port
# 5. Area
# 6. Buffered MPA Density (not necessarily included in matching)


# Setup --------------------------------------------------------------------------

## Clear workspace
rm(list = ls())

## Packages
library(tidyverse)
library(MatchIt)
library(Polychrome)
library(cobalt)

## Directories
base.dir <- "/Volumes/GoogleDrive-105151121202188525604/Shared drives/NCEAS MPA network assessment/MPA Network Assessment: Working Group Shared Folder/data/sync-data" # Cori Local
gis.dir <- file.path(base.dir, "gis_data", "processed")

blocks <- wcfish::blocks
blocks_simple <- blocks %>% sf::st_drop_geometry()

blocks_subset <- blocks %>% 
  filter(!(block_id %in% blocks$block_id[blocks$block_type %in% c("Offshore", "Midshore")])) %>% 
  filter(block_state == "California")

# Read Data --------------------------------------------------------------------
block_stats <- readRDS(file.path(gis.dir,"block_mpa_coverage_reduced_types.Rds")) 
depth <- readRDS(file.path(gis.dir, "block_mean_depth.Rds")) 
shore <- readRDS(file.path(gis.dir, "block_distance_to_shore.Rds"))
port <- readRDS(file.path(gis.dir, "block_distance_to_port.Rds"))

# Add lat/lon for proximity
block_latlon <- blocks_simple %>% 
  select(block_id, block_long_dd, block_lat_dd)

# Build Data -------------------------------------------------------------------

# Join covariates
data <- block_stats %>% 
  left_join(., depth) %>% 
  left_join(., shore) %>% 
  left_join(., port) %>% 
  left_join(., block_latlon) %>% 
  ## Add treatment column
  mutate(block_treatment = ifelse(is.na(mpa_n), 0, 1)) %>% 
  ## Remove midshore and offshore blocks
  filter(!(block_id %in% blocks$block_id[blocks$block_type %in% c("Offshore", "Midshore")])) %>% 
  select(block_id, block_area_km2, block_mean_depth_m:block_treatment)



# Matching -------------------------------------------------------------------

## 1. Pre-matching covariate balance
# Good balance = SMD and eCDF close to zero, variance ratios close to one
pre_match <- matchit(data = data,
                     block_treatment ~ block_area_km2 + block_mean_depth_fa + distance_to_shore_km + dist_to_port_km,
                     method = NULL,
                     distance = "glm")

summary(pre_match)

## 2. Matching

# Genetic Matching
# 1:1 without replacement,
# propensity score distance with caliper 0.052, 
# estimated with logistic regression,
# 94 matches
matched <- matchit(data = data,
                   block_treatment ~ block_area_km2 + block_mean_depth_fa + distance_to_shore_km + dist_to_port_km,
                   method = "genetic",
                   ratio = 1, # match each treatment block with one control block,
                   caliper = 0.20, 
                   replace = F
)
matched
summary(matched, un = FALSE)

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

matched.3 <- matchit(data = data,
                     block_treatment ~ block_area_km2 + block_mean_depth_fa + distance_to_shore_km + dist_to_port_km,
                     ratio = 1, # match each treatment block with one control block
                     distance = "mahalanobis",
                     replace = F
)
matched.3
summary(matched.3)

###----------------------------------------
# Think this one is best?
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



###----------------------------------------
p88 = createPalette(88,  c("#ff0000", "#00ff00", "#0000ff"))
swatch(p88)
names(p88) <- NULL

all_m2_data <- full_join(blocks_subset, matched.2.data) %>% 
  mutate(subclass = as.factor(subclass)) 
  

ggplot(data = all_data) +
  geom_sf(aes(fill = subclass)) +
  scale_fill_manual(values = p88, na.value = "grey50")

###----------------------------------------
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
plot(matched.2c, type = "density")
plot(matched.2c, type = "histogram")
print(matched.2c)

matched.2c.data <- get_matches(matched.2c, data = data)
matched.2c.all <- match.data(matched.2c)
bal.plot(matched.2c, which = "both")
love.plot(matched.2c, binary = "std", thresholds = c(m = .1))


p91 = createPalette(91,  c("#ff0000", "#00ff00", "#0000ff"))
swatch(p91)
names(p91) <- NULL

all_m2c_data <- full_join(blocks_subset, matched.2c.data) %>% 
  mutate(subclass = as.factor(subclass)) 

ggplot(data = all_m2c_data) +
  geom_sf(aes(fill = subclass)) +
  geom_sf_text(aes(label = subclass), size = 3) +
  scale_fill_manual(values = p91, na.value = "grey50") 

# Export this 2c version
#saveRDS(matched.2c.all, file.path(getwd(), "analyses", "2performance_fisheries",
#                                  "analyses", "blocks", 
#                                  "block_counterfactual_key.Rds"))




