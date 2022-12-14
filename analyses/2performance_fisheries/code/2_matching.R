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

### Compiled covariate data
data.dir <- file.path("analyses", "2performance_fisheries", "analyses", "blocks")

# Read Data --------------------------------------------------------------------
data <- readRDS(file.path(data.dir, "block_all_covariates.Rds"))


# Pre-matching covariate balance -----------------------------------------------

# Good balance = SMD and eCDF close to zero, variance ratios close to one
pre_match <- matchit(data = data,
                     block_treatment ~ block_area_km2 + block_mean_depth_fa + 
                       distance_to_shore_km + dist_to_port_km + total_lb_sqkm + 
                       block_lat_dd + total_lb,
                     method = NULL,
                     distance = "glm")

summary(pre_match)
bal.tab(pre_match)

# Matching-----------------------------------------------

## 1. Genetic Matching 1 ----
### a) 1:1 w/o repl, using PS ----
matched.1a <- matchit(data = data,
                   block_treatment ~ block_area_km2 + block_mean_depth_fa + distance_to_shore_km + 
                     dist_to_port_km + block_lat_dd + total_lb,
                   method = "genetic", 
                   pop.size = 100000)
matched.1a
summary(matched.1a, un = FALSE)
bal.tab(matched.1a, which = "both")
bal.plot(matched.1a, which = "both")
love.plot(matched.1a, binary = "std", thresholds = c(m = .2))

### b) 1:1 w/o repl, using PS + caliper  ----
matched.1b <- matchit(data = data,
                   block_treatment ~ block_area_km2 + block_mean_depth_fa + distance_to_shore_km + 
                     dist_to_port_km + block_lat_dd + total_lb,
                   method = "genetic", 
                   caliper = c(0.2, block_lat_dd = 1),
                   std.caliper = c(TRUE, FALSE),
                   pop.size = 10000)
matched.1b
bal.tab(matched.1b)

# BREAK -----
### c) 1:2 w/ repl, no PS, no caliper ----
matched.1c <- matchit(data = data,
                      block_treatment ~ block_area_km2 + block_mean_depth_fa + distance_to_shore_km + 
                        dist_to_port_km + block_lat_dd + total_lb,
                      method = "genetic", 
                      replace = TRUE,
                      ratio = 2, distance = "mahalanobis",
                      pop.size = 100000)
matched.1c
summary(matched.1c)
plot(matched.1c)
bal.tab(matched.1c)
plot(matched.1c, type = "density")


matched.1c.data <- get_matches(matched.1c, data = data)
matched.1c.all <- match.data(matched.1c)

my.vars <- data.frame(new = c("Block Area", "Mean Depth", 
                              "Distance to Shore", "Distance to Port", 
                              "Latitude", "Fishing Intensity"),
                      old = var.names(bal.tab(matched.1c))) %>% select(new)

bal.plot(matched.1c, which = "both", 
         var.name = var.names(bal.tab(matched.1c)))
love.plot(matched.1c, 
          stats = "mean.diffs",
          #binary = "std", 
          var.order = "adjusted",
          thresholds = c(m = .2),
          sample.names = c("Unmatched", "Matched"),
          var.names = my.vars)

#### Create Palette ----
p112 = createPalette(112,  c("#ff0000", "#00ff00", "#0000ff"))
swatch(p112)
names(p112) <- NULL

#### Load Blocks ----
blocks <- wcfish::blocks
blocks_subset <- blocks %>% 
  filter(!(block_id %in% blocks$block_id[blocks$block_type %in% c("Offshore", "Midshore")])) %>% 
  filter(block_state == "California")

#### Create DF ----
all_1c_data <- full_join(blocks_subset, matched.1c.data) %>% 
  mutate(subclass = as.factor(subclass)) 

#### Plot ----
ggplot(data = all_1c_data) +
  geom_sf(aes(fill = subclass)) +
  geom_sf_text(aes(label = subclass), size = 3) +
  scale_fill_manual(values = p112, na.value = "grey50") +
  scale_y_continuous(limits = c(32.3, 42), expand = c(0,0))

# Export this 3d version
#saveRDS(matched.3d.all, file.path(getwd(), "analyses", "2performance_fisheries",
#                                  "analyses", "blocks", 
#                                  "block_counterfactual_key.Rds"))

### d) 1:2 w/ repl, PS, caliper ----
matched.1d <- matchit(data = data,
                      block_treatment ~ block_area_km2 + block_mean_depth_fa + distance_to_shore_km + 
                        dist_to_port_km + block_lat_dd + total_lb,
                      method = "genetic", 
                      replace = TRUE,
                      ratio = 2, 
                      mahvars = ~ block_area_km2 + block_mean_depth_fa + distance_to_shore_km + 
                        dist_to_port_km + block_lat_dd + total_lb,
                      caliper = c(0.2, block_lat_dd = 1),
                      std.caliper = c(TRUE, FALSE),
                      pop.size = 10000)

bal.tab(matched.1d)
plot(matched.1d, type = "density")
matched.1d.data <- get_matches(matched.1d, data = data)
matched.1d.all <- match.data(matched.1d)

my.vars <- data.frame(new = c("Block Area", "Mean Depth", 
                              "Distance to Shore", "Distance to Port", 
                              "Latitude", "Fishing Intensity"),
                      old = var.names(bal.tab(matched.1c))) %>% select(new)

bal.plot(matched.1d, which = "both",
         sample.names = c("Unmatched", "Matched"),
         position = c(0.8, 0.8))

love.plot(matched.1d, 
          stats = "mean.diffs",
          #binary = "std", 
          var.order = "adjusted",
          thresholds = c(m = .2),
          sample.names = c("Unmatched", "Matched"),
          var.names = my.vars)

#### Create Palette ----
p110 = createPalette(110,  c("#ff0000", "#00ff00", "#0000ff"))
swatch(p110)
names(p110) <- NULL

#### Load Blocks ----
blocks <- wcfish::blocks
blocks_subset <- blocks %>% 
  filter(!(block_id %in% blocks$block_id[blocks$block_type %in% c("Offshore", "Midshore")])) %>% 
  filter(block_state == "California")

#### Create DF ----
all_1d_data <- full_join(blocks_subset, matched.1d.data) %>% 
  mutate(subclass = as.factor(subclass)) 

#### Plot ----
ggplot(data = all_1d_data) +
  geom_sf(aes(fill = subclass)) +
  geom_sf_text(aes(label = subclass), size = 2.5) +
  scale_fill_manual(values = p110, na.value = "grey50") +
  scale_y_continuous(limits = c(32.3, 42.5), expand = c(0,0))

# Export this 3d version
#saveRDS(matched.3d.all, file.path(getwd(), "analyses", "2performance_fisheries",
#                                  "analyses", "blocks", 
# 

## 2. Standard Propensity Score ----
matched.2 <- matchit(data = data,
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
### a) 1:1 w/o repl ----
matched.3a <- matchit(data = data,
                     block_treatment ~ block_area_km2 + block_mean_depth_fa + distance_to_shore_km + dist_to_port_km,
                     ratio = 1, # match each treatment block with one control block
                     distance = "mahalanobis",
                     replace = F
)
matched.3a
summary(matched.3a)

### b) 1:1 w/o repl, PS caliper 0.1 ----
# 1:1 nearest neighbor without replacement
# Mahalanobis matching with propensity score calipers of 0.1
# Estimated with logistic regression (caliper <distance> 0.026)
# 88 matches
matched.3b <- matchit(data = data,
                     block_treatment ~ block_area_km2 + block_mean_depth_fa + distance_to_shore_km + dist_to_port_km,
                     ratio = 1, # match each treatment block with one control block
                     distance = "glm", # logistic?
                     caliper = 0.10, # sd
                     mahvars = ~ block_area_km2 + block_mean_depth_fa + distance_to_shore_km + dist_to_port_km,
                     replace = F)
matched.3b
summary(matched.3b)
plot(matched.3b)
plot(matched.3b, type = "density")
plot(matched.3b, type = "histogram")
print(matched.3b)

matched.3b.data <- get_matches(matched.3b, data = data)


### Plots ------
p88 = createPalette(88,  c("#ff0000", "#00ff00", "#0000ff"))
swatch(p88)
names(p88) <- NULL

all_3b_data <- full_join(blocks_subset, matched.3b.data) %>% 
  mutate(subclass = as.factor(subclass)) 


ggplot(data = all_3b_data) +
  geom_sf(aes(fill = subclass)) +
  scale_fill_manual(values = p88, na.value = "grey50")


### c) 1:1 w/o repl, PS + Lat caliper ----
# Mahalanobis w/ PS Calpier + Latitude
# Add latitude to try to get them closer together?
matched.3c <- matchit(data = data,
                      block_treatment ~ block_area_km2 + block_mean_depth_fa + distance_to_shore_km + dist_to_port_km + block_lat_dd,
                      ratio = 1, # match each treatment block with one control block
                      distance = "glm", # logistic?
                      caliper = 0.10, # sd
                      mahvars = ~ block_area_km2 + block_mean_depth_fa + distance_to_shore_km + dist_to_port_km + block_lat_dd,
                      replace = F)

matched.3c
summary(matched.3c)
plot(matched.3c)
plot(matched.3c, type = "density")
plot(matched.3c, type = "histogram")
print(matched.3c)

matched.3c.data <- get_matches(matched.3c, data = data)
bal.plot(pre_match)
bal.plot(matched.3c, which = "both")
love.plot(matched.3c, binary = "std", thresholds = c(m = .1))


p91 = createPalette(91,  c("#ff0000", "#00ff00", "#0000ff"))
swatch(p91)
names(p91) <- NULL

all_3c_data <- full_join(blocks_subset, matched.3c.data) %>% 
  mutate(subclass = as.factor(subclass)) 

ggplot(data = all_3c_data) +
  geom_sf(aes(fill = subclass)) +
  geom_sf_text(aes(label = subclass), size = 3) +
  scale_fill_manual(values = p91, na.value = "grey50") 

### d) NR ----
# Traditional mahalanobis?
matched.3d <- matchit(data = data,
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

matched.3d
summary(matched.3d, un = FALSE)
plot(matched.3d)

plot(summary(matched.3d), var.order = "unmatched")

plot(matched.3d, type = "density")
plot(matched.3d, type = "histogram")
print(matched.3d)

matched.3d.data <- get_matches(matched.3d, data = data)
matched.3d.all <- match.data(matched.3d)
bal.plot(matched.3d, which = "both")
love.plot(matched.3d, binary = "std", thresholds = c(m = .2))


p91 = createPalette(91,  c("#ff0000", "#00ff00", "#0000ff"))
swatch(p91)
names(p91) <- NULL

all_3d_data <- full_join(blocks_subset, matched.3d.data) %>% 
  mutate(subclass = as.factor(subclass)) 

ggplot(data = all_3d_data) +
  geom_sf(aes(fill = subclass)) +
  geom_sf_text(aes(label = subclass), size = 3) +
  scale_fill_manual(values = p91, na.value = "grey50") 

# Slightly more zoomed in version
ggplot(data = all_3d_data) +
  geom_sf(aes(fill = subclass)) +
  geom_sf_text(aes(label = subclass), size = 3) +
  scale_fill_manual(values = p91, na.value = "grey50") +
  scale_y_continuous(limits = c(38, 42))

# Export this 3d version
#saveRDS(matched.3d.all, file.path(getwd(), "analyses", "2performance_fisheries",
#                                  "analyses", "blocks", 
#                                  "block_counterfactual_key.Rds"))

## CURRENT ------
matched.4 <- matchit(data = data,
                      block_treatment ~ block_area_km2 + block_mean_depth_fa + distance_to_shore_km + dist_to_port_km + block_lat_dd,
                      ratio = 1, # match each treatment block with one control block
                      distance = "glm", # logistic?
                      caliper = c(0.20, block_lat_dd = 1), # sd and then value
                      std.caliper = c(TRUE, FALSE), # std then value
                      mahvars = ~ block_area_km2 + block_mean_depth_fa + distance_to_shore_km + dist_to_port_km + block_lat_dd,
                      replace = F)

matched.4
summary(matched.4)
plot(matched.4)

plot(summary(matched.4), var.order = "unmatched")

plot(matched.4, type = "density")
plot(matched.4, type = "histogram")
print(matched.4)

matched.4.data <- get_matches(matched.4, data = data)
matched.4.all <- match.data(matched.4)
bal.plot(matched.4, which = "both")
love.plot(matched.4, binary = "std", thresholds = c(m = .2))


p91 = createPalette(91,  c("#ff0000", "#00ff00", "#0000ff"))
swatch(p91)
names(p91) <- NULL

all_4_data <- full_join(blocks_subset, matched.4.data) %>% 
  mutate(subclass = as.factor(subclass)) 

ggplot(data = all_4_data) +
  geom_sf(aes(fill = subclass)) +
  geom_sf_text(aes(label = subclass), size = 3) +
  scale_fill_manual(values = p91, na.value = "grey50") 

# Slightly more zoomed in version
ggplot(data = all_4_data) +
  geom_sf(aes(fill = subclass)) +
  geom_sf_text(aes(label = subclass), size = 3) +
  scale_fill_manual(values = p91, na.value = "grey50") +
  scale_y_continuous(limits = c(38, 42))

ggplot(data = all_4_data) +
  geom_sf(aes(fill = subclass, show.legend = F)) +
  geom_sf_text(aes(label = subclass, show.legend = F), size = 3) +
  scale_fill_manual(values = p91, na.value = "grey50") +
  scale_y_continuous(limits = c(32.4, 35))

# Export this 4 version
#saveRDS(matched.4.all, file.path(getwd(), "analyses", "2performance_fisheries",
#                                  "analyses", "blocks", 
#                                  "block_counterfactual_key.Rds"))



