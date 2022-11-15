
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

# Read matching data
data_orig <- readRDS(file.path(datadir, "counterfactual_layers_shallow_epsg3309.Rds"))

# Get MPAs
mpas_orig <- wcfish::mpas_ca
mpas <- mpas_orig %>% 
  filter(type!="SMP")

# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Remove useless
  select(-inat_count) %>% 
  # Rename
  rename(cell_id=cell, 
         depth_m=bathymetry, 
         npeople_50km=population_density_50km,
         shore_dist_km=distance_shore,
         park_dist_km=distance_park, # entry point
         beach_dist_km=distance_beach, # beach access
         mpa_yn=mpa_bin, 
         nbeaches_600m=beach_access_count_600m, # beach access points
         nparks_600m=park_count_600m) %>% # park access points
  # Convert NAs to zeros
  mutate(npeople_50km=ifelse(is.na(npeople_50km), 0, npeople_50km),
         nparks_600m=ifelse(is.na(nparks_600m), 0, nparks_600m),
         nbeaches_600m=ifelse(is.na(nparks_600m), 0, nparks_600m)) %>% 
  # Format MPA yes/no
  mutate(mpa_yn=ifelse(is.na(mpa_yn), 0, mpa_yn)) %>% 
  # Format MPA id
  mutate(mpa_id=ifelse(is.nan(mpa_id), NA, mpa_id)) %>% 
  # Add MPA name
  mutate(mpa=mpas$name[mpa_id]) %>% 
  # Arrange
  select(cell_id, x_epsg3309, y_epsg3309, 
         mpa_yn, mpa_id, mpa,
         shore_dist_km, depth_m, npeople_50km, 
         park_dist_km, beach_dist_km,
         nparks_600m, nbeaches_600m)

# Inspect
freeR::complete(data)
table(data$mpa_yn)

# Build MPA range
mpa_vals <- data %>% 
  filter(mpa_yn==1) %>% 
  gather(key="var", value="value", 7:ncol(.)) %>% 
  group_by(var) %>% 
  summarize(value_min=min(value),
            value_max=max(value))

# Reduce data
data_mpa <- data %>% 
  filter(mpa_yn==1)

data_control <- data %>% 
  filter(mpa_yn==0) %>% 
  sample_frac(size=0.05)

data_use <- bind_rows(data_mpa, data_control)



# Identify matches
################################################################################

# 1. Pre-matching covariate balance
# Good balance = SMD and eCDF close to zero, variance ratios close to one
pre_match <- matchit(data = data_use,
                     mpa_yn ~ shore_dist_km + depth_m + npeople_50km + park_dist_km + beach_dist_km + nparks_600m + nbeaches_600m,
                     method = NULL,
                     distance = "glm")
summary(pre_match)

# 2. Perform matching
matched <- matchit(data = data_use,
                   mpa_yn ~ shore_dist_km + depth_m + npeople_50km + park_dist_km + beach_dist_km + nparks_600m + nbeaches_600m,
                   ratio = 1, # match each treatment block with one control block
                   distance = "glm", # logistic
                   caliper = c(0.20), # sd 
                   std.caliper = T, 
                   mahvars = ~ shore_dist_km + depth_m + npeople_50km + park_dist_km + beach_dist_km + nparks_600m + nbeaches_600m,
                   replace = T)

# 3. Inspect match
summary(matched)
plot(matched)
plot(matched, type = "density")
plot(matched, type = "histogram")
print(matched)

# 4. Get matches
matches <- get_matches(matched, data = data_use)


# Export matches
################################################################################

# Save
save(matched, matches, data_use, file=file.path(outputdir, "counterfactual_output.Rds"))












