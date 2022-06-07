################################################################################
# CDFW MPA Attributes Data 
#      1. Process Raw Data 
#      2. PCA Visualization (this script)
#           Input:  cdfw_mpa_attributes_clean.csv
#           Output: figures
################################################################################

# Packages
library(tidyverse)
library(GGally)

# Directories
base.dir <- here::here("data", "cdfw_mpa_attributes")
in.dir <- here::here(base.dir, "processed")
plot.dir <- here::here(base.dir, "figures")

# Data
attributes <- read_csv(file.path(in.dir, "cdfw_mpa_attributes_clean.csv"))

# Minor Processing
data <- attributes %>% 
  mutate(four_region_north_ci = as.factor(four_region_north_ci)) %>% 
  mutate(prop_hard = total_hard_substrate/size_km2) %>% 
  mutate(prop_soft = total_soft_substrate/size_km2)

  
################################################################################
# A. Raw Rock, Raw Sand, Depth Range, Size
################################################################################
# Create dataframe with variables of interest
a_data <- data %>% 
  select(four_region_north_ci,
         total_hard_substrate, 
         total_soft_substrate,
         depth_range, 
         size_km2) %>% 
  drop_na() 

# Show pairwise correlation between variables
ggpairs(a_data[2:5], 
        aes(col = a_data$four_region_north_ci, alpha = 0.8))

# Pairwise scatterplots suggest size and soft are correlated; drop size?
ggpairs(a_data[2:4], 
        aes(col = a_data$four_region_north_ci, alpha = 0.8))

a_pca <- princomp(a_data[2:4])

# Plot
ggbiplot(a_pca, group = a_data$four_region_north_ci, ellipse = T) +
  theme_minimal()

# Depth range is 100% of explained variance

################################################################################
# B. Proportion Rock, Proportion Sand, Depth Range
################################################################################
b_data <- data %>% 
  select(four_region_north_ci,
         prop_hard,
         prop_soft,
         depth_range) %>% 
  drop_na()

ggpairs(b_data[2:4], aes(col = b_data$four_region_north_ci, alpha = 0.8))

b_pca <- princomp(b_data[2:4])

ggbiplot(b_pca, 
         group = b_data$four_region_north_ci, ellipse = T) +
  labs(color = NULL) +
  theme_minimal() +
  theme(legend.direction = "horizontal", legend.position = "top", aspect.ratio = 1)

# Depth range still 100% of explained variance


################################################################################
# C. Habitat Groups, Size, Depth Range?
################################################################################
c_data <- data %>% 
  select(four_region_north_ci,
         sandy_beach_km:max_kelp_canopy_cdfw_km2,
         prop_hard, 
         prop_soft, 
         depth_range,
         size_km2) %>% 
  drop_na()

ggpairs(c_data[2:9], aes(col = c_data$four_region_north_ci, alpha = 0.8))

c_pca <- princomp(c_data[2:9])

ggbiplot(c_pca,
         groups = c_data$four_region_north_ci,
         ellipse = TRUE,
         circle = FALSE,
         # draw ellipse around points (+/-) 1 standard deviation
         ellipse.prob = 0.68) +
  theme_minimal()+
  labs(color = NULL)+
  theme(legend.direction = "horizontal", legend.position = "top", aspect.ratio = 1)

# Chaos

################################################################################
# D. Just Habitat Groups + Depth
################################################################################
d_data <- data %>% 
  select(four_region_north_ci,
         sandy_beach_km:max_kelp_canopy_cdfw_km2, 
         depth_range) %>% 
  drop_na()

ggpairs(d_data[2:6], aes(col = d_data$four_region_north_ci, alpha = 0.8))

d_pca <- princomp(d_data[2:6])

ggbiplot(d_pca,
         groups = d_data$four_region_north_ci,
         ellipse = TRUE,
         circle = FALSE,
         # draw ellipse around points (+/-) 1 standard deviation
         ellipse.prob = 0.68) +
  theme_minimal()+
  labs(color = NULL)+
  theme(legend.direction = "horizontal", legend.position = "top", aspect.ratio = 1)

# Depth is everything?

################################################################################
# E. Just Habitat Groups
################################################################################
e_data <- data %>% 
  select(four_region_north_ci,
         sandy_beach_km:max_kelp_canopy_cdfw_km2) %>% 
  drop_na()

ggpairs(e_data[2:5], aes(col = e_data$four_region_north_ci, alpha = 0.8))

e_pca <- princomp(e_data[2:5])

ggbiplot(e_pca,
         groups = e_data$four_region_north_ci,
         ellipse = TRUE,
         circle = FALSE,
         # draw ellipse around points (+/-) 1 standard deviation
         ellipse.prob = 0.68) +
  theme_minimal()+
  labs(color = NULL)+
  theme(legend.direction = "horizontal", legend.position = "top", aspect.ratio = 1)


# Depth is everything?



