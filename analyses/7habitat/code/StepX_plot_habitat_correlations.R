

# Cori Lopazanski; lopazanski@bren.ucsb.edu
# July 2024 


# Setup   ----------------------------------------------------------------------
rm(list = ls())
gc()

library(tidyverse)
library(janitor)
library(sf)
library(dplyr)
library(purrr)
library(stringr)
library(corrr)

fig.dir <- "~/ca-mpa/analyses/7habitat/figures"
hab.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/combined/combined_mlpa_sites_1000m"
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data"
sp.dir <- "/home/shares/ca-mpa/data/sync-data/species_traits/processed"
int.dir <- "~/ca-mpa/analyses/7habitat/intermediate_data"


# Read  ----------------------------------------------------------------------
# Read the habitat table
habitat <- readRDS(file.path(int.dir, "habitat_buffers_by_site.Rds")) %>% 
  rename(affiliated_mpa = mpa_orig)

rownames(habitat) <- NULL


# Read the species table
sp_raw <- readRDS(file.path(sp.dir, "species_lw_habitat.Rds")) %>% 
  select(genus, sciname = species, common_name, target_status, vertical_zonation, min_max_m, common_m, 
         region, assemblage, assemblage_new)

# Read the kelp forest monitoring data
kelp_raw <- readRDS(file.path(ltm.dir, "biomass_site_year/kelp_biomass_site_year.Rds")) 
kelp_effort <- readRDS(file.path(ltm.dir, "biomass_site_year/kelp_site_year_effort.Rds")) %>% ungroup()

mpas <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_general.Rds") %>% 
  mutate(implementation_year = as.numeric(format(implementation_date,'%Y')))


# Build  ----------------------------------------------------------------------
kelp_sites <- kelp_raw %>% 
  distinct(year, site, bioregion, affiliated_mpa, site_type) %>% 
  mutate(affiliated_mpa = recode(affiliated_mpa, 
                                 "blue cavern smca" = "blue cavern onshore smca",
                                 "swamis smca" = "swami's smca")) %>% 
  left_join(mpas %>% select(bioregion, affiliated_mpa, implementation_year)) %>% 
  mutate(before = if_else(year <= implementation_year, 1, 0),
         after = if_else(year > implementation_year, 1, 0)) %>% 
  group_by(site, bioregion, affiliated_mpa, site_type) %>% 
  summarize(n_before = sum(before),
            n_after = sum(after),
            n_total = sum(n_before, n_after)) %>% 
  filter(n_after >= 5)


habitat <- habitat %>% 
  filter(habitat == "Kelp") %>% 
  filter(site %in% kelp_sites$site) 

correlation_matrix <- habitat %>%
  select(-c(area_depth_m2, area_site_m2, depth_zone)) %>% 
  pivot_wider(names_from = c(habitat_depth, buffer), values_from = area_m2, values_fill = list(area_m2 = 0)) %>% 
  select(-c(habitat, mpa, affiliated_mpa, site, site_type)) %>%
  select(where(~ var(.) != 0)) %>% 
  correlate() %>% 
  rearrange()

rplot(correlation_matrix %>% 
        shave()) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


cor_long <- correlation_matrix %>%
  stretch() %>%
  rename(Var1 = x, Var2 = y, value = r) %>% 
  mutate(buffer1 = sub(".*_", "", Var1),
         buffer2 = sub(".*_", "", Var2),
         habitat1 = sub("_[^_]*$", "", Var1),
         habitat2 = sub("_[^_]*$", "", Var2),
         depth1 = sub(".*_([^_]*)_[^_]*$", "\\1", Var1),
         depth2 = sub(".*_([^_]*)_[^_]*$", "\\1", Var2)) %>% 
  filter(buffer1 == buffer2) %>%
  mutate(buffer = as.numeric(buffer1)) %>%
  select(-buffer1, -buffer2) %>% 
  filter(!(habitat1 %in% c("biotic_0_30m", "biotic_30_100m",
                         "hard_bottom_landward", "soft_bottom_landward")),
         !(habitat2 %in% c("biotic_0_30m", "biotic_30_100m",
                           "hard_bottom_landward", "soft_bottom_landward")))

# Plot correlations using ggplot2 and facet by buffer
ggplot(data = cor_long, aes(x = habitat1, y = habitat2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Pearson\nCorrelation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 65, vjust = 1, 
                                   size = 10, hjust = 1)) +
  labs(x = "Habitat Type 1",
       y = "Habitat Type 2") +
  facet_wrap(~ buffer, scales = "free")


# Correlation matrix with total area ----------------------------------------------------------------------

correlation_matrix <- habitat %>%
  mutate(prop_site = area_m2/area_site_m2) %>% 
  select(-c(area_m2, area_depth_m2, area_site_m2, depth_zone)) %>% 
  pivot_wider(names_from = c(habitat_depth, buffer), values_from = prop_site, values_fill = list(prop_site = 0)) %>% 
  select(-c(habitat, mpa, affiliated_mpa, site, site_type)) %>%
  select(where(~ var(.) != 0)) %>% 
  correlate() %>% 
  rearrange()

cor_long <- correlation_matrix %>%
  stretch() %>%
  rename(Var1 = x, Var2 = y, value = r) %>% 
  mutate(buffer1 = sub(".*_", "", Var1),
         buffer2 = sub(".*_", "", Var2),
         habitat1 = sub("_[^_]*$", "", Var1),
         habitat2 = sub("_[^_]*$", "", Var2),
         depth1 = sub(".*_([^_]*)_[^_]*$", "\\1", Var1),
         depth2 = sub(".*_([^_]*)_[^_]*$", "\\1", Var2)) %>% 
  filter(buffer1 == buffer2) %>%
  mutate(buffer = as.numeric(buffer1)) %>%
  select(-buffer1, -buffer2) %>% 
  filter(!(habitat1 %in% c("biotic_0_30m", "biotic_30_100m",
                           "hard_bottom_landward", "soft_bottom_landward")),
         !(habitat2 %in% c("biotic_0_30m", "biotic_30_100m",
                           "hard_bottom_landward", "soft_bottom_landward")))

# Plot correlations using ggplot2 and facet by buffer
ggplot(data = cor_long, aes(x = habitat1, y = habitat2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Pearson\nCorrelation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 65, vjust = 1, 
                                   size = 10, hjust = 1)) +
  labs(x = "Habitat Type 1",
       y = "Habitat Type 2") +
  facet_wrap(~ buffer, scales = "free")

# Correlation matrix with area in depth zone --------------------------------------------------------------------

correlation_matrix <- habitat %>%
  mutate(prop_depth = if_else((area_m2 == 0 & area_depth_m2 == 0), 0, area_m2/area_depth_m2)) %>% 
  select(-c(area_m2, area_depth_m2, area_site_m2, depth_zone)) %>% 
  pivot_wider(names_from = c(habitat_depth, buffer), values_from = prop_depth, values_fill = list(prop_depth = 0)) %>% 
  select(-c(habitat, mpa, affiliated_mpa, site, site_type)) %>%
  select(where(~ var(.) != 0)) %>% 
  correlate() %>% 
  rearrange()

cor_long <- correlation_matrix %>%
  stretch() %>%
  rename(Var1 = x, Var2 = y, value = r) %>% 
  mutate(buffer1 = sub(".*_", "", Var1),
         buffer2 = sub(".*_", "", Var2),
         habitat1 = sub("_[^_]*$", "", Var1),
         habitat2 = sub("_[^_]*$", "", Var2),
         depth1 = sub(".*_([^_]*)_[^_]*$", "\\1", Var1),
         depth2 = sub(".*_([^_]*)_[^_]*$", "\\1", Var2)) %>% 
  filter(buffer1 == buffer2) %>%
  mutate(buffer = as.numeric(buffer1)) %>%
  select(-buffer1, -buffer2) %>% 
  filter(!(habitat1 %in% c("biotic_0_30m", "biotic_30_100m",
                           "hard_bottom_landward", "soft_bottom_landward")),
         !(habitat2 %in% c("biotic_0_30m", "biotic_30_100m",
                           "hard_bottom_landward", "soft_bottom_landward")))

# Plot correlations using ggplot2 and facet by buffer
ggplot(data = cor_long, aes(x = habitat1, y = habitat2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Pearson\nCorrelation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 65, vjust = 1, 
                                   size = 10, hjust = 1)) +
  labs(x = "Habitat Type 1",
       y = "Habitat Type 2") +
  facet_wrap(~ buffer, scales = "free")

# Plot site visitation ----
kelp_sites <- kelp_raw %>% 
  distinct(year, site, bioregion, affiliated_mpa, site_type) %>% 
  mutate(affiliated_mpa = recode(affiliated_mpa, 
                                 "blue cavern smca" = "blue cavern onshore smca",
                                 "swamis smca" = "swami's smca")) %>% 
  left_join(mpas %>% select(bioregion, affiliated_mpa, implementation_year)) %>% 
  mutate(before = if_else(year <= implementation_year, 1, 0),
         after = if_else(year > implementation_year, 1, 0)) %>% 
  group_by(site, bioregion, affiliated_mpa, site_type) %>% 
  summarize(n_before = sum(before),
            n_after = sum(after),
            n_total = sum(n_before, n_after)) 

ggplot(data = kelp_sites %>% filter(!is.na(site_type))) +
  geom_jitter(aes(x = n_before, y = n_after, color = bioregion, shape = site_type),
              width = 0.25, height = 0.25) + 
  labs(x = "# years sampled before MPA implemented",
       y = "# years sampled after MPA implemented",
       color = NULL,
       shape = NULL) +
  theme_minimal()


# Generate a data frame with cumulative counts for each year cutoff
binned_sites <- kelp_sites %>%
  group_by(n_before, n_after) %>% 
  summarize(n = n()) %>% 
  mutate(n_binned = cut(n, breaks = c(1, 5, 10, 15, 20, 25, 31), 
                        labels = c("1-5", "6-10", "11-15", "16-20", "21-25", "26-31"), 
                        include.lowest = TRUE, right = TRUE))

# Define the color palette
color_palette <- c("1-5" = "#bdd7e7",   
                   "6-10" = "#6baed6", 
                   "11-15" = "#3182bd", 
                   "16-20" = "#08519c", 
                   "21-25" = "#08306b", 
                   "26-31" = "#041d42")

ggplot(data = binned_sites) +
  geom_tile(aes(x = n_before, y = n_after, fill = n_binned)) +
  scale_fill_manual(values = color_palette) +
  labs(x = "# years sampled before MPA implemented",
       y = "# years sampled after MPA implemented",
       fill = "Number of sites") +
  theme_minimal()





# Pheatmap pacakge ----
library(pheatmap)

cor <- habitat %>%
  filter(site %in% kelp_sites$site) %>% 
  filter(buffer == 100) %>% 
  filter(!(depth_zone == "landward")) %>% 
  mutate(prop_depth = if_else((area_m2 == 0 & area_depth_m2 == 0), 0, area_m2/area_depth_m2)) %>% 
  select(-c(area_m2, area_depth_m2, area_site_m2, depth_zone)) %>% 
  pivot_wider(names_from = c(habitat_depth, buffer), values_from = prop_depth, values_fill = list(prop_depth = 0)) %>% 
  select(-c(habitat, mpa, affiliated_mpa, site, site_type)) %>%
  select(where(~ var(.) != 0)) %>% 
  cor()

pheatmap(cor, angle_col = 45)
