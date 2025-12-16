# Cori Lopazanski
# August 2024

# About ------------------------------------------------------------------------------------

# Merge the species, habitat, and monitoring tables into one df for models

# Setup -------------------------------------------------------------------------------------------------------------------------
library(tidyverse) 
library(gt)

rm(list = ls())

# Directories
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024/2025"
sp.dir <- "/home/shares/ca-mpa/data/sync-data/species_traits/processed"
int.dir <- "~/ca-mpa/analyses/7habitat/intermediate_data"
kw.dir <- "/home/shares/ca-mpa/data/sync-data/kelpwatch/2024/processed"

# Read Data --------------------------------------------------------------------------------------------------------------------
# Area of each habitat by buffer (across all depths)
habitat_combined <- readRDS(file.path(int.dir, "habitat_buffers_by_site_combined_v3.Rds")) 

# Annual kelp canopy cover
habitat_kelp <- readRDS(file.path(kw.dir, "kelp_site_buffers.Rds")) %>% 
  dplyr::select(-habitat, -site_id) %>% 
  distinct() %>% 
  mutate(year = as.numeric(year))

# Add plotting details
fig.dir <- "~/ca-mpa/analyses/7habitat/figures"

my_theme <- theme_minimal(base_family = "Arial") + 
  theme(plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 8),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.margin = margin(t = 0, unit='cm'),
        plot.caption = element_text(size = 8),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 8, face = "bold"),
        panel.background = element_rect(fill = "white", color = NA),  
        plot.background = element_rect(fill = "white", color = NA),
        legend.position = "top",
        panel.spacing = unit(1, "lines"),
        legend.key.size = unit(1, "lines"))

mpa_colors <- c("Reference" = "#6d55aa", "MPA" = "#c42119")

# Kelp -------------------------------------------------------------------------------------------------------------------

kelp_raw <- readRDS(file.path(ltm.dir, "kelp_biomass_subset.Rds")) 

kelp <- kelp_raw %>%
  left_join(habitat_combined, by = c("site", "site_type")) %>% 
  left_join(habitat_kelp, by = c("year", "site", "site_type")) 

kelp <- kelp %>%
  filter(site != "SCAI_SHIP_ROCK") # %>%  # too deep
 # filter(site != "CASPAR_2") %>% 
 # filter(site != "POINT_CABRILLO_2")

## Remove the extreme sites for kelp forest -----------------------------------------------

# Examine the range for each habitat characteristic
kelp_sites <- kelp %>% 
  dplyr::select(habitat, site, site_type, affiliated_mpa, all_of(names(habitat_combined))) %>% distinct() %>% 
  pivot_longer(cols = hard_bottom_25:slope_sd_500, names_to = "habitat_var", values_to = "value") %>% 
  filter(!str_detect(habitat_var, "aquatic|soft")) %>% 
  mutate(scale = as.numeric(str_extract(habitat_var, "\\d+"))) %>% 
  mutate(habitat = str_remove(habitat_var, "_\\d+")) %>% 
  arrange(desc(habitat), scale) %>% 
  mutate(habitat_var = factor(habitat_var, levels = unique(habitat_var))) %>% 
  mutate(habitat_label = paste0(str_replace_all(habitat_var, "_", " ") %>% str_to_sentence() %>% str_replace_all("cv", "CV") %>% str_replace_all("Tri", "TRI"), "m")) %>% 
  mutate(habitat_label = factor(habitat_label, levels = unique(habitat_label))) %>% 
  dplyr::bind_rows(tibble(habitat_var = c("depth_cv_25", "tri_mean_25", "slope_mean_25", "slope_sd_25",
                                          "depth_cv_50", "tri_mean_50", "slope_mean_50", "slope_sd_50"),
                          scale       = c(25, 25, 25, 25, 50, 50, 50, 50),
                          value       = NA_real_,
                          site_type   = NA_character_)) %>% 
  mutate(habitat_cat = factor(case_when(str_detect(habitat_var, "tri") ~ "TRI",
                                        str_detect(habitat_var, "hard") ~ "Hard bottom",
                                        str_detect(habitat_var, "soft") ~ "Soft bottom",
                                        str_detect(habitat_var, "depth_mean") ~ "Depth mean",
                                        str_detect(habitat_var, "depth_cv") ~ "Depth CV",
                                        str_detect(habitat_var, "slope_mean") ~ "Slope mean",
                                        str_detect(habitat_var, "slope_sd") ~ "Slope sd",
                                        T~habitat_var), 
                              levels = c("Hard bottom", "Soft bottom", "Depth mean", "Depth CV", "TRI", "Slope mean", "Slope sd")))


kelp_max <- kelp_sites %>% filter(!is.na(site_type)) %>% 
  group_by(site_type, habitat_var, scale) %>% 
  summarize(max = max(value, na.rm = T), .groups = 'drop') %>% 
  pivot_wider(names_from = site_type, values_from = max) %>% 
  mutate(range_max = pmin(MPA, Reference),
         pct_diff = round(abs(MPA - Reference)/(0.5*(MPA + Reference))*100, 3))

# Add the values to the kelp_sites df for comparison
kelp_sites_max <- kelp_sites %>% 
  left_join(kelp_max %>% dplyr::select(habitat_var, range_max))

# Flag the ones that are outside, calculate how far outside
kelp_flagged_max <- kelp_sites_max %>% 
  filter(value > range_max) %>% 
  mutate(pct_diff = round(abs(value - range_max)/(0.5*(value + range_max))*100, 3))

# Set filter to remove sites that are more than X% outside the range
kelp_remove <- kelp_flagged_max %>% 
  dplyr::select(site, site_type, affiliated_mpa, habitat_var, value, range_max, pct_diff) %>% 
  filter(pct_diff > 30)

# Check balance among MPA/REF for MPAs
kelp_balance <- kelp %>% 
  filter(!site %in% kelp_remove$site) %>% 
  group_by(affiliated_mpa, site_type) %>% 
  summarise(n = n_distinct(site), .groups = "drop") %>%
  count(affiliated_mpa) %>%
  filter(n < 2)

kelp_remove2 <- kelp_remove %>% 
  bind_rows(kelp_sites %>%
              filter(affiliated_mpa %in% kelp_balance$affiliated_mpa) %>%
              distinct(site, site_type, affiliated_mpa) %>%
              filter(!site %in% kelp_remove$site))

length(unique(kelp_remove2$site))

#kelp <- kelp %>% 
#  filter(!site %in% kelp_remove2$site) 

kelp_remove2 %>% 
  arrange(desc(pct_diff)) %>% 
  arrange(site) %>% 
  mutate(habitat_var = str_replace_all(habitat_var, "_", " ") %>% 
           str_to_sentence() %>%   str_replace_all("cv", "CV") %>%  str_replace_all("Tri", "TRI")) %>% 
  mutate(affiliated_mpa = str_to_title(affiliated_mpa) %>% str_replace_all("Smr", "SMR") %>% str_replace_all("Smca", "SMCA")) %>% 
  gt() %>% 
  cols_label(site = "Site",
             site_type = "Site Type",
             affiliated_mpa = "MPA",
             habitat_var = "Variable",
             value = "Site Value",
             range_max = "Range Max Value",
             pct_diff = "% Difference") %>% 
  sub_missing(everything(), missing_text = "")

ggplot(data = kelp_sites %>% 
         filter(site %in% kelp$site | is.na(site)) %>% 
         filter(affiliated_mpa %in% kelp$affiliated_mpa | is.na(affiliated_mpa))) +
  geom_density(aes(x = value, color = site_type, fill = site_type), alpha = 0.3) + 
  geom_blank() +
  scale_fill_manual(values = mpa_colors)+
  scale_color_manual(values = mpa_colors)+
  labs(x = "Value of habitat characteristic", y = "Density", fill = NULL, color = NULL)+
  my_theme +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ habitat_cat + scale,
    nrow = length(unique(kelp_sites$habitat_cat)),
    ncol = length(unique(kelp_sites$scale)),
    scales = "free")

ggsave(file.path(fig.dir, "si-fig4-kelp-dist.png"), 
       width = 9, height = 6, dpi = 600, units = "in")


# Rock (CCFRP) ----------------------------------------------------------------------------------------------
rock_raw <- readRDS(file.path(ltm.dir, "rock_biomass_subset.Rds")) 

rock <- rock_raw %>% 
  left_join(habitat_combined) %>% 
  left_join(habitat_kelp, by = c("year", "site", "site_type"))

# Examine the range for each habitat characteristic
rock_sites <- rock %>% 
  dplyr::select(site, site_type, affiliated_mpa, all_of(names(habitat_combined))) %>% distinct() %>% 
  pivot_longer(cols = hard_bottom_25:slope_sd_500, names_to = "habitat_var", values_to = "value") %>% 
  filter(!str_detect(habitat_var, "aquatic|soft|seagrass|depth_sd")) %>% 
  mutate(scale = as.numeric(str_extract(habitat_var, "\\d+"))) %>% 
  mutate(habitat = str_remove(habitat_var, "_\\d+")) %>% 
  arrange(desc(habitat), scale) %>% 
  mutate(habitat_var = factor(habitat_var, levels = unique(habitat_var)))  %>% 
  dplyr::bind_rows(tibble(habitat_var = c("depth_cv_25", "tri_mean_25", "slope_mean_25", "slope_sd_25",
                                          "depth_cv_50", "tri_mean_50", "slope_mean_50", "slope_sd_50"),
                          scale       = c(25, 25, 25, 25, 50, 50, 50, 50),
                          value       = NA_real_,
                          site_type   = NA_character_)) %>% 
  mutate(habitat_cat = factor(case_when(str_detect(habitat_var, "tri") ~ "TRI",
                                        str_detect(habitat_var, "hard") ~ "Hard bottom",
                                        str_detect(habitat_var, "soft") ~ "Soft bottom",
                                        str_detect(habitat_var, "depth_mean") ~ "Depth mean",
                                        str_detect(habitat_var, "depth_cv") ~ "Depth CV",
                                        str_detect(habitat_var, "slope_mean") ~ "Slope mean",
                                        str_detect(habitat_var, "slope_sd") ~ "Slope sd",
                                        T~habitat_var), 
                              levels = c("Hard bottom", "Soft bottom", "Depth mean", "Depth CV", "TRI", "Slope mean", "Slope sd")))


rock_max <- rock_sites %>% filter(!is.na(site_type)) %>% 
  group_by(site_type, habitat_var, scale) %>% 
  summarize(max = max(value, na.rm = T), .groups = 'drop') %>% 
  pivot_wider(names_from = site_type, values_from = max) %>% 
  mutate(range_max = pmin(MPA, Reference),
         pct_diff = round(abs(MPA - Reference)/(0.5*(MPA + Reference))*100, 3))

# Add the values to the sites df for comparison
rock_sites_max <- rock_sites %>% 
  left_join(rock_max %>% dplyr::select(habitat_var, range_max))

# Flag the ones that are outside, calculate how far outside
rock_flagged_max <- rock_sites_max %>% 
  filter(value > range_max) %>% 
  mutate(pct_diff = round(abs(value - range_max)/(0.5*(value + range_max))*100, 3))

# Make final table for sites removed
rock_remove <- rock_flagged_max %>% 
  dplyr::select(site, site_type, affiliated_mpa, habitat_var, value, range_max, pct_diff) %>% 
  filter(pct_diff > 30) 

# rock <- rock %>% 
#   filter(!site %in% rock_remove$site) 

rock_remove %>% 
  arrange(desc(pct_diff)) %>% 
  gt() %>% 
  cols_label(site = "Site",
             site_type = "Site Type",
             affiliated_mpa = "MPA",
             habitat_var = "Variable",
             value = "Site Value",
             range_max = "Range Max Value",
             pct_diff = "% Difference")

ggplot(data = rock_sites) +
  geom_density(aes(x = value, color = site_type, fill = site_type), alpha = 0.3) + 
  geom_blank() +
  scale_fill_manual(values = mpa_colors) +
  scale_color_manual(values = mpa_colors) +
  labs(x = "Value of habitat characteristic", y = "Density", fill = NULL, color = NULL)+
  my_theme + 
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ habitat_cat + scale,
             nrow = length(unique(rock_sites$habitat_cat)),
             ncol = length(unique(rock_sites$scale)),
             scales = "free")


ggsave(file.path(fig.dir, "si-fig4-rock-dist.png"), 
       width = 9, height = 6, dpi = 600, units = "in")

# Surf zone (seines) ----------------------------------------------------------------------------------------------

surf_raw <- readRDS(file.path(ltm.dir, "surf_biomass_subset.Rds")) 

surf <- surf_raw %>% 
  left_join(habitat_combined) %>% 
  left_join(habitat_kelp, by = c("year", "site", "site_type")) 

# Examine the range for each habitat characteristic
surf_sites <- surf %>%
  dplyr::select(site, site_type, affiliated_mpa, all_of(names(habitat_combined))) %>% distinct() %>%
  pivot_longer(cols = hard_bottom_25:slope_sd_500, names_to = "habitat_var", values_to = "value") %>%
  filter(!str_detect(habitat_var, "seagrass|depth_sd")) %>%
  mutate(scale = as.numeric(str_extract(habitat_var, "\\d+"))) %>%
  mutate(habitat = str_remove(habitat_var, "_\\d+")) %>%
  arrange(desc(habitat), scale) %>%
  mutate(habitat_var = factor(habitat_var, levels = unique(habitat_var))) %>% 
  dplyr::bind_rows(tibble(habitat_var = c("depth_cv_25", "tri_mean_25", "slope_mean_25", "slope_sd_25",
                                          "depth_cv_50", "tri_mean_50", "slope_mean_50", "slope_sd_50"),
                          scale       = c(25, 25, 25, 25, 50, 50, 50, 50),
                          value       = NA_real_,
                          site_type   = NA_character_)) %>% 
  mutate(habitat_cat = factor(case_when(str_detect(habitat_var, "tri") ~ "TRI",
                                        str_detect(habitat_var, "hard") ~ "Hard bottom",
                                        str_detect(habitat_var, "soft") ~ "Soft bottom",
                                        str_detect(habitat_var, "aquatic") ~ "Max biotic",
                                        str_detect(habitat_var, "depth_mean") ~ "Depth mean",
                                        str_detect(habitat_var, "depth_cv") ~ "Depth CV",
                                        str_detect(habitat_var, "slope_mean") ~ "Slope mean",
                                        str_detect(habitat_var, "slope_sd") ~ "Slope sd",
                                        T~habitat_var), 
                              levels = c("Hard bottom", "Soft bottom", "Max biotic", "Depth mean", "Depth CV", "TRI", "Slope mean", "Slope sd")))

surf_max <- surf_sites %>% filter(!is.na(site_type)) %>% 
  group_by(site_type, habitat_var, scale) %>%
  summarize(max = max(value, na.rm = T), .groups = 'drop') %>%
  pivot_wider(names_from = site_type, values_from = max) %>%
  mutate(range_max = pmin(MPA, Reference),
         pct_diff = round(abs(MPA - Reference)/(0.5*(MPA + Reference))*100, 3))

# Add the values to the sites df for comparison
surf_sites_max <- surf_sites %>% 
  left_join(surf_max %>% dplyr::select(habitat_var, range_max))

# Flag the ones that are outside, calculate how far outside
surf_flagged_max <- surf_sites_max %>% 
  filter(value > range_max) %>% 
  mutate(pct_diff = round(abs(value - range_max)/(0.5*(value + range_max))*100, 3)) %>% 
  filter(pct_diff > 0) %>% arrange(pct_diff)

# Make final table for sites removed
surf_remove <- surf_flagged_max %>% 
  dplyr::select(site, site_type, affiliated_mpa, habitat_var, value, range_max, pct_diff) %>% 
  filter(pct_diff > 35) 


surf_remove %>% 
  arrange(desc(pct_diff)) %>% 
  gt() %>% 
  cols_label(site = "Site",
             site_type = "Site Type",
             affiliated_mpa = "MPA",
             habitat_var = "Variable",
             value = "Site Value",
             range_max = "Range Max Value",
             pct_diff = "% Difference")

ggplot(data = surf_sites) +
  geom_density(aes(x = value, color = site_type, fill = site_type), alpha = 0.3) + 
  geom_blank() +
  scale_fill_manual(values = mpa_colors) +
  scale_color_manual(values = mpa_colors) +
  labs(x = "Value of habitat characteristic", y = "Density", fill = NULL, color = NULL)+
  my_theme + 
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(
    ~ habitat_cat + scale,
    nrow = length(unique(surf_sites$habitat_cat)),
    ncol = length(unique(surf_sites$scale)),
    scales = "free"
  )
  #facet_wrap(~habitat_var_label, scales = "free", ncol = 5)

ggsave(file.path(fig.dir, "si-fig4-surf-dist.png"), 
       width = 9, height = 9, dpi = 600, units = "in")


# Export 
saveRDS(kelp, file.path(ltm.dir, "combine_tables/kelp_full.Rds")) 
saveRDS(surf, file.path(ltm.dir, "combine_tables/surf_full.Rds")) 
saveRDS(rock, file.path(ltm.dir, "combine_tables/ccfrp_full.Rds")) 

