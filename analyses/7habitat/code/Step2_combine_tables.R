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
# Area of each habitat type stratified by depth and buffer
habitat <- readRDS(file.path(int.dir, "habitat_buffers_by_site_v3.Rds")) %>% # v2 has old depth, v3 only updated for KF
  dplyr::select(-habitat) %>% ungroup()

# Area of each habitat by buffer (across all depths)
habitat_combined <- readRDS(file.path(int.dir, "habitat_buffers_by_site_combined_v3.Rds")) %>% 
  dplyr::select(-habitat) %>% ungroup()

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
  left_join(habitat_combined) %>% 
  left_join(habitat_kelp) 

kelp <- kelp %>%
  filter(site != "SCAI_SHIP_ROCK") %>%  # too deep
  filter(site != "CASPAR_2") %>% 
  filter(site != "POINT_CABRILLO_2")

## Remove the extreme sites for kelp forest -----------------------------------------------

# Examine the range for each habitat characteristic
kelp_sites <- kelp %>% 
  dplyr::select(site, site_type, affiliated_mpa, hard_bottom_25:depth_cv_500) %>% distinct() %>% 
  pivot_longer(cols = hard_bottom_25:depth_cv_500, names_to = "habitat_variable", values_to = "value") %>% 
  filter(!str_detect(habitat_variable, "aquatic|soft|seagrass|depth_sd")) %>% 
  filter(!habitat_variable == "depth_cv_25") %>% 
  mutate(scale = as.numeric(str_extract(habitat_variable, "\\d+"))) %>% 
  mutate(habitat = str_remove(habitat_variable, "_\\d+")) %>% 
  arrange(desc(habitat), scale) %>% 
  mutate(habitat_variable = factor(habitat_variable, levels = unique(habitat_variable))) %>% 
  mutate(habitat_variable_label = paste0(str_replace_all(habitat_variable, "_", " ") %>% 
                                           str_to_sentence() %>% 
                                           str_replace_all("cv", "CV"), "m")) %>% 
  mutate(habitat_variable_label = factor(habitat_variable_label, levels = unique(habitat_variable_label)))

kelp_max <- kelp_sites %>% 
  group_by(site_type, habitat_variable, scale) %>% 
  summarize(max = max(value, na.rm = T), .groups = 'drop') %>% 
  pivot_wider(names_from = site_type, values_from = max) %>% 
  mutate(range_max = pmin(MPA, Reference),
         pct_diff = round(abs(MPA - Reference)/(0.5*(MPA + Reference))*100, 3))

# Add the values to the kelp_sites df for comparison
kelp_sites_max <- kelp_sites %>% 
  left_join(kelp_max %>% dplyr::select(habitat_variable, range_max))

# Flag the ones that are outside, calculate how far outside
kelp_flagged_max <- kelp_sites_max %>% 
  filter(value > range_max) %>% 
  mutate(pct_diff = round(abs(value - range_max)/(0.5*(value + range_max))*100, 3))

# Set filter to remove sites that are more than X% outside the range
kelp_remove <- kelp_flagged_max %>% 
  dplyr::select(site, site_type, affiliated_mpa, habitat_variable, value, range_max, pct_diff) %>% 
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

kelp <- kelp %>% 
  filter(!site %in% kelp_remove2$site) 

kelp_remove2 %>% 
  arrange(desc(pct_diff)) %>% 
  arrange(site) %>% 
  mutate(habitat_variable = str_replace_all(habitat_variable, "_", " ") %>% str_to_sentence() %>% str_replace_all("cv", "CV")) %>% 
  mutate(affiliated_mpa = str_to_title(affiliated_mpa) %>% str_replace_all("Smr", "SMR") %>% str_replace_all("Smca", "SMCA")) %>% 
  gt() %>% 
  cols_label(site = "Site",
             site_type = "Site Type",
             affiliated_mpa = "MPA",
             habitat_variable = "Variable",
             value = "Site Value",
             range_max = "Range Max Value",
             pct_diff = "% Difference") %>% 
  sub_missing(everything(), missing_text = "")

ggplot(data = kelp_sites %>% 
         filter(site %in% kelp$site) %>% 
         filter(affiliated_mpa %in% kelp$affiliated_mpa)) +
  geom_density(aes(x = value, color = site_type, fill = site_type), alpha = 0.3) + 
  scale_fill_manual(values = mpa_colors)+
  scale_color_manual(values = mpa_colors)+
  labs(x = "Value of habitat characteristic", y = "Density", fill = NULL, color = NULL)+
  my_theme+
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~habitat_variable_label, scales = "free", ncol = 5)

ggsave(file.path(fig.dir, "si-fig4-kelp-dist.png"), 
       width = 9, height = 6, dpi = 600, units = "in")


# Rock (CCFRP) ----------------------------------------------------------------------------------------------
rock_raw <- readRDS(file.path(ltm.dir, "rock_biomass_subset.Rds")) 

rock <- rock_raw %>% 
  left_join(habitat_combined) %>% 
  left_join(habitat_kelp)

# Examine the range for each habitat characteristic
rock_sites <- rock %>% 
  dplyr::select(site, site_type, affiliated_mpa, hard_bottom_25:depth_cv_500) %>% distinct() %>% 
  pivot_longer(cols = hard_bottom_25:depth_cv_500, names_to = "habitat_variable", values_to = "value") %>% 
  filter(!str_detect(habitat_variable, "aquatic|soft|seagrass|depth_sd")) %>% 
  filter(!habitat_variable == "depth_cv_25") %>% 
  mutate(scale = as.numeric(str_extract(habitat_variable, "\\d+"))) %>% 
  mutate(habitat = str_remove(habitat_variable, "_\\d+")) %>% 
  arrange(desc(habitat), scale) %>% 
  mutate(habitat_variable = factor(habitat_variable, levels = unique(habitat_variable))) %>% 
  mutate(habitat_variable_label = paste0(str_replace_all(habitat_variable, "_", " ") %>% 
                                           str_to_sentence() %>% 
                                           str_replace_all("cv", "CV"), "m")) %>% 
  mutate(habitat_variable_label = factor(habitat_variable_label, levels = unique(habitat_variable_label)))

rock_max <- rock_sites %>% 
  group_by(site_type, habitat_variable, scale) %>% 
  summarize(max = max(value, na.rm = T), .groups = 'drop') %>% 
  pivot_wider(names_from = site_type, values_from = max) %>% 
  mutate(range_max = pmin(MPA, Reference),
         pct_diff = round(abs(MPA - Reference)/(0.5*(MPA + Reference))*100, 3))

# Add the values to the sites df for comparison
rock_sites_max <- rock_sites %>% 
  left_join(rock_max %>% dplyr::select(habitat_variable, range_max))

# Flag the ones that are outside, calculate how far outside
rock_flagged_max <- rock_sites_max %>% 
  filter(value > range_max) %>% 
  mutate(pct_diff = round(abs(value - range_max)/(0.5*(value + range_max))*100, 3))

# Make final table for sites removed
rock_remove <- rock_flagged_max %>% 
  dplyr::select(site, site_type, affiliated_mpa, habitat_variable, value, range_max, pct_diff) %>% 
  filter(pct_diff > 30) 

rock <- rock %>% 
  filter(!site %in% rock_remove$site) 

rock_remove %>% 
  arrange(desc(pct_diff)) %>% 
  gt() %>% 
  cols_label(site = "Site",
             site_type = "Site Type",
             affiliated_mpa = "MPA",
             habitat_variable = "Variable",
             value = "Site Value",
             range_max = "Range Max Value",
             pct_diff = "% Difference")

ggplot(data = rock_sites %>% 
         filter(site %in% rock$site) %>% 
         filter(affiliated_mpa %in% rock$affiliated_mpa)) +
  geom_density(aes(x = value, color = site_type, fill = site_type), alpha = 0.3) + 
  scale_fill_manual(values = mpa_colors) +
  scale_color_manual(values = mpa_colors) +
  labs(x = "Value of habitat characteristic", y = "Density", fill = NULL, color = NULL)+
  my_theme + 
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~habitat_variable_label, scales = "free", ncol = 5)

ggsave(file.path(fig.dir, "si-fig4-rock-dist.png"), 
       width = 9, height = 6, dpi = 600, units = "in")

# Surf zone (seines) ----------------------------------------------------------------------------------------------

surf_raw <- readRDS(file.path(ltm.dir, "surf_biomass_subset.Rds")) 

surf <- surf_raw %>% 
  left_join(habitat_combined) %>% 
  left_join(habitat_kelp, by = c("year", "site", "site_type")) 

# Examine the range for each habitat characteristic
surf_sites <- surf %>%
  dplyr::select(site, site_type, affiliated_mpa, hard_bottom_25:depth_cv_500) %>% distinct() %>%
  pivot_longer(cols = hard_bottom_25:depth_cv_500, names_to = "habitat_variable", values_to = "value") %>%
  filter(!str_detect(habitat_variable, "aquatic|seagrass|depth_sd")) %>%
  filter(!habitat_variable == "depth_cv_25") %>%
  mutate(scale = as.numeric(str_extract(habitat_variable, "\\d+"))) %>%
  mutate(habitat = str_remove(habitat_variable, "_\\d+")) %>%
  arrange(desc(habitat), scale) %>%
  mutate(habitat_variable = factor(habitat_variable, levels = unique(habitat_variable))) %>% 
  mutate(habitat_variable_label = paste0(str_replace_all(habitat_variable, "_", " ") %>% 
                                           str_to_sentence() %>% 
                                           str_replace_all("cv", "CV"), "m")) %>% 
  mutate(habitat_variable_label = factor(habitat_variable_label, levels = unique(habitat_variable_label)))

surf_max <- surf_sites %>%
  group_by(site_type, habitat_variable, scale) %>%
  summarize(max = max(value, na.rm = T), .groups = 'drop') %>%
  pivot_wider(names_from = site_type, values_from = max) %>%
  mutate(range_max = pmin(MPA, Reference),
         pct_diff = round(abs(MPA - Reference)/(0.5*(MPA + Reference))*100, 3))

# Add the values to the sites df for comparison
surf_sites_max <- surf_sites %>% 
  left_join(surf_max %>% dplyr::select(habitat_variable, range_max))

# Flag the ones that are outside, calculate how far outside
surf_flagged_max <- surf_sites_max %>% 
  filter(value > range_max) %>% 
  mutate(pct_diff = round(abs(value - range_max)/(0.5*(value + range_max))*100, 3)) %>% 
  filter(pct_diff > 0) %>% arrange(pct_diff)

# Make final table for sites removed
surf_remove <- surf_flagged_max %>% 
  dplyr::select(site, site_type, affiliated_mpa, habitat_variable, value, range_max, pct_diff) %>% 
  filter(pct_diff > 35) 


surf_remove %>% 
  arrange(desc(pct_diff)) %>% 
  gt() %>% 
  cols_label(site = "Site",
             site_type = "Site Type",
             affiliated_mpa = "MPA",
             habitat_variable = "Variable",
             value = "Site Value",
             range_max = "Range Max Value",
             pct_diff = "% Difference")

ggplot(data = surf_sites) +
  geom_density(aes(x = value, color = site_type, fill = site_type), alpha = 0.3) + 
  scale_fill_manual(values = mpa_colors) +
  scale_color_manual(values = mpa_colors) +
  labs(x = "Value of habitat characteristic", y = "Density", fill = NULL, color = NULL)+
  my_theme + 
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~habitat_variable_label, scales = "free", ncol = 5)

ggsave(file.path(fig.dir, "si-fig4-surf-dist.png"), 
       width = 9, height = 9, dpi = 600, units = "in")


# Export 
saveRDS(kelp, file.path(ltm.dir, "combine_tables/kelp_full.Rds")) 
saveRDS(surf, file.path(ltm.dir, "combine_tables/surf_full.Rds")) 
saveRDS(rock, file.path(ltm.dir, "combine_tables/ccfrp_full.Rds")) 

