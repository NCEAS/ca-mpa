# Habitat Diversity

# Setup ------------------------------------------------------------------------
# Packages
library(tidyverse)
library(vegan)

# Clear workspace
rm(list = ls())

# Directories
base.dir <- "/Volumes/GoogleDrive-105151121202188525604/Shared drives/NCEAS MPA network assessment/MPA Network Assessment: Working Group Shared Folder/data/sync-data" # Cori Local
data.dir <- file.path(getwd(), "analyses", "7habitat", "intermediate_data")

# Read Attribute (Habitat) Data
hab_raw <- readRDS(file.path(data.dir, "mpa_attributes_processed.Rds")) %>% 
  filter(!name == "Moro Cojo Slough SMR") %>%   # We know this is wrong 
  filter(!name == "Painted Cave SMCA") %>% 
  filter(!name == "Anacapa Island SMCA") %>% 
  filter(!name == "Anacapa Island SMR") %>%
  filter(!name == "Judith Rock SMR") %>% 
  filter(!name == "South La Jolla SMCA") %>% 
  filter(!name == "Richardson Rock SMR")
  
  
# Build ------------------------------------------------------------------------
## Create habitat matrix
hab <- hab_raw %>% 
  select(name, hard_substrate_0_30m_km2_comb:hardened_armored_shore_km) %>% 
  column_to_rownames("name") 

hab[is.na(hab)] <- 0

## Metadata matrix for each MPA
hab_type <- hab_raw %>% 
  select(name:mpa_habitat_type)

linear_habitats <- c("sandy_beach_km",
                     "rocky_inter_km",
                     "coastal_marsh_km",
                     "tidal_flats_km",
                     "hardened_armored_shore_km")

## Convert length to area 
hab_adj <- hab %>% 
  mutate(across(all_of(linear_habitats), ~.x*0.003))

## Calculate proportion rock
prop_rock <- hab_adj %>% 
  mutate(total_area_km2 = rowSums(across(everything()))) %>% 
  select(total_area_km2, hard_substrate_0_30m_km2_comb, hard_substrate_30_100m_km2,
         hard_substrate_100_200m_km2, hard_substrate_200_3000m_km2, 
         rocky_inter_km) %>% 
  mutate(total_rock_km2 = rowSums(across(c(hard_substrate_0_30m_km2_comb, 
                                       hard_substrate_30_100m_km2,
                                       hard_substrate_100_200m_km2, 
                                       hard_substrate_200_3000m_km2, 
                                       rocky_inter_km)))) %>% 
  mutate(prop_rock = total_rock_km2/total_area_km2) %>% 
  rownames_to_column("name") %>% 
  select(name, total_area_km2, total_rock_km2, prop_rock) %>% 
  left_join(hab_type %>% select(name, size_km2))

## Export proportion rock at bottom


## Richness --------------------------------------------------------------------

## Calculate habitat richness for each MPA
habr <- specnumber(hab)

habr_df <- habr %>% 
  enframe() %>% 
  full_join(hab_type)

### Among MPA types? Yes <0.0001 ----
habr_aov <- aov(habr ~ mpa_habitat_type, data = hab_type)
summary(habr_aov)


ggplot(habr_df, aes(x = mpa_habitat_type, y = value, fill = mpa_habitat_type)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 13.5), expand = c(0,0))+
  theme_bw()+
  labs(x = NULL,
       y = "Habitat richness",
       fill = "Classification")

### Among 4 regions? Yes 0.00516 ----
habr_aov2 <- aov(habr ~ four_region_north_ci, data = hab_type)
summary(habr_aov2)

ggplot(habr_df, aes(x = four_region_north_ci, y = value, fill = four_region_north_ci)) +
  geom_boxplot() +
  theme_bw()+
  scale_y_continuous(limits = c(0, 13.5), expand = c(0,0))+
  labs(x = NULL,
       y = "Habitat richness",
       fill = "Region")

### Among 3 regions? Yes 0.00193 ----
habr_aov3 <- aov(habr ~ bioregion, data = hab_type)
summary(habr_aov3)

ggplot(habr_df, aes(x = bioregion, y = value, fill = bioregion)) +
  geom_boxplot() +
  theme_bw()+
  scale_y_continuous(limits = c(0, 13.5), expand = c(0,0))+
  labs(x = NULL,
       y = "Habitat richness",
       fill = "Region")

## Evenness --------------------------------------------------------------------



## Diversity -------------------------------------------------------------------

habd <- diversity(hab_adj)

habd_df <- habd %>% 
  enframe() %>% 
  full_join(hab_type)

### Among MPA types? Yes <0.0001 ----
habd_aov <- aov(habd ~ mpa_habitat_type, data = hab_type)
summary(habd_aov)
# p < 0.001

ggplot(habd_df, aes(x = mpa_habitat_type, y = value, fill = mpa_habitat_type)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 2.1), expand = c(0,0))+
  theme_bw()+
  labs(x = NULL,
       y = "Habitat Diversity",
       fill = "Classification")

### Among 4 regions?  No 0.078 ----
habd_aov2 <- aov(habd ~ four_region_north_ci, data = hab_type)
summary(habd_aov2)

ggplot(habd_df, aes(x = four_region_north_ci, y = value, fill = four_region_north_ci)) +
  geom_boxplot() +
  theme_bw()+
  scale_y_continuous(limits = c(0, 2.15), expand = c(0,0))+
  labs(x = NULL,
       y = "Habitat Diversity",
       fill = "Region")

### Among 3 regions? No 0.0521 ----
habd_aov3 <- aov(habd ~ bioregion, data = hab_type)
summary(habd_aov3)

ggplot(habd_df, aes(x = bioregion, y = value, fill = bioregion)) +
  geom_boxplot() +
  theme_bw()+
  scale_y_continuous(limits = c(0, 2.15), expand = c(0,0))+
  labs(x = NULL,
       y = "Habitat Diversity",
       fill = "Region")

# Coastal Only -----------------------------------------------------------------
hab_coast <- hab %>% 
  filter(rownames(.) %in% hab_type$name[hab_type$mpa_habitat_type == "Coastal"])

coast_type <- hab_type %>% 
  filter(name %in% rownames(hab_coast))

## Richness ----
coastr <- specnumber(hab_coast)

coastr_df <- coastr %>% 
  enframe() %>% 
  full_join(coast_type)


### Among 4 regions? Yes <0.0001 ----
coastr_aov <- aov(coastr ~ four_region_north_ci, data = coast_type)
summary(coastr_aov)

ggplot(coastr_df, aes(x = four_region_north_ci, y = value, fill = four_region_north_ci)) +
  geom_boxplot() +
  theme_bw()+
  scale_y_continuous(limits = c(0, 13.5), expand = c(0,0))+
  labs(x = NULL,
       y = "Habitat richness",
       fill = "Region")

### Among 3 regions? Yes <0.0001 ----
coastr_aov2 <- aov(coastr ~ bioregion, data = coast_type)
summary(coastr_aov2)

ggplot(coastr_df, aes(x = bioregion, y = value, fill = bioregion)) +
  geom_boxplot() +
  theme_bw()+
  scale_y_continuous(limits = c(0, 13.5), expand = c(0,0))+
  labs(x = NULL,
       y = "Habitat richness",
       fill = "Region")

## Diversity ----

coastd <- diversity(hab_adj %>% filter(rownames(.) %in% rownames(hab_coast)))

coastd_df <- coastd %>% 
  enframe() %>% 
  full_join(coast_type)

### Among 4 regions?  Yes <0.0001 ----
coastd_aov <- aov(coastd ~ four_region_north_ci, data = coast_type)
summary(coastd_aov)

ggplot(coastd_df, 
       aes(x = four_region_north_ci, y = value, 
           fill = four_region_north_ci)) +
  geom_boxplot() +
  theme_bw()+
  scale_y_continuous(limits = c(0, 2.15), expand = c(0,0))+
  labs(x = NULL,
       y = "Habitat Diversity",
       fill = "Region")

### Among 3 regions? Yes 0.000113 ----
coastd_aov2 <- aov(coastd ~ bioregion, data = coast_type)
summary(coastd_aov2)

ggplot(coastd_df, aes(x = bioregion, y = value, fill = bioregion)) +
  geom_boxplot() +
  theme_bw()+
  scale_y_continuous(limits = c(0, 2.25), expand = c(0,0))+
  labs(x = NULL,
       y = "Habitat Diversity",
       fill = "Region")

## Relationship with MPA Size ----
### All MPAs ----
ggplot(habd_df) +
  geom_point(aes(x = size_km2, y = value, color = bioregion)) +
  theme_bw() +
  scale_x_continuous(limits = c(0, 90), expand = c(0,0))+
  scale_y_continuous(limits = c(0, 2.15), expand = c(0,0))+
  labs(x = "MPA Size (sqkm)",
       y = "Habitat Diversity (Shannon)",
       color = "Region")

### Coastal MPAs only ----
ggplot(coastd_df) +
  geom_point(aes(x = size_km2, y = value, color = bioregion)) +
  geom_smooth(aes(x = size_km2, y= value), color = "grey20") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_x_continuous(limits = c(0, 85.5), expand = c(0,0))+
  scale_y_continuous(limits = c(0, 2.15), expand = c(0,0))+
  labs(x = "MPA Size (sqkm)",
       y = "Habitat Diversity (Shannon)",
       color = NULL)


# Export ----
all_div <- habr_df %>% 
  rename(habitat_richness = value) %>% 
  left_join(habd_df %>% select(name, habitat_diversity_sw = value), by = "name") %>% 
  select(name, mpa_habitat_type, habitat_richness, habitat_diversity_sw)

base.dir <- "/Volumes/GoogleDrive-105151121202188525604/Shared drives/NCEAS MPA network assessment/MPA Network Assessment: Working Group Shared Folder/data/sync-data" # Cori Local
clean.dir <- file.path(base.dir, "mpa_traits", "processed")

saveRDS(all_div, file.path(clean.dir, "mpa_attributes_habitat_diversity.Rds"))
#saveRDS(prop_rock, file.path(clean.dir, "mpa_attributes_habitat_rock.Rds"))

  
