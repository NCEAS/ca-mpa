# Explore data within MPAs

# About 


# Setup ------------------------------------------------------------------------------
# Packages
library(tidyverse)
library(sf)
library(janitor)


# Directories
sync.dir <- "/home/shares/ca-mpa/data/sync-data"
pro.dir  <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed"
org.dir  <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/biomass_processed"

# Read Data -------------------------------------------------------------------------
#sub_ras <- readRDS(file.path(pro.dir, "substrate/substrate_rasters", "PMEP_Substrate_24mRes_Section41.Rds"))
sub_shp <- readRDS(file.path(pro.dir, "substrate", "mpa_substrate_intersection.Rds"))


# Build -------------------------------------------------------------------------
# Calculate totals within each MPA
totals <- sub_shp %>% 
  mutate(PMEP_Zone_Desc = case_when(PMEP_Zone == 0 ~ "Landward",
                                    PMEP_Zone %in% c(2, 3) ~ "0 to 30m",
                                    PMEP_Zone %in% c(4, 5) ~ "30 to 100m",
                                    PMEP_Zone %in% c(6, 7) ~ "100 to 200m",
                                    PMEP_Zone == 8 ~ ">200m"),
         CMECS_SC_Category_Consolidated = 
           case_when(CMECS_SC_Category %in% c("Unconsolidated Mineral Substrate", "Fine Unconsolidated Substrate", "Coarse Unconsolidated Substrate") ~ "Unconsolidated Substrate",
                     TRUE ~ CMECS_SC_Category)) %>% 
  group_by(name, region, PMEP_Zone_Desc, CMECS_SC_Category_Consolidated) %>% 
  summarize(geometry = st_union(Shape),
            area = st_area(geometry)) 

totals_simple <- totals %>% 
  st_drop_geometry() %>% 
  mutate(area_km = as.vector(area)/1e6,
         name = str_replace(name, " \\s*\\([^\\)]+\\)", ""))

saveRDS(totals_simple, file.path(pro.dir, "substrate", "mpa_substrate_intersection_totals.Rds"))

anacapa <- sub_shp %>% 
  filter(name == "Anacapa Island SMR" |
           name == "Anacapa Island SMCA") %>% 
  filter(!(CMECS_SC_Category == "Rock Substrate"))

ggplot(data = anacapa) +
  geom_sf(aes(fill = CMECS_SC_Category_Code))

rock_only <- totals_simple %>% ungroup() %>% 
  filter(CMECS_SC_Category == "Rock Substrate") %>% 
  select(name, region, PMEP_Zone_Desc, area_km) %>% 
  pivot_wider(names_from = PMEP_Zone_Desc, names_prefix = "rock_", values_from = area_km)

# Compare to old attribute data
old_build <- old_att %>% 
  select(name, bioregion,
         rock_30_cdfw = hard_substrate_0_30m_km2_comb,
         rock_100_cdfw = hard_substrate_100_200m_km2,
         rock_200_cdfw  = hard_substrate_200_3000m_km2)


rock_compare <- full_join(old_build, rock_only)


# Load data
kelp_raw <- read.csv(file.path(org.dir, "kelpforest_fish_biomass_updated.csv")) 
ccfrp_biomass <- read.csv(file.path(org.dir, "ccfrp_fish_biomass_updated.csv"))
deep_biomass <- read.csv(file.path(org.dir, "deep_reef_fish_biomass_updated.csv"))

# Process Biomass DFs -----
kelp <- kelp_raw %>% # WARNING: THE NA REMOVALS HERE DROPS LOTS OF TRANSECTS (~871)
  filter(!is.na(affiliated_mpa)) %>% # drops sites with no mpa (yellowbanks, trinidad, etc - see kf processing for details)
  filter(!is.na(weight_kg)) %>%  # drops fishes unknown or without lengths/conversion params
  mutate(target_status = if_else(species_code == "NO_ORG", "NO_ORG", target_status))

kelp_sp <- kelp %>% 
  distinct(sciname, target_status)

# Identify distinct transects after NAs dropped (for subsequent analyses)
# 28300
kelp_effort_transect <- kelp %>% 
  distinct(year, month, day, site, # need this to get actual individual transects - updated by CL
           affiliated_mpa, mpa_defacto_class, mpa_defacto_designation,
           zone, transect) %>% 
  arrange(year, month, day, site, affiliated_mpa, mpa_defacto_designation, zone, transect) # Could then group to calculate transects; not needed yet

# calculate effort as n transects per MPA year (separately for smr and ref)
kelp_effort_mpa <- kelp_effort_transect %>% 
  group_by(year, affiliated_mpa, mpa_defacto_class,  mpa_defacto_designation) %>% 
  summarize(n_rep = n()) # for comparison later

kelp_species_biomass <- kelp %>% 
#  filter(sciname == "Semicossyphus pulcher") %>% 
  group_by(year, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, sciname) %>%
  summarise(total_biomass = sum(weight_kg)) %>%  # total species-specific biomass per mpa-year-site
  full_join(., kelp_effort_mpa) %>% 
  mutate(total_biomass = if_else(is.na(total_biomass), 0, total_biomass)) %>% 
  ungroup() 


implementation_year <- mpa_attributes_gen %>% 
  mutate(implementation_year = as.numeric(format(as.Date(implementation_date, format="%Y-%m-%d"),"%Y"))) %>% 
  select(affiliated_mpa, bioregion, implementation_year)


kelp_species_summary <- kelp_species_biomass %>% 
  filter(!(mpa_defacto_designation == "ref")) %>% 
  left_join(., implementation_year) %>% 
  mutate(biom_std = total_biomass/n_rep,
         age_at_survey = year - implementation_year) %>% 
  filter(age_at_survey > 0) %>% 
  group_by(affiliated_mpa, mpa_defacto_class, bioregion, sciname) %>% 
  summarize(biomass_mean = mean(biom_std, na.rm = T),
            biomass_sd = sd(biom_std, na.rm = T)) %>% 
  ungroup() %>% 
  filter(biomass_mean > 0) %>% 
  left_join(., pmep_sub, relationship = "many-to-many") %>% 
  clean_names() %>% 
  filter(cmecs_sc_category_consolidated %in% c("Rock Substrate", "Unconsolidated Substrate")) %>% 
  mutate(pmep_zone_desc = if_else(pmep_zone_desc == "Landward", "Intertidal", pmep_zone_desc)) %>% 
  mutate(pmep_zone_desc = factor(pmep_zone_desc, levels = c("Intertidal", "0 to 30m", "30 to 100m", "100 to 200m", ">200m"))) 
  
  
g1 <- ggplot(kelp_species_summary %>% 
         filter(mpa_defacto_class == "smr") %>% 
         filter(sciname == "Semicossyphus pulcher") %>%
           filter(bioregion == "South")) +
  geom_point(aes(x = area_km, y = biomass_mean)) +
  geom_smooth(aes(x = area_km, y = biomass_mean), method = "lm", se = F) +
  facet_wrap(~cmecs_sc_category_consolidated+pmep_zone_desc, scales = "free", nrow = 2) +
  expand_limits(y=0) +
  labs(x = "Area of habitat within MPA (km2)",
       y = "Average biomass within the MPA post-establishment") +
  theme_minimal()
g1

ggplot(kelp_species_summary %>% 
         filter(mpa_defacto_class == "smr") %>% 
         filter(sciname == "Sebastes spp")) +
  geom_point(aes(x = area_km, y = biomass_mean)) +
  geom_smooth(aes(x = area_km, y = biomass_mean), method = "lm", se = F) +
  facet_grid(pmep_zone_desc ~ bioregion + cmecs_sc_category_consolidated, scales = "free") +
  expand_limits(y=0) +
  labs(x = "Area of habitat within MPA (km2)",
       y = "Average biomass within the MPA post-establishment") +
  theme_minimal()




ggplot() + 
  geom_jitter(data = sheep_rock %>% 
               filter(bioregion == "South") %>% 
               filter(mpa_defacto_class == "smr"), 
             aes(x = area_km, y = biom_std, col = affiliated_mpa)) +
  facet_wrap(~mpa_defacto_class)

ggplot() + 
  geom_jitter(data = sheep_rock %>% 
                filter(bioregion == "South") %>% 
                filter(mpa_defacto_class == "smr") %>% 
                filter(year > 2015) %>% 
                group_by(affiliated_mpa, mpa_defacto_class, bioregion, area_km) %>% 
                summarize(biom_avg = mean(biom_std)), 
              aes(x = area_km, y = biom_avg, col = affiliated_mpa)) +
  labs(x = "Area of rock 0 - 30m within MPA",
       y = "Mean sheephead biomass (kg) per MPA")


plot <- totals %>% filter(PMEP_Section == 41)



tmap_mode("view")
tmap_options(check.and.fix = TRUE)
tm_shape(plot)+
  tm_fill(col = "CMECS_SC_Category",
          palette = c("burlywood1", "tan4", "grey60", "burlywood3")) +
  tm_layout(legend.outside = TRUE)






combined <- readRDS(file.path(pro.dir, "combined", "PMEP_Combined_24mRes_Section40.Rds"))

old_att <- readRDS(file.path(getwd(), "analyses/7habitat/intermediate_data/mpa_attributes_processed.Rds"))


zone_ids <- data.frame(zone = c(0:8)) %>% 
  mutate(zone_depth = case_when(zone == 0 ~ "Landward",
                                zone == 1 ~ "Estuary Overlap",
                                zone %in% c(2, 3) ~ "Shoreline to -30m",
                                zone %in% c(4, 5) ~ "-30m to -100m",
                                zone %in% c(6, 7) ~ "-100 to -200m",
                                zone == 8 ~ "> -300m")) %>% 
  mutate(zone_depth = factor(zone_depth, levels = c("Landward", "Estuary Overlap", "Shoreline to -30m",
                                                    "-30m to -100m","-100 to -200m", "> -300m")))


# Summarize habitat within MPAs
summary <- combined %>% 
  left_join(., zone_ids) %>%
  group_by(mpa_name, zone_depth) %>% 
  summarize(rock_sum = sum(rock_substrate, na.rm = T),
            unconsolidated_sum = sum(unconsolidated_substrate, na.rm = T),
            coarse_unconsolidated_sum = sum(coarse_unconsolidated_substrate, na.rm = T),
            fine_unconsolidated_sum = sum(fine_unconsolidated_substrate, na.rm = T)) %>% 
  group_by(mpa_name, zone_depth) %>% 
  mutate(soft_sum = unconsolidated_sum + coarse_unconsolidated_sum + fine_unconsolidated_sum) %>% 
  mutate(rock_area = rock_sum*24*24/1e6,
         soft_area = soft_sum*24*24/1e6) %>% 
  select(mpa_name, zone_depth, rock_area, soft_area) %>% 
  mutate(mpa_name = str_replace(mpa_name, " \\s*\\([^\\)]+\\)", "")) %>% 
  arrange(mpa_name, zone_depth)


old_sub <- old_att %>% 
  filter(name %in% summary$mpa_name)


