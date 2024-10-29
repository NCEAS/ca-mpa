# Cori Loapzanski
# August 2024

# About ------------------------------------------------------------------------------------

# Merge the species, habitat, and monitoring tables into one df for models

# Setup ------------------------------------------------------------------------------------
library(tidyverse) 


# Directories
fig.dir <- "~/ca-mpa/analyses/7habitat/figures"
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data"
sp.dir <- "/home/shares/ca-mpa/data/sync-data/species_traits/processed"
int.dir <- "~/ca-mpa/analyses/7habitat/intermediate_data"

# Read Data -----------------------------------------------------------
habitat_raw <- readRDS(file.path(int.dir, "habitat_buffers_by_site.Rds")) %>% 
  rename(affiliated_mpa = mpa_orig)

sp_raw <- readRDS(file.path(sp.dir, "species_lw_habitat.Rds")) %>% 
  select(genus, sciname = species, common_name, target_status, vertical_zonation, 
         depth_min_m, depth_max_m, depth_common_min_m, depth_common_max_m,
         region, assemblage, assemblage_new)

mlpa_sp <- read_csv("/home/shares/ca-mpa/data/sync-data/species_traits/processed/species_key.csv") %>% 
  filter(habitat == "Kelp forest") %>% 
  select(species_code = habitat_specific_code, habitat_specific_spp_name) %>% 
  distinct()

kelp_raw <- readRDS(file.path(ltm.dir, "biomass_site_year/kelp_biomass_site_year.Rds")) 

kelp_effort_raw <- readRDS(file.path(ltm.dir, "biomass_site_year/kelp_site_year_effort.Rds")) %>% 
  ungroup()

mpas_orig <- readRDS(file.path("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed", "CA_mpa_metadata.Rds")) %>% 
  dplyr::select(name = mpa, region) %>% 
  mutate(name = str_replace(name, " \\s*\\([^\\)]+\\)", "")) # fix name to match join

mpas <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_general.Rds") %>% 
  mutate(implementation_year = as.numeric(format(implementation_date, '%Y'))) %>% 
  left_join(mpas_orig) %>% 
  mutate(affiliated_mpa = dplyr::recode(affiliated_mpa, 
                                        "swami's smca" = "swamis smca"))

# Build Kelp Sites Summary ---------------------------------------------
kelp_sites <- kelp_raw %>% 
  distinct(year, site, affiliated_mpa, site_type) %>% 
  left_join(mpas %>% dplyr::select(region, affiliated_mpa, implementation_year)) %>% 
  mutate(before = if_else(year <= implementation_year, 1, 0),
         after = if_else(year > implementation_year, 1, 0)) %>% 
  group_by(site, region, affiliated_mpa, implementation_year, site_type) %>% 
  summarize(n_before = sum(before),
            n_after = sum(after),
            n_total = n_before + n_after) %>% 
  filter(!is.na(site_type)) %>% ungroup()

# Process Habitat Data ------------------------------------------------
habitat <- habitat_raw %>% 
  filter(habitat == "Kelp") %>% 
  filter(site %in% kelp_sites$site) %>% 
 # filter(depth_zone == "0_30m") %>% 
  mutate(habitat_depth_buffer = paste(habitat_depth, buffer, sep = "_"))

habitat_wide <- habitat %>% 
  dplyr::select(mpa, affiliated_mpa, site, site_type, area_m2, habitat_depth_buffer) %>% 
  pivot_wider(names_from = "habitat_depth_buffer", values_from = "area_m2") %>% 
  dplyr::select(-affiliated_mpa, -site_type)

# Process Kelp Effort Data --------------------------------------------
kelp_effort <- kelp_effort_raw %>% 
  filter(site %in% kelp_sites$site) %>% 
  mutate(site_type = if_else(mpa_defacto_designation == "ref", "Reference", "MPA"))

# Summarize Kelp Data -------------------------------------------------
kelp <- kelp_raw %>% 
  filter(site %in% kelp_sites$site) %>% 
  group_by(species_code, sciname, target_status, assemblage_new, year, bioregion, affiliated_mpa, site, site_type) %>% 
  summarize(kg_per_m2 = sum(kg_per_m2),
            count_per_m2 = sum(count_per_m2), .groups = 'drop') 

kelp_assemblage <- kelp %>%
  distinct(species_code, sciname, target_status, bioregion, assemblage_new)

kelp_subset <- kelp %>% 
  mutate(assemblage_new = case_when(sciname == "Paralabrax clathratus" ~ "Hard Bottom Biotic", 
                                    sciname == "Sebastes rastrelliger" ~ "Hard Bottom Biotic", 
                                    sciname == "Sebastes miniatus" ~ "Hard Bottom", 
                                    T~assemblage_new)) %>% 
  filter(sciname %in% c("Semicossyphus pulcher", #sheephead
                        "Caulolatilus princeps", # ocean whitefish
                        "Sebastes atrovirens", # kelp rockfish
                        "Ophiodon elongatus", # lingcod
                        "Sebastes entomelas") | # widow rockfish
           species_code %in% c("OYT", #olive yellowtail
                               "PCLA" # kelp bass
           )) 


# Create Complete Kelp Data -------------------------------------------
kelp_complete <- kelp_effort %>% 
  expand_grid(species_code = unique(kelp$species_code)) %>% 
  left_join(kelp %>% dplyr::select(-target_status, -assemblage_new, -sciname), by = c("year", "bioregion", "affiliated_mpa", "site", "site_type", "species_code")) %>% 
  left_join(kelp %>% distinct(species_code, bioregion, sciname, target_status, assemblage_new), by = c("species_code", "bioregion")) %>% 
  mutate_at(vars(kg_per_m2), ~ replace(., is.na(.), 0)) %>% 
  mutate_at(vars(count_per_m2), ~ replace(., is.na(.), 0)) %>% 
  left_join(kelp_sites %>% dplyr::select(site, region, affiliated_mpa, implementation_year, site_type)) %>% 
  mutate(region = factor(region, levels = c("North Coast", "North Central Coast", "Central Coast", "South Coast")),
         age_at_survey = year-implementation_year) %>% 
  dplyr::select(year:mpa_defacto_designation, region, implementation_year, age_at_survey, site_type, species_code, sciname, target_status, assemblage_new, kg_per_m2, count_per_m2) %>% 
  left_join(habitat_wide, by = "site") %>%
  mutate(log_kg_per_m2 = log(kg_per_m2 + 1),
         status = factor(if_else(age_at_survey < 0, "before", "after")))


# Identify Top Species by Count and Biomass ---------------------------
top_species <- kelp_complete %>%
  group_by(bioregion, species_code, sciname, target_status, assemblage_new) %>%
  summarize(total_kg = sum(kg_per_m2), 
            total_count = sum(count_per_m2), .groups = 'drop') %>%
  filter(!is.na(sciname))

# Prepare Final Dataset for Modeling ----------------------------------
subset_sites <- kelp_sites %>%
  filter(n_total >5)

subset_mpas <- subset_sites %>%
  group_by(region, affiliated_mpa, implementation_year, site_type) %>%
  summarize(n_total = sum(n_total), .groups = 'drop') %>%
  pivot_wider(names_from = site_type, values_from = n_total) %>%
  filter(!is.na(Reference)) %>% 
  filter(!is.na(MPA))

data <- kelp_complete %>%
  filter(site %in% subset_sites$site) %>% 
  filter(affiliated_mpa %in% subset_mpas$affiliated_mpa)



# # Plot site subset
# ggplot(data = habitat %>% 
#          filter(site %in% subset_sites$site) %>% 
#          filter(affiliated_mpa %in% subset_mpas$affiliated_mpa)) +
#   geom_bar(aes(x = site, y = area_m2, fill = habitat_class), 
#            stat = "identity", position = "stack") +
#   facet_wrap(~buffer+depth_zone, scales = "free_y")

