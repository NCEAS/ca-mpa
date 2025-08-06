# Baselines 

# Cori Lopazanski: lopazanski@bren.ucsb.edu
# Feb 2025

# About ------------------------------------------------------------------------
# Export datasets used for baseline analyses


# Setup ------------------------------------------------------------------------
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"


# 1. Initial Analyses: Use site-year aggregated data ------------------------------
kelp_complete <- readRDS(file.path(ltm.dir, "kelp_biomass_complete.Rds")) %>% 
  mutate(site_type = factor(site_type, levels = c("Reference", "MPA"))) %>% 
  mutate(kg_per_100m2 = kg_per_m2*100) 


# 2. Secondary Analyses: Use data prior to site-year aggregation ------------------

# Read processed kelp biomass dataset prior to site-year aggregation
kelp_orig <- read_csv(file.path(ltm.dir,"kelpforest_fish_biomass_updated.6.csv")) %>%  
  filter(!is.na(affiliated_mpa)) %>% # drops sites with no mpa
  filter(!is.na(weight_kg)) %>%  # drops fishes unknown or without lengths
  mutate(target_status = if_else(species_code == "NO_ORG", "NO_ORG", target_status))

# Read the MPA table
mpas <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_general.Rds") %>% 
  mutate(implementation_year = as.numeric(format(implementation_date, '%Y'))) %>% 
  left_join(readRDS(file.path("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed", "CA_mpa_metadata.Rds")) %>% 
              dplyr::select(name = mpa, region) %>% 
              mutate(name = str_replace(name, " \\s*\\([^\\)]+\\)", "")))  # fix name to match join) 


# Create site-date effort table (mirrors approach used to create kelp_complete)
kelp_effort <- kelp_orig %>% 
  # Identify distinct transects 
  distinct(year, month, day, site, zone, transect, 
           bioregion, region4, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation) %>% 
  # Identify distinct "sampling events" (e.g. visits to a site in a day rather than a year)
  group_by(year, month, day, site, bioregion, region4, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation) %>% 
  summarize(n_rep = n(), .groups = 'drop') %>%  # total number of transects for that event
  # Join implementation dates
  left_join(mpas %>% dplyr::select(affiliated_mpa, implementation_date, implementation_year, size_km2)) %>% 
  mutate(survey_date = lubridate::make_date(year, month, day),
         implementation_date = as.Date(implementation_date),
         site_type = if_else(mpa_defacto_designation == "ref", "Reference", "MPA"),
         age_at_survey_days = as.numeric(survey_date - implementation_date),   # Calculate actual age at survey
         age_at_survey = year - implementation_year) # Calculate age at survey by year


# # Examine difference in age....
# kelp_effort_test <- kelp_effort %>% 
#   group_by(year, affiliated_mpa, site, site_type, age_at_survey) %>% 
#   summarize(min_age = min(age_at_survey_days),
#             max_age = max(age_at_survey_days),
#             diff_age = max_age - min_age)

# Create version of kelp_complete based on site-date instead of site-year
kelp <- kelp_orig %>% 
  group_by(year, month, day, site, bioregion, region4, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, species_code, target_status) %>% 
  # Total biomass of each species per site per DATE
  summarise(weight_kg = sum(weight_kg),
            count = sum(count), .groups = 'drop') %>%
  filter(!target_status == "NO_ORG") %>%    # drop these bc aren't true zeroes
  select(-target_status)


kelp_complete_date <- kelp_effort %>% 
  # Create complete grid of all species at all sites and years
  expand_grid(species_code = unique(kelp$species_code)) %>% 
  # Add counts and weights (those that were not seen will be NA)
  left_join(kelp) %>% 
  # Add the scinames and target status
  left_join(kelp_orig %>% distinct(species_code, sciname, genus, target_status, name)) %>% 
  # Change NAs to zeroes (those species were not observed in that site-date)
  mutate_at(vars(weight_kg), ~ replace(., is.na(.), 0)) %>% 
  mutate_at(vars(count), ~ replace(., is.na(.), 0)) %>% 
  mutate(kg_per_100m2 = weight_kg/(n_rep*60)*100, # 30x2x2m but JC says typical density is per m2 (60)
         count_per_100m2 = count/(n_rep*60)*100) %>% 
  filter(!species_code %in% c("RFYOY", "SEBSPP")) %>% 
  dplyr::select(year, survey_date, site, site_type, 
                bioregion, region4, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, n_rep, 
                implementation_date, implementation_year, size_km2,
                age_at_survey, age_at_survey_days, n_rep, 
                species_code, sciname, genus, target_status, 
                weight_kg, count, kg_per_100m2, count_per_100m2)


# Export these dataframes for the analyses
out.dir <- "~/ca-mpa/analyses/8baselines/output"

saveRDS(kelp_complete, file.path(out.dir, "kelp_complete.Rds"))
saveRDS(kelp_complete_date, file.path(out.dir, "kelp_complete_date.Rds"))



