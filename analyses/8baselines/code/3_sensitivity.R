# Baselines Analyses
# Cori Lopazanski
# Feb 2025



# Sensitivity Analyses ----

## 1. How sensitive is our baseline biomass estimate? 
## -- So far: Calculated within one year of MPA implementation, based on the survey year (aggregated data)
## -- To test: calculate based on survey DATE within set time frame of implementation DATE

# Use: kelp_complete_date
kelp_complete_date <- readRDS(file.path(out.dir, "kelp_complete_date.Rds"))

kelp_sites_date <- kelp_complete_date %>% 
  # Count number of years each site was visited before and after the MPA was implemented
  group_by(site, bioregion, region4, affiliated_mpa,  mpa_defacto_class, implementation_year, implementation_year_adj, size_km2, cluster_area_km2, site_type) %>% 
  summarize(n_before = sum(before),
            n_after = sum(after),
            n_total = n_before + n_after,
            n_baseline = sum(baseline),
            years = paste(unique(year), collapse = ", ")) %>% 
  # Drop sites with no inside/outside information
  filter(!is.na(site_type)) %>%  
  # Keep sites that have been visited at least 5 times after implementation
  filter(n_after >= 5) %>%  
  # Keep MPAs that still have at least one MPA site and one Reference site
  group_by(affiliated_mpa) %>% 
  filter(n_distinct(site_type) == 2) %>% 
  # Keep MPAs with at least one site with baseline information
  filter(any(site_type == "MPA" & n_baseline > 0) &
           any(site_type == "Reference" & n_baseline > 0)) %>% ungroup()

kelp_effort <- kelp_orig %>% 
  # Identify distinct transects - 35251 (after the NA dropped above)
  distinct(year, month, day, site, zone, transect, 
           bioregion, region4, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation) %>% 
  # Identify distinct "sampling events" (e.g. visits to a site)
  group_by(year, month, day, site, bioregion, region4, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation) %>% 
  summarize(n_rep = n(), .groups = 'drop') %>%  # total number of transects for that event
  # Join implementation dates
  left_join(mpas %>% dplyr::select(affiliated_mpa, implementation_date, implementation_year, size_km2)) %>% 
  mutate(survey_date = lubridate::make_date(year, month, day),
         implementation_date = as.Date(implementation_date),
         site_type = if_else(mpa_defacto_designation == "ref", "Reference", "MPA"),
         age_at_survey_days = as.numeric(survey_date - implementation_date),   # Calculate actual age at survey
         age_at_survey = year - implementation_year, # Calculate age at survey by year
         before = if_else(age_at_survey_days < 0, n_rep, 0),
         after = if_else(age_at_survey_days >= 0, n_rep, 0)) %>%
  # Drop sites with no affiliation
  filter(!is.na(site_type)) %>% 
  # Keep sites visited at least 5 distinct years after MPA implementation
  group_by(site, site_type, affiliated_mpa) %>% 
  filter(n_distinct(year[after > 0]) >= 5) %>% ungroup() %>% 
  # Keep MPAs that have at least one inside and one outside site
  group_by(affiliated_mpa) %>% 
  filter(n_distinct(site_type) == 2) %>% 
  # Keep MPAs with in/out data within 2 years of implementation
  filter(any(site_type == "MPA" & sum(n_rep[between(age_at_survey_days, -731, 731)]) > 0) &
           any(site_type == "Reference" & sum(n_rep[between(age_at_survey_days, -731, 731)]) > 0)) %>% ungroup() 

length(unique(kelp_effort$affiliated_mpa)) # 32 total within 2 years 
length(unique(kelp_effort$affiliated_mpa[kelp_effort$mpa_defacto_class == "smr"])) # 25 SMRs within 2 years


kelp <- kelp_orig %>% 
  group_by(year, month, day, site, bioregion, region4, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, target_status) %>% 
  # Total biomass of targeted species per site per **day** 
  summarise(weight_kg = sum(weight_kg),
            count = sum(count), .groups = 'drop') %>%
  filter(!target_status == "NO_ORG") %>%    # drop these bc aren't true zeroes
  select(-target_status)


kelp_complete <- kelp_effort2 %>% 
  # Add counts and weights (those that were not seen will be NA)
  left_join(kelp) %>% 
  # Change NAs to zeroes (those species were not observed in that site-year)
  mutate_at(vars(weight_kg), ~ replace(., is.na(.), 0)) %>% 
  mutate_at(vars(count), ~ replace(., is.na(.), 0)) %>% 
  mutate(site_type = if_else(mpa_defacto_designation == "ref", "Reference", "MPA")) %>% 
  mutate(kg_per_m2 = weight_kg/(n_rep*60), # 30x2x2m but JC says typical density is per m2 (60)
         count_per_m2 = count/(n_rep*60),
         age_at_survey = year - implementation_year) %>% 
  dplyr::select(year, site, site_type, 
                bioregion, region4, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, implementation_year, size_km2, cluster_area_km2,
                age_at_survey, age_at_survey_days, n_rep, weight_kg, count, kg_per_m2, count_per_m2)



# Filter for the surveys within 365 days of implementation
kelp_baseline <- kelp_effort2 %>% 
  filter(between(age_at_survey_days, -365, 365)) %>% 
  group_by(affiliated_mpa, site, mpa_defacto_designation) %>% 
  summarize(n_rep = sum(n_rep))






