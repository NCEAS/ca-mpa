# Process Kelp Forest Monitoring Data
# Cori Lopazanski
# 15 July 2022

# Packages
library(tidyverse)
library(janitor)

# Directories
basedir <- "/Volumes/GoogleDrive-105151121202188525604/Shared drives/NCEAS MPA network assessment/MPA Network Assessment: Working Group Shared Folder/data/sync-data/monitoring/monitoring_kelp"

# Load Data -------------------------------------------------------------------------
kf_swath <- read_csv(file.path(basedir, "MLPA_kelpforest_swath.4.csv")) %>% clean_names()
kf_fish  <- read_csv(file.path(basedir, "MLPA_kelpforest_fish.4.csv")) %>% clean_names()
kf_site  <- read_csv(file.path(basedir, "MLPA_kelpforest_site_table.4.csv")) %>% clean_names()
kf_taxa  <- read_csv(file.path(basedir, "MLPA_kelpforest_taxon_table.4.csv")) %>% clean_names()


# Fish Counts -------------------------------------------------------------
## Each Transect  ----------
# The fish data from kelp forest surveys are reported for each size class, 
# with each level (bottom, middle, canopy) of each transect reported separately. 
# We are interested in total counts across all size classes, and want to quantify
# effort at the transect level rather than # transect levels.

# Total count for all size classes, across all levels (count per sp per transect)
fish_transect <- kf_fish %>% 
  select(survey_year:fish_tl) %>% 
  group_by(survey_year, year, month, day, site, zone, transect, classcode) %>% 
  dplyr::summarize(total_count = sum(count))

# Reclassify NO_ORG to mean completely empty transect instead of empty zone
# (Zeroes matter but only if the count was zero for the entire transect)
fish_transect_wide <- fish_transect %>% 
  pivot_wider(names_from = classcode,
              values_from = total_count) %>% 
  select(survey_year:transect, NO_ORG, BFRE:DMAC) %>% 
  # new column sums the total organisms for each transect
  mutate(total = rowSums(across(BFRE:DMAC), na.rm = TRUE)) %>% # total org per transect
  select(survey_year:transect, total, NO_ORG, BFRE:DMAC) 

fish_transect_wide$NO_ORG[fish_transect_wide$total > 0] <- NA 
    # if the total org per transect > 0, replace NO_ORG with NA

fish_transect_corrected <- fish_transect_wide %>%
  pivot_longer(NO_ORG:DMAC, names_to = "classcode", values_to = "total_count",
               values_drop_na = T) 

## Quantify Sampling Effort  --------

# Get associated MPA information
kf_site_clean <- kf_site %>% 
  select(site, survey_year, ca_mpa_name_short, site_designation, site_status, long_term_region) %>% 
  distinct()

# Calculate sampling effort per site (# of transects at each site each year)
fish_effort <- fish_transect_corrected %>% 
  pivot_wider(names_from = classcode,
              values_from = total_count) %>% 
  group_by(survey_year, site) %>% 
  dplyr::summarize(n_transects = n()) %>% 
  left_join(., kf_site_clean)

# Calculate sampling effort per MPA/ref (# transects for each associated MPA/ref each year)
fish_effort_mpa <- fish_effort %>% 
  group_by(ca_mpa_name_short, site_designation, site_status, survey_year, long_term_region) %>% 
  dplyr::summarize(n_transects = sum(n_transects),
                   n_sites = n())

## Summary Dataframe --------------

# Species by community matrix where each value is the average count per transect 
# conducted at each site between 2016-2020

# Total count for each MPA, all sites, each year separate
fish_mpa <- fish_transect_corrected %>% 
  filter(classcode != "NO_ORG") %>%  # remove empty transects
  left_join(., fish_effort_mpa) %>% 
  group_by(ca_mpa_name_short, site_designation, site_status, survey_year, n_transects, n_sites, classcode) %>% 
  dplyr::summarize(total_count_mpa_year = sum(total_count, na.rm = TRUE)) 

# Total count for each MPA, 2016-2020 combined counts
fish_mpa_1620 <- fish_mpa %>% 
  filter(survey_year %in% c(2016:2020)) %>% 
  group_by(ca_mpa_name_short, site_designation, site_status, classcode) %>% 
  dplyr::summarize(total_count = sum(total_count_mpa_year, na.rm = TRUE),
                   total_transects = sum(n_transects, na.rm = TRUE)) %>% 
  mutate(area_surveyed_m2 = total_transects*60,
         count_per_m2 = total_count/area_surveyed_m2)

## Species-Community Matrix -----------------

mpa_only <- fish_mpa_1620 %>% 
  filter(site_status == "mpa")

spmatrix <- mpa_only %>%
  pivot_wider(id_cols = c("ca_mpa_name_short"), names_from = classcode, values_from = count_per_m2) %>% 
  ungroup() %>% 
  rename(name = ca_mpa_name_short) %>% 
  mutate(name = str_to_lower(name)) 

saveRDS(spmatrix, file = file.path("analyses", "7habitat", "intermediate_data",
                                   "species_matrix_kelp_fish_1620.Rds"))
        

# Swath -------------------------------------------------------------

## Each transect ----
swath <- kf_swath %>% 
  group_by(survey_year, site, zone, transect, classcode) %>% 
  dplyr::summarize(total_count = sum(count, na.rm = TRUE))

# Checked and there are only two instances 
# of "NO_ORG" and neither are empty transects

## Quantify Sampling Effort  --------

# Get associated MPA information
kf_site_clean <- kf_site %>% 
  select(site, ca_mpa_name_short, site_designation, site_status) %>% 
  distinct()

# Calculate sampling effort per site (# of transects at each site each year)
swath_effort <- swath %>% 
  pivot_wider(names_from = classcode,
              values_from = total_count) %>% 
  group_by(survey_year, site) %>% 
  dplyr::summarize(n_transects = n()) %>% 
  left_join(., kf_site_clean)

# Calculate sampling effort per MPA/ref (# transects for each associated MPA/ref each year)
swath_effort_mpa <- swath_effort %>% 
  group_by(ca_mpa_name_short, site_designation, site_status, survey_year) %>% 
  dplyr::summarize(n_transects = sum(n_transects),
                   n_sites = n())

## Summary Dataframe --------------

# Species by community matrix where each value is the average count per transect 
# conducted at each site between 2016-2020

# Total count for each MPA, all sites, each year separate
swath_mpa <- swath %>% 
  filter(classcode != "NO_ORG") %>%  # remove empty transects
  left_join(., swath_effort_mpa) %>% 
  group_by(ca_mpa_name_short, site_designation, site_status, survey_year, n_transects, n_sites, classcode) %>% 
  dplyr::summarize(total_count_mpa_year = sum(total_count, na.rm = TRUE)) 

# Correct swami to swami's
swath_mpa$ca_mpa_name_short[swath_mpa$ca_mpa_name_short == "Swamis SMCA"] = "swami's smca"

# Total count for each MPA, 2016-2020 combined counts
swath_mpa_1620 <- swath_mpa %>% 
  filter(survey_year %in% c(2016:2020)) %>% 
  group_by(ca_mpa_name_short, site_designation, site_status, classcode) %>% 
  dplyr::summarize(total_count = sum(total_count_mpa_year, na.rm = TRUE),
                   total_transects = sum(n_transects, na.rm = TRUE)) %>% 
  mutate(area_surveyed_m2 = total_transects*60,
         count_per_m2 = total_count/area_surveyed_m2) %>% 
  rename(name = ca_mpa_name_short) %>% 
  mutate(name = str_to_lower(name)) %>% 
  ungroup()

## Species-Community Matrix -----------------

swath_mpa_only <- swath_mpa_1620 %>% 
  filter(site_status == "mpa")

swath_spmatrix <- swath_mpa_only %>%
  pivot_wider(id_cols = c("name"), names_from = classcode, values_from = count_per_m2)

swath_spmatrix[is.na(swath_spmatrix)] <- 0

saveRDS(swath_spmatrix, file = file.path("analyses", "7habitat", "intermediate_data",
                                   "species_matrix_kelp_swath_1620.Rds"))


