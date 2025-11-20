# Process Surf Zone Data
# Cori Lopazanski
# Nov 2024

# Reading and processing steps adapted from Josh Smith, Step1_process_biomass.R

# Summary of key changes from original code:
# - Added column for whether reference site is actually inside SMCA
# - Added taxa information from species_key 
# - Estimated tl_cm for "Seriphus politus" and "Atherinops affinis" since these species were subsampled. 
#   the original data used the mean tl_cm, but we opted to draw from the size frequency distribution of the subsampled individuals 
#   at the MPA-year level. 


# Note potential concerns for next steps:
# - Some of the lengths seem to be already converted to centimeters -8/31/23 used tl_mm instead (not fish_length) -JGS
# - There are some taxa that don't match fully (mostly higher groupings) - 8/31/23 added these to taxon table -JGS

# NOTE: THIS IS IN THE PROCESS 2024 VERSIONS FOLDER, BUT THERE IS NO UPDATED DATA JUST YET.
# THE ONLY CHANGE FOR THE HABITAT ANALYSES IS ADDING BACK THE SITE NAME TO FIGURE OUT WHAT
# SITE THE DATA ARE FROM.

# Setup --------------------------------------------------------------------------------
rm(list=ls())

# Load required packages
library(tidyverse)
library(janitor)
library(readxl)

# Set directories
datadir <- "/home/shares/ca-mpa/data/sync-data/monitoring/"
outdir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data"

# Read surf monitoring data
surf_zone_raw <- read.csv(file.path(datadir, "monitoring_sandy-beach/surf_zone_fish_seine_data.csv")) %>%
  clean_names()

surf_zone_raw <- read.csv("/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_sandy-beach/update_2024/seine_fish_19_24.csv", na.strings = c("", "NA")) %>% 
  clean_names() %>% 
  rename(site_type = mpa_status,
         mpa_name_short = ca_mpa_name_short) 

# Read taxonomy lookup table
taxon_tab <- read.csv("/home/shares/ca-mpa/data/sync-data/species_traits/processed/species_key_2025.csv") %>% 
  clean_names()%>%
  #reassign target_status_standardized for downstream code
  dplyr::select(-target_status)%>%
  rename(target_status = target_status_standardized)%>%
  filter(habitat == "Surf Zone")

# Read regions from MPA attributes table
regions <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_general.Rds") %>% 
  dplyr::select(name, bioregion, region4 = four_region_north_ci) %>%
  mutate(name = tolower(name))

# Read de-facto SMRs
defacto_smr_surf <- readxl::read_excel("/home/shares/ca-mpa/data/sync-data/mpa_traits/mpa-attributes.xlsx", sheet = 5, skip = 0, na = "NA") %>%
  filter(group == "surf") %>%
  dplyr::select(affiliated_mpa, mpa_defacto_class = mpa_class) %>% 
  mutate(mpa_defacto_class = str_to_lower(mpa_defacto_class))


# Process Data ------------------------------------------------------------------------

# Identify paired mpa-reference sites
pair_key <- surf_zone_raw %>% 
  distinct(site_type, mpa_name_short, site_pair) %>% 
  filter(site_type == "MPA") %>% 
  mutate(affiliated_mpa = mpa_name_short) %>% 
  select(site_pair, affiliated_mpa)

# Surf zone used an alphabetic naming convention ('site_pair') to identify matched pairs (inside vs. out)
pairs <- surf_zone_raw %>% 
  dplyr::select(site_type, site_name, mpa_name_short, site_pair) %>% # site_code, affiliated_mpa,  mpa_status,  mpa_type
  distinct() %>%
  left_join(pair_key) %>% 
  # Surf zone called any SMR or SMCA a 'MPA', so use the affiliated_mpa name to identify the state class 
  mutate(mpa_state_class = tolower(word(affiliated_mpa, -1)),
         mpa_state_designation = ifelse(site_type == "Reference", "ref", mpa_state_class)) %>% 
  # Creat column that notes that the reference is an SMCA - 6 in surf zone
  mutate(ref_is_mpa = if_else(site_type == "Reference" & !is.na(mpa_name_short), "yes", "no"))


data <- surf_zone_raw %>% 
  # Add paired mpa-reference sites
  left_join(pairs) %>% 
  rename(weight_g = fish_weight_individual,
         total_weight_g = fish_weight) %>%
  mutate(total_weight_kg = total_weight_g/1000,
         tl_cm = tl_mm/10, # changed to calculate from tl_mm instead of fish_length -JGS
         sl_cm = sl_mm/10,
         affiliated_mpa = tolower(affiliated_mpa)) %>% 
  # Add de-facto smr designations
  left_join(defacto_smr_surf) %>% 
  mutate(affiliated_mpa = recode(affiliated_mpa, "ano nuevo smr" = "año nuevo smr")) %>% 
  # Add regions
  left_join(regions, by = c("affiliated_mpa" = "name")) %>% 
  mutate(mpa_defacto_designation = ifelse(mpa_state_designation == "ref", "ref", tolower(mpa_defacto_class))) %>% 
  dplyr::select(year, month, day, bioregion, region4, affiliated_mpa, 
                mpa_state_class, mpa_state_designation, mpa_defacto_class, 
                mpa_defacto_designation, ref_is_mpa, haul_number, site_name,
                species_code, # Intentionally drop other taxa info - default to the surf taxon table
                tl_cm,sl_cm, weight_g, total_weight_g, 
                count, total_weight_kg) %>%
  # Change "NOSP" to "NO_ORG" to match other habitats
  mutate(species_code = recode(species_code, 
                               "NOSP" = "NO_ORG", 
                               "GLIN" = "GELI")) %>% 
  mutate(total_weight_g = ifelse(species_code == "NO_ORG", 0, total_weight_g),
         total_weight_kg = ifelse(species_code == "NO_ORG", 0, total_weight_kg)) %>% 
  # Add taxa info from surf taxon table
  left_join(taxon_tab, by = c("species_code" = "habitat_specific_code")) %>% 
  # Change unidentified to match other habitats
  mutate(species_code = case_when(species_code %in% c("unspecified", "FFUN") ~ "UNKNOWN",
                                  is.na(species_code) & count > 0 ~ "UNKNOWN", # length data but missing species data
                                  T~species_code)) %>% 
  # Drop 2024 to match other habitats and because we don't have the kelp data 
  filter(year < 2024) %>% 
  # Drop a few rows with no count data
  filter(!is.na(count)) %>% 
  # Uncount so each row is a fish
  uncount(weights = count, .remove = F) 


# estimate tl_cm for schooling fishes ------------------------------------------

data <- data %>% mutate(.row = row_number())

site_keys <- c("year","month","day","bioregion","region4","affiliated_mpa",
               "mpa_state_class","mpa_state_designation","mpa_defacto_class",
               "mpa_defacto_designation","species_code")

sub_fish_sp <- data %>% filter(count > 1) %>% distinct(species_code)
sub_fish <- data %>% filter(species_code %in% sub_fish_sp$species_code)

size_dist_site <- sub_fish %>%
  filter(!is.na(tl_cm)) %>%
  group_by(across(all_of(site_keys)), tl_cm) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  group_by(across(all_of(site_keys))) %>%
  summarise(sizes = list(tl_cm), probs = list(n / sum(n)), .groups = "drop")

na_rows <- sub_fish %>% filter(is.na(tl_cm))

na_with_dist <- na_rows %>%
  left_join(size_dist_site, by = site_keys) %>%
  mutate(sizes_use = sizes, probs_use = probs)

set.seed(1985)
inferred_tl_cm <- na_with_dist %>%
  mutate(tl_cm_inferred = map2_dbl(sizes_use, probs_use, ~ {
    if(is.null(.x) || length(.x) == 0) return(NA_real_)
    sample(.x, size = 1, prob = .y)
  })) %>%
  select(.row, tl_cm_inferred)

data2 <- data %>%
  left_join(inferred_tl_cm, by = ".row") %>%
  mutate(tl_cm = coalesce(tl_cm, tl_cm_inferred)) %>%
  select(-tl_cm_inferred, - .row)

#new dist
ggplot(data2 %>% filter(habitat_specific_spp_name %in% c("Seriphus politus", "Atherinops affinis")), aes(x = tl_cm)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~ habitat_specific_spp_name, scales = "free_x") 

#orig dist
ggplot(data %>% filter(habitat_specific_spp_name %in% c("Seriphus politus", "Atherinops affinis")), aes(x = tl_cm)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~ habitat_specific_spp_name, scales = "free_x") 


# Check taxa NAs
taxa_na <- #data %>% #old
  data2 %>%
  filter(is.na(sciname) & !(species_code == "NO_ORG"))

# Test for matching taxa
taxa_match <- #data %>% #old
  data2 %>%
  distinct(species_code) %>% 
  filter(!is.na(species_code)) %>% 
  filter(!(species_code == "NO_ORG")) %>% 
  filter(!(species_code %in% taxon_tab$habitat_specific_code))

## Note: There are still 4 with no taxonomic info that need to be updated in 
## the main species_key if we want to include beyond tracking effort (e.g. 
## manually fill in appropriate taxa information across columns when processing
## surf zone taxon table)

#added above to surf zone taxon table on 8/31/23 -JGS
# Unspecified, HALI, RFYOY, FFUN


# Write data ------------------------------------------------------------------------
#write.csv(data, row.names = FALSE, file.path(outdir, "surf_zone_fish_processed.csv"))
# last write 13 Sept 2023


#write.csv(inferred_size, row.names = FALSE, file.path(outdir, "surf_zone_processed.csv"))
#last export 16 Feb 2024

outdir <-  "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"
write.csv(data2, row.names = FALSE, file.path(outdir, "surf_zone_processed.csv"))
#last export 10 Nov 2025

