# Process Surf Zone Data
# Cori Lopazanski
# August 18, 2023

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

# Read taxonomy lookup table
taxon_tab <- read.csv("/home/shares/ca-mpa/data/sync-data/species_traits/processed/species_key.csv") %>% 
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
# Surf zone used an alphabetic naming convention ('site_pair') to identify matched pairs (inside vs. out)

pairs <- surf_zone_raw %>% 
  dplyr::select(site_code, site_type, mpa_name_short, affiliated_mpa, site_pair, mpa_status, mpa_type) %>% 
  distinct() %>%
  # Surf zone called any SMR or SMCA a 'MPA', so use the affiliated_mpa name to identify the state class 
  mutate(mpa_state_class = tolower(word(affiliated_mpa, -1)),
         mpa_state_designation = ifelse(mpa_status == "Reference", "ref", mpa_state_class)) %>% 
  # Creat column that notes that the reference is an SMCA - 6 in surf zone
  mutate(ref_is_mpa = if_else(mpa_status == "Reference" & !(mpa_name_short == ""), "yes", "no"))


data <- surf_zone_raw %>% 
  # Add paired mpa-reference sites
  left_join(pairs) %>% 
  rename(weight_g = fish_weight_individual,
         total_weight_g = fish_weight) %>%
  mutate(total_weight_kg = total_weight_g / 1000,
         tl_cm = tl_mm / 10, # changed to calculate from tl_mm instead of fish_length -JGS
         sl_cm = sl_mm / 10,
         affiliated_mpa = tolower(affiliated_mpa)) %>% 
  # Add de-facto smr designations
  left_join(defacto_smr_surf) %>% 
  mutate(affiliated_mpa = recode(affiliated_mpa, "ano nuevo smr" = "año nuevo smr")) %>% 
  # Add regions
  left_join(regions, by = c("affiliated_mpa" = "name")) %>% 
  mutate(mpa_defacto_designation = ifelse(mpa_state_designation == "ref", "ref", tolower(mpa_defacto_class))) %>% 
  dplyr::select(year, month, day, bioregion, region4, affiliated_mpa, 
                mpa_state_class, mpa_state_designation, mpa_defacto_class, 
                mpa_defacto_designation, ref_is_mpa, haul_number, 
                species_code, # Intentionally drop other taxa info - default to the surf taxon table
                tl_cm,sl_cm, weight_g, total_weight_g, 
                count, total_weight_kg) %>%
  # Change "NOSP" to "NO_ORG" to match other habitats
  mutate(species_code = recode(species_code, "NOSP" = "NO_ORG")) %>% 
  mutate(total_weight_g = ifelse(species_code == "NO_ORG", 0, total_weight_g),
         total_weight_kg = ifelse(species_code == "NO_ORG", 0, total_weight_kg)) %>% 
  # Add taxa info from surf taxon table
  left_join(taxon_tab, by = c("species_code" = "habitat_specific_code")) %>% 
  # Change unidentified to match other habitats
  mutate(species_code = case_when(species_code %in% c("unspecified", "FFUN") ~ "UNKNOWN",
                                  is.na(species_code) & count > 0 ~ "UNKNOWN", # length data but missing species data
                                  T~species_code))


# estimate tl_cm for schooling fishes ------------------------------------------

# Step 1 - Filter the subsampled schooling fishes 
sub_fish <- data %>%
  filter(habitat_specific_spp_name %in% c("Seriphus politus", "Atherinops affinis"))

#find the known size frequency distribution at the MPA-year level. i.e., counts per size
size_distribution <- sub_fish %>%
  filter(!is.na(tl_cm)) %>%
  group_by(year, month, day, bioregion, region4, affiliated_mpa, mpa_state_class, mpa_state_designation, mpa_defacto_class, mpa_defacto_designation, tl_cm) %>%
  summarize(n = n()) %>%
  ungroup()


# Step 2 - Estimate size distribution for rows with NA in tl_cm
na_rows <- sub_fish %>% filter(is.na(tl_cm))

inferred_tl_cm <- data.frame()
set.seed(1985)
for (i in 1:nrow(na_rows)) {
  row <- na_rows[i, ]
  # Match the row to the size distribution
  matching_distribution <- size_distribution %>%
    filter(year == row$year, month == row$month, day == row$day, bioregion == row$bioregion, 
           region4 == row$region4, affiliated_mpa == row$affiliated_mpa, mpa_state_class == row$mpa_state_class, 
           mpa_state_designation == row$mpa_state_designation, mpa_defacto_class == row$mpa_defacto_class, 
           mpa_defacto_designation == row$mpa_defacto_designation)
  
  # Sample a size value from the matching distribution
  sampled_size <- sample(matching_distribution$tl_cm, size = 1, prob = matching_distribution$n) 
                            #matching_distribution$tl_cm extracts the tl_cm from matching_distribution which contains the known sizes at the MPA year level
                            #size = 1 samples one value from the distribution
                            #prob sets the sampling distribution from the known distribution
  
  # Create a new row with the inferred size
  inferred_row <- row
  inferred_row$tl_cm <- sampled_size
  
  # Add the inferred row to the result
  inferred_tl_cm <- rbind(inferred_tl_cm, inferred_row)
}

# Step 3-  Update the original data frame with the inferred tl_cm
inferred_size <- rbind(
  data %>%
    filter(!(habitat_specific_spp_name %in% c("Seriphus politus", "Atherinops affinis") & is.na(tl_cm))),
  inferred_tl_cm
)

#check that it worked
nrow(inferred_size)
nrow(data)

#new dist
ggplot(inferred_size %>% filter(habitat_specific_spp_name %in% c("Seriphus politus", "Atherinops affinis")), aes(x = tl_cm)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~ habitat_specific_spp_name, scales = "free_x") 

#orig dist
ggplot(data %>% filter(habitat_specific_spp_name %in% c("Seriphus politus", "Atherinops affinis")), aes(x = tl_cm)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~ habitat_specific_spp_name, scales = "free_x") 


# Check taxa NAs
taxa_na <- #data %>% #old
  inferred_size %>%
  filter(is.na(sciname) & !(species_code == "NO_ORG"))

# Test for matching taxa
taxa_match <- #data %>% #old
  inferred_size %>%
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


write.csv(inferred_size, row.names = FALSE, file.path(outdir, "surf_zone_processed.csv"))
#last export 26 Oct 2023


