# Process CCFRP Monitoring Data
# Cori Lopazanski


# Setup ------------------------------------------------------------------------

# Packages
library(tidyverse)
library(janitor)

# Directories
basedir <- "/Volumes/GoogleDrive-105151121202188525604/Shared drives/NCEAS MPA network assessment/MPA Network Assessment: Working Group Shared Folder/data/sync-data/monitoring"

# Read Data
ccfrp_raw <- read_csv(file.path(basedir, "monitoring_ccfrp", "CCFRP_derived_data_tables_DataONE",
                                "CCFRP_derived_effort_table.csv")) %>% clean_names()


# Build ------------------------------------------------------------------------

ccfrp <- ccfrp_raw %>% 
  select(ca_mpa_name_short, area, mpa_designation = mpa_status, year, common_name:count) %>% 
  mutate(mpa_designation = str_to_lower(mpa_designation),
         affiliated_mpa = if_else(is.na(ca_mpa_name_short), 
                                  paste(str_to_lower(area), "smr"), 
                                  str_to_lower(ca_mpa_name_short))) %>% 
  select(affiliated_mpa, mpa_designation, year, common_name:count)

ccfrp_1620 <- ccfrp %>% 
  filter(year %in% c(2016:2020)) %>% 
  filter(mpa_designation == "mpa") %>% 
  group_by(affiliated_mpa, common_name) %>% 
  dplyr::summarize(total_count = sum(count))
  
sp_matrix <- ccfrp_1620 %>% 
  pivot_wider(names_from = common_name,
              values_from = total_count)

sp_matrix[is.na(sp_matrix)] <- 0

saveRDS(sp_matrix, file = file.path("analyses", "7habitat", "intermediate_data",
                                    "species_matrix_ccfrp_1620.Rds"))
