# Process Rocky Intertidal Monitoring Data
# Cori Lopazanski


# Setup ------------------------------------------------------------------------

# Packages
library(tidyverse)
library(janitor)

# Directories
basedir <- "/Volumes/GoogleDrive-105151121202188525604/Shared drives/NCEAS MPA network assessment/MPA Network Assessment: Working Group Shared Folder/data/sync-data/monitoring"

# Read Data
int_raw <- read_csv(file.path(basedir, "monitoring_rocky-intertidal", "Community analysis",
                              "intertidal_site_counts.csv")) %>% clean_names()

# Build

int <- int_raw %>% 
  filter(year %in% c(2016:2020)) %>% 
  filter(affiliated_mpa != "NONE") %>%
  filter(mpa_designation != "NONE") %>% 
  pivot_longer(cols = antele:zosmar,
               names_to = "species",
               values_to = "amount") %>% 
  group_by(affiliated_mpa, species) %>% 
  dplyr::summarize(avg_amount = mean(amount, na.rm = TRUE)) %>% 
  pivot_wider(names_from = species,
              values_from = avg_amount)


# Export
saveRDS(int, file = file.path("analyses", "7habitat", "intermediate_data",
                              "species_matrix_rocky_1620.Rds"))
