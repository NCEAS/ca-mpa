# Process ROV Monitoring Data



# Setup ------------------------------------------------------------------------

# Packages
library(tidyverse)
library(janitor)

# Directories
basedir <- "/Volumes/GoogleDrive-105151121202188525604/Shared drives/NCEAS MPA network assessment/MPA Network Assessment: Working Group Shared Folder/data/sync-data/monitoring"

# Read Data
rov_raw <- read_csv(file.path(basedir, "monitoring_deep-reef", "ROV_Dataset",
                          "MidDepth_ROV_Fish_Count.csv"), na = c("N/A", "NA")) %>% clean_names()


# Build Data -------------------------------------------------------------------

rov <- rov_raw %>% 
  filter(type %in% c("SMR", "SMCA")) %>% 
  filter(!(scientific_name == "")) %>% 
  group_by(year, mpa_group, type, designation, scientific_name) %>% 
  dplyr::summarize(total_count = sum(count)) %>% 
  ungroup() %>% 
  rename(mpa_class = type) %>% 
  mutate(name = str_to_lower(paste(mpa_group, mpa_class))) %>%
  mutate(mpa_class = str_to_lower(mpa_class),
         name = str_replace(name, "se f", "southeast f"),
         name = str_replace(name, "n f", "north f"),
         name = str_replace(name, "islands", "island"),
         name = str_replace(name, "farnsworth", "farnsworth offshore"),
         name = str_replace(name, "george", "george reef offshore"),
         designation = str_to_lower(designation)) %>% 
  select(year, name, designation, scientific_name, total_count)

rov$name[rov$name == "farallon island smr"] = "southeast farallon island smr"
rov$name[rov$name == "farallon island smca"] = "southeast farallon island smca"

# Summary Dataframe -------------------------------------------------------------
rov_1620 <- rov %>% 
  filter(year %in% c(2016:2020)) %>% 
  filter(designation == "mpa") %>% 
  group_by(name, scientific_name) %>% 
  dplyr::summarize(total_count = sum(total_count, na.rm = TRUE))

# Species by Community Matrix ---------------------------------------------------
sp_matrix <- rov_1620 %>% 
  pivot_wider(id_cols = name,
              names_from = scientific_name,
              values_from = total_count)

sp_matrix[is.na(sp_matrix)] <- 0

saveRDS(sp_matrix, file = file.path("analyses", "7habitat", "intermediate_data",
                                    "species_matrix_rov_1620.Rds"))

