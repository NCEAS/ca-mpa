# Fix Site Names
# Cori Lopazanski
# Nov 2024


# Challenge that surf zone sites did not have names saved, so no way to identify them in the 
# monitoring data.
# Found a full dataframe with site names attached still; add those to the cleaned monitoring sites data,
# can then attach them to habitat!

rm(list = ls())

site_table <- read.csv("/home/shares/ca-mpa/data/sync-data/monitoring/site_tables/processed/monitoring_site_table.csv") %>% 
  distinct(group, mlpa_region, site_name, affiliated_mpa, tier, mpa_designation, lat_wgs84, lon_wgs84) %>% 
  filter(group == "Surf zone") %>% 
  dplyr::select(site_name, lat_dd = lat_wgs84, long_dd = lon_wgs84) %>% 
  mutate(lat_dd = round(lat_dd, 5),
         long_dd = round(long_dd, 5))

site_clean <- readRDS("/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_sites_clean.Rds") %>% 
  dplyr::select(habitat, affiliated_mpa = mpa, site, site_type, lat_dd, long_dd) %>% 
  filter(habitat == "Surf zone") %>% 
  mutate(lat_dd = round(lat_dd, 5),
         long_dd = round(long_dd, 5))


surf <- full_join(site_clean, site_table)

surf_clean <- surf %>% 
  mutate(affiliated_mpa = str_to_lower(affiliated_mpa)) %>% 
  mutate(affiliated_mpa = recode(affiliated_mpa, "campus point smca (no-take)" = "campus point smca")) %>% 
  # Match to surf zone actual names
  mutate(site_name = recode(site_name,
                            "Doran County Park" = "Doran",
                            "Drakes Beach" = "Drake's",
                            "Four Mile Beach" ="Four Mile",
                            "Leo Carrillo State Beach" = "Leo Carrillo",
                            "Mad River Beach" = "Mad River",
                            "Percos Beach" = "Percos",
                            "Refugio State Beach" = "Refugio",
                            "San Elijo State Beach" = "San Elijo",
                            "Twin Lakes Beach" = "Twin Lakes",
                            "South Campus Beach" = "South Campus",
                            "Scripps Beach" = "Scripps",
                            "Strand Beach" = "Strand",
                            "Asilomar" = "Spanish Bay")) %>% 
  mutate(affiliated_mpa = case_when(site_name == "Stillwater Cove" ~ "point lobos smr", # confirmed in raw data
                                    T~affiliated_mpa))

saveRDS(surf_clean, "/home/shares/ca-mpa/data/sync-data/monitoring/site_tables/processed/surf_site_names.Rds")                  
