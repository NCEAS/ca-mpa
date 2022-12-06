# Process ROMS Habitat
# C. Lopazanski 
# 30 Aug 2022

# Setup ------------------------------------------------------------------------
# Packages
library(tidyverse)

# Clear workspace
rm(list = ls())

# Directories
data.dir <- file.path(getwd(), "analyses", "7habitat", "intermediate_data")
plot.dir <- file.path(getwd(), "analyses", "7habitat", "figures")

# Read ROMS (Habitat) Data
raw <- readxl::read_excel(file.path(data.dir, "roms-habitat.xlsx")) %>% 
  janitor::clean_names()

# Build ------------------------------------------------------------------------
# Define regions and lengthen
roms_data <- raw %>% 
  mutate(region_4 = cut(x = latitude,
                        breaks = c(42.03, 39, 37.18, 34.5, 32),
                        labels = c("South", "Central", "North Central", "North"))) %>% 
  select(cell:cell_mpa, region_4, everything()) %>% 
  pivot_longer(cols = hard_0_30m:last_col(),
               names_to = "habitat",
               values_to = "habitat_amount") 

# Specify habitats
key_habitats = c("coastal_marsh", "rocky_intertidal_and_cliff", "sandy_or_gravel_beaches",
                 "tidal_flats", "average_kelp", "hard_0_30m", "hard_30_100m", "hard_100_200m", "hard_200_3000m",
                 "soft_0_30m", "soft_30_100m", "soft_100_200m", "soft_200_3000m")

hab_colors <- c("peachpuff2", # sandy
                "steelblue3", # intertidal
                "palegreen3", # coastal marsh
                "burlywood4",# tidal flats
                "seagreen4", # kelp 
                "tan1", "tan2", "tan3", "tan4", # hard
                "wheat", "wheat1", "wheat2", "wheat3") # soft

# Filter data for habitats
data_core <- roms_data %>% 
  filter(habitat %in% key_habitats) %>% 
  mutate(habitat = recode_factor(habitat,
                                 "sandy_or_gravel_beaches" = "Sandy beach",
                                 "rocky_intertidal_and_cliff" = "Rocky intertidal", 
                                 "coastal_marsh" = "Coastal marsh",
                                 "tidal_flats" = "Tidal flats",
                                 "average_kelp" = "Kelp canopy",
                                 "coastal marsh" = "Coastal marsh", 
                                 "hard_0_30m" = "Hard substrate (0-30m)", 
                                 "hard_30_100m" = "Hard substrate (30-100m)", 
                                 "hard_100_200m" = "Hard substrate (100-200m)", 
                                 "hard_200_3000m" = "Hard substrate (200-3000m)",
                                 "soft_0_30m" = "Soft substrate (0-30m)", 
                                 "soft_30_100m" = "Soft substrate (30-100m)", 
                                 "soft_100_200m" = "Soft substrate (100-200m)", 
                                 "soft_200_3000m" = "Soft substrate (200-3000m)")) %>% 
  mutate(habitat_type = if_else(habitat %in% c("Sandy beach", "Rocky intertidal",
                                                 "Coastal marsh", "Tidal flats"), "linear",
                                  "area"))

data_mpa <- data_core %>% 
  filter(!is.na(mpa))

# Summarize ------------------------------------------------------------------------

# Regional totals
region_all <- data_core %>% 
  group_by(region_4, habitat, habitat_type) %>% 
  summarize(total_habitat = sum(habitat_amount))

region_mpa <- data_mpa %>% 
  group_by(region_4, habitat, habitat_type) %>% 
  summarize(mpa_habitat = sum(habitat_amount))

region <- full_join(region_all, region_mpa) %>% 
  mutate(pct_rep = mpa_habitat/total_habitat*100)

#Statewide totals
state_all <- data_core %>% 
  group_by(habitat, habitat_type) %>% 
  summarize(total_habitat = sum(habitat_amount)) %>% 
  group_by(habitat_type) %>% 
  mutate(total_type = sum(total_habitat)) %>% 
  ungroup() %>% 
  mutate(pct_comp = total_habitat/total_type*100)

state_mpa <- data_mpa %>% 
  group_by(habitat, habitat_type) %>% 
  summarize(mpa_habitat = sum(habitat_amount)) %>% 
  group_by(habitat_type) %>% 
  mutate(mpa_type = sum(mpa_habitat)) %>% 
  ungroup() %>% 
  mutate(mpa_pct_comp = mpa_habitat/mpa_type*100)

state <- full_join(state_all, state_mpa) 

state_long <- state %>% 
  pivot_longer(cols = total_habitat:pct_rep,
               names_to = "type", values_to = "amount") %>% 
  mutate(type = recode_factor(type,
                              "pct_comp_mpa" = "MPA Network",
                              "pct_comp_all" = "Statewide")) 

# Plot -------------------------------------------------------------------------
ggplot()+
  geom_col(data = state_long %>% 
             filter(type %in% c("Statewide", "MPA Network")),
           aes(x = type, y = amount, fill = habitat)) +
  coord_flip() +
  scale_fill_manual(name = "Habitat Type", 
                    values = hab_colors) +
  labs(y = "Percent Contribution to Total Habitat Area",
       x = "") +
  scale_y_continuous(limits = c(0,101), expand = c(0,0)) +
  facet_grid(~habitat_type)+
  theme_classic()

# Export -----------------------------------------------------------------------
