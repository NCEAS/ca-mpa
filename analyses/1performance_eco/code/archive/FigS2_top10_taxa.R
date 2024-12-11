

# Setup --------------------------------------------------------------------------
rm(list=ls())

# Load required packages
library(tidyverse, ggplot2, tidytext)

# Set directories 
datadir <- "/home/shares/ca-mpa/data/sync-data/"
fig_dir <- here::here("analyses","1performance_eco","figures")

# Load data
surf_biomass <- read.csv(file.path(datadir, "monitoring/processed_data/biomass_processed/surf_zone_fish_biomass_updated.csv"))
kelp_biomass <- read.csv(file.path(datadir, "monitoring/processed_data/biomass_processed/kelpforest_fish_biomass_updated.csv")) 
ccfrp_biomass <- read.csv(file.path(datadir, "monitoring/processed_data/biomass_processed/ccfrp_fish_biomass_updated.csv"))
deep_biomass <- read.csv(file.path(datadir, "monitoring/processed_data/biomass_processed/deep_reef_fish_biomass_updated.csv"))

mpas_orig <- readRDS(file.path(datadir, "mpa_traits/processed", "CA_mpa_metadata.Rds")) %>% 
  dplyr::select(name = mpa, region) %>% 
  mutate(name = str_replace(name, " \\s*\\([^\\)]+\\)", ""),
         affiliated_mpa = tolower(name)) # fix name to match join

# Process Biomass DFs -----
surf_zone_raw <- surf_biomass %>% 
  filter(!is.na(weight_g)) %>%  # drop for now - these are all fishes that are unknown or species with no lengths (WARNING: currently drops one full haul!)
  mutate(target_status = if_else(species_code == "NO_ORG", "NO_ORG", target_status)) %>%  # helpful for inspecting 
  filter(!is.na(target_status))  # this drops: RFYOY, FFUN, HALI, Zoarcidae spp (after previous step to avoid dropping NO_ORG)

kelp_raw <- kelp_biomass %>% # WARNING: THE NA REMOVALS HERE DROPS LOTS OF TRANSECTS (~871)
  filter(!is.na(affiliated_mpa)) %>% # drops sites with no mpa (yellowbanks, trinidad, etc - see kf processing for details)
  filter(!is.na(weight_kg)) %>%  # drops fishes unknown or without lengths/conversion params
  mutate(target_status = if_else(species_code == "NO_ORG", "NO_ORG", target_status))

rocky_reef_raw <- ccfrp_biomass %>% # WARNING: NA REMOVALS DROPS 2 CELL TRIPS
  filter(!is.na(weight_kg)) %>%   #  drops fishes unknown or without lengths/conversion params
  mutate(target_status = if_else(species_code == "NO_ORG", "NO_ORG", target_status)) %>%  # helpful for inspecting 
  filter(!is.na(target_status)) # drop for now - spp without target status identified (see notes for details)

deep_reef_raw <-  deep_biomass %>% # WARNING: NA REMOVALS DROPS 12 TRANSECTS
  filter(!is.na(weight_kg)) %>% # drop fishes unknown or without lengths/conversion params
  mutate(target_status = if_else(species_code == "NO_ORG" & is.na(target_status), "NO_ORG", target_status)) %>%  # helpful for inspecting 
  filter(!is.na(target_status))


## Surf Zone  -----------------------------------------------------------------------------------------------------

# Calculate total counts of each species for each rep unit (haul)
surf_build1 <- surf_zone_raw %>%
  #add MPA meta data
  left_join(., mpas_orig) %>%
  group_by(year, month, day, 
            region,
           affiliated_mpa, 
           mpa_defacto_class, mpa_defacto_designation,
           haul_number, genus, species, sciname, target_status) %>%
  dplyr::summarize(total_counts = sum(count)) %>%
  #find most recent sampling year for each MPA
  group_by(affiliated_mpa) %>%
  filter(year == max(year))%>%
  ungroup() %>%
  #drop true zeros
  filter(!(target_status == "NO_ORG")) %>%
  #calculate counts average by species across all years and hauls
  group_by(region, genus, species, sciname, target_status) %>%
  summarize(mean_counts = mean(total_counts, na.rm=TRUE),
            sd_counts = mean(total_counts),
            se_counts = sd_counts / sqrt(n())) %>%
  mutate(ecosystem = "Surf zone")

## Kelp Forest --------------------------------------------------------------------------------------------

# calculate total counts for each rep unit (transect)
kelp_build1 <- kelp_raw %>% 
  left_join(., mpas_orig) %>%
  group_by(year, 
           month, day, region, site, 
           affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, 
           zone, transect, genus, species, sciname, target_status) %>%
  #drop NO_ORG
  filter(!(target_status == "NO_ORG"))%>%
  summarise(total_counts = sum(count)) %>%
  #retain only the most recent sampling year for each MPA
  group_by(affiliated_mpa) %>%
  filter(year == max(year))%>%
  ungroup() %>%
  #calculate counts average by species across all years and transects
  group_by(region, genus, species, sciname, target_status) %>%
  summarize(mean_counts = mean(total_counts),
            sd_counts = mean(total_counts),
            se_counts = sd_counts / sqrt(n())) %>%
  mutate(ecosystem = "Kelp forest")



## CCFRP ---------------------------------------------------------------------------------------
### Treats cell-trip as the unit of replication
### Approached updated by CL September 2023

# Calculate number of cell-trips in full data - 2413
ccfrp_effort <- ccfrp_biomass %>% 
  distinct(id_cell_per_trip)

# Calculate number of cell-trips after NA drop - 2411
# 2 full cell-trips are dropped due to missing data
ccfrp_effort_drop <- rocky_reef_raw %>% 
  distinct(id_cell_per_trip)

# Calculated biomass per unit effort (trip-cell)
ccfrp_build1 <- rocky_reef_raw %>% 
  left_join(., mpas_orig) %>%
  select(year, region, affiliated_mpa, 
         mpa_defacto_class, mpa_defacto_designation,
         id_cell_per_trip, species_code, sciname, target_status,
         total_angler_hrs_cell, weight_kg) %>% 
  #drop NA region (== TRinidad, drop per PI)
  filter(!(is.na(region))) %>%
  group_by(year, region, affiliated_mpa, 
           mpa_defacto_class, mpa_defacto_designation,
           id_cell_per_trip, species_code, sciname, target_status,
           total_angler_hrs_cell) %>%
  #calculate no. caught per spp
  summarize(total_counts = n()) %>%
  #find most recent sampling year for each MPA
  group_by(affiliated_mpa) %>%
  filter(year == max(year))%>%
  ungroup() %>%
  #calculate cpue
  mutate(cpue = total_counts / total_angler_hrs_cell) %>%
  #calcualte average cpue per spp across all replicate trip-cells
  group_by(region, sciname, target_status) %>%
  summarize(mean_counts = mean(cpue, na.rm=TRUE),
            sd_counts = mean(total_counts),
            se_counts = sd_counts / sqrt(n())) %>%
  #drop NO_ORG
  filter(!(target_status == "NO_ORG")) %>%
  mutate(ecosystem = "Shallow reef",
         #all spp are targeted per PI
         target_status = "Targeted") 


## Deep Reef --------------------------------------------------------------------


# Calculate number of transects in full dataset (before drop NAs) 
# 1647
deep_effort <- deep_biomass %>% 
  distinct(year, affiliated_mpa, line_id)

# Compare to number of transects after dropping NAs
# 1635
deep_effort_drop <- deep_reef_raw %>% 
  distinct(year, affiliated_mpa, line_id)

# calculate total biom for each rep unit (transect - aka line_id)
deep_build1 <- deep_reef_raw %>%
  left_join(., mpas_orig) %>%
  group_by(year, region, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation,
           line_id, genus, species, sciname, target_status) %>%
  dplyr::summarize(total_counts = sum(count)) %>%
  #find most recent sampling year for each MPA
  group_by(affiliated_mpa) %>%
  filter(year == max(year))%>%
  ungroup() %>%
  group_by(region, sciname, target_status) %>%
  summarize(mean_counts = mean(total_counts, na.rm=TRUE),
            sd_counts = mean(total_counts),
            se_counts = sd_counts / sqrt(n())) %>%
  #drop NO_ORG column
  filter(!(target_status=="NO_ORG")) %>%
  mutate(ecosystem = "Deep reef")


# Join data ----------------------------------------------------------------------------

spp_counts <- rbind(surf_build1, kelp_build1, ccfrp_build1, deep_build1)

#prep for plotting
top_species_overall <- spp_counts %>%
  group_by(region, ecosystem) %>%
  slice_max(mean_counts, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(ecosystem = factor(ecosystem, levels = c("Surf zone", "Kelp forest", "Shallow reef", "Deep reef"))) %>%
  # Split the sciname into genus and species
  separate(sciname, into = c("genus", "species"), sep = " ", extra = "merge", fill = "right") %>%
  # Abbreviate genus if species is not "spp."
  mutate(abbr_sciname = ifelse(species == "spp", 
                               paste(genus, species),               # Keep full genus for "spp."
                               paste0(substr(genus, 1, 1), ". ", species))) %>%  # Abbreviate genus for specific species
  arrange(region, ecosystem, mean_counts)


# Custom theme for the plot
base_theme <-  theme(axis.text=element_text(size=6, color = "black"),
                     axis.text.y = element_text(color = "black", size=6, face = "italic"),
                     axis.text.x = element_text(color = "black", size=6),
                     axis.title=element_text(size=7, color = "black"),
                     plot.tag=element_text(size=6, color = "black"), 
                     plot.title=element_text(size=6, face="bold", color = "black"),
                     # Gridlines 
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.key = element_blank(),
                     legend.background = element_rect(fill=alpha('blue', 0)),
                     #legend.key.size  = unit(0.5, "lines"), 
                     legend.text = element_text(size = 6, color = "black"),
                     legend.title = element_text(size = 6, color = "black"),
                     # Facet labels
                     strip.background = element_blank(),
                     strip.text = element_text(size = 7, face="bold", color = "black")
)


ecosystem_labels <- c(
  "Surf zone" = "Surf zone \n(no. per haul)",
  "Kelp forest" = "Kelp forest \n(no. per transect)",
  "Shallow reef" = "Shallow reef \n(no. per cell trip)",
  "Deep reef" = "Deep reef \n(no. per transect)"
)

#plot
p <- ggplot(top_species_overall, aes(x = mean_counts, y = reorder_within(abbr_sciname, mean_counts, region:ecosystem), fill = target_status)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(xmin = pmax(mean_counts - se_counts, 0), xmax = mean_counts + se_counts), 
                width = 0.2, 
                position = position_dodge(0.9)) +  
  ggh4x::facet_grid2(region ~ ecosystem, scales = "free", independent = "all", 
                     labeller = labeller(ecosystem = ecosystem_labels)) +  
  scale_y_reordered() + 
  labs(x = "Density (counts per haul, cell trip, or transect)", y = "", fill = "Target \nstatus") +  
  theme_minimal() +
  theme(legend.position = "top") +  
  scale_fill_manual(values = c("Targeted" = "indianred", "Nontargeted" = "navyblue")) +  
  theme_bw() + base_theme 

p


ggsave(p, filename=file.path(fig_dir, "FigSX.png"), bg = "white",
       width=9, height=6, units="in", dpi=600) 











