# Plot hard-soft-biotic categories for each site
# Cori Lopazanski
# July 2024


# Setup   ----------------------------------------------------------------------
library(tidyverse)
library(sf)

fig.dir <- "~/ca-mpa/analyses/7habitat/figures"
com.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/combined"


# Build  ----------------------------------------------------------------------
# Find the common geometry classification
# Define columns used to create groups: want to combine per site, per PMEP Zone (depth)
site_columns <- c("habitat", "mpa", "mpa_orig", "site", "site_type", "PMEP_Section", "PMEP_Zone")
bio_columns <- c("FaunalBed", "AquaticVegetationBed", "BenthicMacroalgae", "Kelp", "OtherMacroalgae", "EmergentWetland", "ScrubShrubWetland", "ForestedWetland", "Seagrass", "AquaticVascularVegetation", "FloatingSuspendedBiota")


# Read the combined data with hard-soft-biotic classification
section <- "41"
habitat <- readRDS(file.path(com.dir, paste0("combined_mlpa_sites_1000m/combined_hsb_", section, ".Rds")))

# Site / Habitat
grouped_df <- habitat %>% 
  group_by(habitat, mpa, site_type, site) %>% 
  group_split()

hab_colors <- c("Hard Bottom" = "tan4",
                "Hard Bottom Biotic" = "palegreen4",
                "Soft Bottom" = "wheat",
                "Soft Bottom Biotic" = "palegreen3",
                "Biotic" = "palegreen")

pdf(file.path(fig.dir, "sites_1000m_plots/site_plots_41.pdf"))

for(group in grouped_df){
  mpa <- unique(group$mpa)
  habitat <- unique(group$habitat)
  site_type <- unique(group$site_type)
  site <- unique(group$site)
  
  plot <- ggplot(group) +
    geom_sf(aes(fill = habitat_class)) +
    ggtitle(paste("MPA:", mpa, "\nHabitat:", habitat, "\nSite Type:", site_type, "\nSite:", site)) +
    scale_fill_manual(values = hab_colors) +  
    theme_minimal() +
    theme(plot.title = element_text(size = 10))
  
  print(plot)
}

dev.off()



# Read the combined data with hard-soft-biotic classification
habitat_raw <- list.files(file.path(com.dir, "combined_mlpa_sites_1000m"), pattern = "combined_hsb", full.names = T) %>% 
  lapply(function(file){
    readRDS(file) %>% 
      st_drop_geometry()}) %>%
  bind_rows() 

habitat <- habitat_raw %>% 
  mutate(depth_zone = factor(case_when(PMEP_Zone == '0' ~ "Landward",
                                       PMEP_Zone %in% c('1', '2', '3') ~ "Shoreline to -30m",
                                       PMEP_Zone %in% c('4', '5') ~ "-30m to -100m",
                                       PMEP_Zone %in% c('6', '7') ~ "-100m to -200m",
                                       PMEP_Zone == '8' ~ ">-200m or International Waters"),
                             levels = c("Landward", "Shoreline to -30m",
                                        "-30m to -100m", "-100m to -200m", ">-200m or International Waters"))) %>% 
  group_by(habitat, mpa, mpa_orig, site, site_type, habitat_class, depth_zone) %>% 
  summarize(area_m2 = sum(area_m2, na.rm = T)) 

# Create histograms comparing totals for each habitat class in MPAs and Reference areas 
habitat_grouped <- habitat %>% 
  group_by(habitat, habitat_class) %>% 
  group_split()


pdf(file.path(fig.dir, "sites_1000m_plots/sites_1000m_hsb_histograms.pdf"),
    width = 6, height = 4)

for(group in habitat_grouped){
  
  habitat <- unique(group$habitat)
  habitat_class <- unique(group$habitat_class)
  
  plot <- ggplot(group) +
    geom_histogram(aes(x = area_m2, fill = site_type)) +
    ggtitle(paste("Habitat:", habitat, "\nClass:", habitat_class)) +
    theme_minimal() +
    theme(plot.title = element_text(size = 10)) +
    facet_wrap(~site_type)
  
  print(plot)
}

dev.off()

mpa_average <- habitat %>% ungroup() %>% 
  filter(habitat == "Kelp") %>% 
  group_by(habitat, mpa, site_type, habitat_class, depth_zone) %>% 
  summarize(mean_m2 = mean(area_m2, na.rm = T)) %>% 
  pivot_wider(names_from = site_type, values_from = mean_m2) %>% 
  replace_na(list(MPA = 0, Reference = 0)) %>%
  filter(!is.na(mpa))

ggplot(data = mpa_average) + 
  geom_point(aes(x = MPA, y = Reference, color = depth_zone)) +
  geom_abline(slope=1, linetype = "dashed", color="Red") +
  facet_wrap(~habitat_class)
