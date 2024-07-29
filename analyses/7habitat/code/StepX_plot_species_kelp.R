# Calculate the 1000m totals for each site
# Cori Lopazanski
# July 2024


# Setup   ----------------------------------------------------------------------
rm(list = ls())

library(tidyverse)
library(sf)

fig.dir <- "~/ca-mpa/analyses/7habitat/figures"
com.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/combined"
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data"
sp.dir <- "/home/shares/ca-mpa/data/sync-data/species_traits/processed"

# Build  ----------------------------------------------------------------------
# Find the common geometry classification
# Define columns used to create groups: want to combine per site, per PMEP Zone (depth)
site_columns <- c("habitat", "mpa", "mpa_orig", "site", "site_type", "PMEP_Section", "PMEP_Zone")
bio_columns <- c("FaunalBed", "AquaticVegetationBed", "BenthicMacroalgae", "Kelp", "OtherMacroalgae", "EmergentWetland", "ScrubShrubWetland", "ForestedWetland", "Seagrass", "AquaticVascularVegetation", "FloatingSuspendedBiota")


# Read the combined data with hard-soft-biotic classification
habitat_raw <- list.files(file.path(com.dir, "combined_mlpa_sites_1000m"), pattern = "combined_hsb", full.names = T) %>% 
  lapply(function(file){
    readRDS(file) %>% 
      st_drop_geometry()}) %>%
  bind_rows() 

rownames(habitat_raw) <- NULL

habitat <- habitat_raw %>% 
  mutate(depth_zone = factor(case_when(PMEP_Zone == '0' ~ "Landward",
                                       PMEP_Zone %in% c('1', '2', '3') ~ "Shoreline to -30m",
                                       PMEP_Zone %in% c('4', '5') ~ "-30m to -100m",
                                       PMEP_Zone %in% c('6', '7') ~ "-100m to -200m",
                                       PMEP_Zone == '8' ~ ">-200m or International Waters"),
                             levels = c("Landward", "Shoreline to -30m",
                                        "-30m to -100m", "-100m to -200m", ">-200m or International Waters")),
         depth_zone_simple = case_when(depth_zone == "Landward" ~ "landward",
                                       depth_zone == "Shoreline to -30m" ~ "0_30m",
                                       depth_zone == "-30m to -100m" ~ "30_100m",
                                       depth_zone == "-100m to -200m" ~ "100_200m", 
                                       depth_zone == ">-200m or International Waters" ~ "200m")) %>% 
  # There are a few sites that span across sections - this grouping will summarize those totals
  group_by(habitat, mpa, mpa_orig, site, site_type, habitat_class, depth_zone, depth_zone_simple) %>% 
  summarize(area_m2 = sum(area_m2, na.rm = T), .groups = 'drop') %>% 
  mutate(habitat_class = snakecase::to_snake_case(habitat_class)) %>% 
  mutate(habitat_depth = paste0(habitat_class, "_", depth_zone_simple))

habitat_totals <- habitat %>% 
  group_by(habitat, mpa, mpa_orig, site_type, habitat_class, depth_zone, depth_zone_simple, habitat_depth) %>% 
  summarize(total_area_m2_sites = sum(area_m2, na.rm = T),
            mean_area_m2_sites = mean(area_m2, na.rm = T))


# Read the kelp forest monitoring data
kelp <- readRDS(file.path(ltm.dir, "biomass_site_year/kelp_biomass_site_year.Rds"))


# Read the species table
sp <- readRDS(file.path(sp.dir, "species_lw_habitat.Rds")) %>% 
  select(genus, sciname = species, level, target_status,
         assemblage) %>%
  distinct() %>% 
  filter(!is.na(assemblage)) %>%  # Start with those that have been classified
  filter(!(sciname == "Sebastes miniatus" & assemblage == "Soft-hard"))

habitat_wide <- habitat %>% ungroup() %>% 
  select(site, site_type, habitat_depth, area_m2) %>% 
  pivot_wider(names_from = habitat_depth, values_from = area_m2)

habitat_totals_wide <- habitat_totals %>% ungroup() %>% 
  select(habitat, mpa, mpa_orig, site_type, habitat_depth, total_area_m2_sites) %>% 
  pivot_wider(names_from = habitat_depth, values_from = mean_area_m2_sites) %>% 
  filter(habitat == "Kelp")

kelp_df <- kelp %>% 
  left_join(sp) %>% 
  rename(mpa_orig = affiliated_mpa) %>% 
  mutate(site_type = if_else(mpa_defacto_designation == "ref", "Reference", "MPA")) %>% 
  mutate(mpa_orig = recode(mpa_orig,
                           "swami's smca" = "swamis smca"))

kelp_df2 <- kelp_df %>% 
  mutate(bpue = total_biomass_kg/n_rep) %>% 
  group_by(sciname, target_status, assemblage, year, mpa_orig, site_type) %>% 
  summarize(total_bpue_sites = sum(bpue)) %>% 
  left_join(habitat_totals_wide) %>% 
  clean_names() %>% ungroup()


ggplot(data = kelp_df2) +
  geom_point(aes(x = hard_bottom_0_30m, y = total_bpue_sites, color = site_type)) +
  geom_smooth(aes(x = hard_bottom_0_30m, y = total_bpue_sites, color = site_type)) +
  facet_wrap(~site_type)

kelp_df3 <- kelp_df2 

x_vars <- unique(habitat$habitat_depth)
x_vars <- c("hard_bottom_0_30m", "hard_bottom_30_100m", "hard_bottom_100_200m", "hard_bottom_200m",
            "soft_bottom_0_30m","soft_bottom_30_100m", "soft_bottom_100_200m", "soft_bottom_200m",
            "hard_bottom_biotic_0_30m", "hard_bottom_biotic_30_100m", "hard_bottom_biotic_100_200m",
            "soft_bottom_biotic_0_30m","soft_bottom_biotic_30_100m", "soft_bottom_biotic_100_200m",     
            "biotic_0_30m","biotic_30_100m")  

create_plot <- function(group, x_var, sciname, target_status, assemblage) {
  if (x_var %in% colnames(group) && !all(is.na(group[[x_var]]))) {
    if (sum(!is.na(group$total_bpue_sites)) > 30) {
      return(ggplot(group, aes_string(x = x_var, y = "total_bpue_sites", color = "site_type")) +
               geom_point() + 
               geom_smooth(method = "glm", formula = y ~ x) +
               labs(
                 title = paste("Species:", sciname,
                               "\nTarget status:", target_status,
                               "\nAssemblage:", assemblage),
                 x = x_var,
                 y = "Total BPUE per affiliated MPA per year",
                 color = "Site type") +
               theme_minimal() +
               theme(plot.title = element_text(size = 10)) +
               facet_wrap(~site_type))
    }
  }
  return(ggplot() + theme_void()) # Return blank plot if conditions are not met
}

# Create a PDF file to save the plots
pdf(file.path(fig.dir, "species_plots_glm.pdf"), width = 24, height = 20)  # Adjust size to fit 18 plots

kelp_df3 %>%
  group_by(sciname, target_status, assemblage) %>%
  group_split() %>%
  walk(function(group) {
    sciname <- unique(group$sciname)
    target_status <- unique(group$target_status)
    assemblage <- unique(group$assemblage)
    
    plot_list <- lapply(x_vars, create_plot, group = group, sciname = sciname, target_status = target_status, assemblage = assemblage)
    plot_list <- plot_list[!sapply(plot_list, is.null)] # drop null plots
    
    num_plots <- length(plot_list) # want exactly 16 plots
    if (num_plots < 16) {
      plot_list <- c(plot_list, replicate(16 - num_plots, ggplot() + theme_void(), simplify = FALSE))
    }
    
    if (!all(sapply(plot_list, function(p) inherits(p, "ggplot") && identical(p$labels$title, NULL)))) { # don't plot if all blank
      grid.arrange(grobs = plot_list, 
                   layout_matrix = rbind(c(1, 2, 3, 4),
                                         c(5, 6, 7, 8),
                                         c(9, 10, 11, NA),
                                         c(12, 13, 14, NA),
                                         c(15, 16, NA, NA)))
    }
  })

dev.off()

