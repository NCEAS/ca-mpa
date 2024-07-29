# Calculate the 1000m totals for each site
# Cori Lopazanski
# July 2024


# Setup   ----------------------------------------------------------------------
rm(list = ls())

library(tidyverse)
library(janitor)
library(sf)

fig.dir <- "~/ca-mpa/analyses/7habitat/figures"
hab.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/combined/combined_mlpa_sites_1000m"
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data"
sp.dir <- "/home/shares/ca-mpa/data/sync-data/species_traits/processed"

# Read  ----------------------------------------------------------------------
# Read the combined data with hard-soft-biotic classification
habitat_raw <- list.files(file.path(hab.dir, "buffers/250m"), pattern = "combined_hsb", full.names = T) %>% 
  lapply(function(file){
    readRDS(file) %>% 
      st_drop_geometry()}) %>%
  bind_rows() 

rownames(habitat_raw) <- NULL

# Read the species table
sp_raw <- readRDS(file.path(sp.dir, "species_lw_habitat.Rds")) 

# Read the kelp forest monitoring data
kelp_raw <- readRDS(file.path(ltm.dir, "biomass_site_year/kelp_biomass_site_year.Rds"))
kelp_effort <- readRDS(file.path(ltm.dir, "biomass_site_year/kelp_site_year_effort.Rds")) %>% ungroup()


# Build  ----------------------------------------------------------------------
## Habitat -----
habitat <- habitat_raw %>% 
  filter(habitat == "Kelp") %>% 
  mutate(depth_zone = factor(case_when(PMEP_Zone == '0' ~ "Landward",
                                       PMEP_Zone %in% c('1', '2', '3') ~ "Shoreline to -30m",
                                       PMEP_Zone %in% c('4', '5') ~ "-30m to -100m",
                                       PMEP_Zone %in% c('6', '7') ~ "-100m to -200m",
                                       PMEP_Zone == '8' ~ ">-200m or International Waters"),
                             levels = c("Landward", "Shoreline to -30m",
                                        "-30m to -100m", "-100m to -200m", "-200m or International Waters")),
         depth_zone_simple = case_when(depth_zone == "Landward" ~ "landward",
                                       depth_zone == "Shoreline to -30m" ~ "0_30m",
                                       depth_zone == "-30m to -100m" ~ "30_100m",
                                       depth_zone == "-100m to -200m" ~ "100_200m", 
                                       depth_zone == "-200m or International Waters" ~ "200m")) %>% 
  # There are a few sites that span across sections - this grouping will summarize those totals
  group_by(mpa, mpa_orig, site, site_type, habitat_class, depth_zone, depth_zone_simple) %>% 
  summarize(area_m2 = sum(area_m2, na.rm = T), .groups = 'drop') %>% 
  mutate(habitat_class = snakecase::to_snake_case(habitat_class)) %>% 
  mutate(habitat_depth = paste0(habitat_class, "_", depth_zone_simple)) %>%
  dplyr::select(!c(depth_zone, depth_zone_simple, habitat_class)) %>% 
  # Widen to fill in the appropriate zeroes across each site
  pivot_wider(names_from = habitat_depth, values_from = area_m2) %>% 
  mutate_at(vars(biotic_0_30m:biotic_30_100m), ~ replace(., is.na(.), 0)) %>% 
  # Lengthen
  pivot_longer(cols = biotic_0_30m:biotic_30_100m, names_to = "habitat_depth", values_to = "area_m2") %>% 
  # Find the total and average for each habitat class, across all the sites affiliated with each MPA
  group_by(mpa, mpa_orig, site_type, habitat_depth) %>% 
  summarize(area_m2 = sum(area_m2, na.rm = T),
            mean_area_m2 = mean(area_m2, na.rm = T), .groups = 'drop') %>% 
  filter(!is.na(mpa)) 

habitat_wide <- habitat %>% 
  dplyr::select(mpa, mpa_orig, site_type, habitat_depth, mean_area_m2) %>% 
  pivot_wider(names_from = habitat_depth, values_from = mean_area_m2) 
  
## Species -----
sp <- sp_raw %>% 
  dplyr::select(genus, sciname = species, level, target_status, assemblage) %>%
  distinct() %>% 
  filter(!is.na(assemblage)) %>%  # Start with those that have been classified
  filter(!(sciname == "Sebastes miniatus" & assemblage == "Soft-hard"))

## Kelp ----
kelp <- kelp_raw %>% 
  rename(mpa_orig = affiliated_mpa) %>% 
  mutate(site_type = if_else(mpa_defacto_designation == "ref", "Reference", "MPA")) %>% 
  mutate(mpa_orig = recode(mpa_orig, "swami's smca" = "swamis smca")) %>% 
  mutate(kg_per_m3 = total_biomass_kg/(n_rep*120),
         count_per_m3 = total_count/(n_rep*120)) # 30x2x2m transects, so biomass per m^3

kelp2 <- kelp %>% 
  left_join(sp) %>% 
  group_by(sciname, target_status, assemblage, year, mpa_orig, site_type) %>% 
  summarize(kg_per_m3 = sum(kg_per_m3),
            count_per_m3 = sum(count_per_m3)) %>% 
  left_join(habitat_wide) %>% 
  clean_names() %>% ungroup()

# 7 missing sites:
test <- kelp %>% ungroup() %>% 
  distinct(site, bioregion, mpa_orig, site_type) %>% 
  filter(!site %in% habitat_raw$site)


kelp_effort_mpayear <- kelp_effort %>% 
  ungroup() %>% 
  mutate(site_type = if_else(mpa_defacto_designation == "ref", "Reference", "MPA")) %>% 
  distinct(year, bioregion, affiliated_mpa, site_type)

kelp_effort2 <- kelp_effort_mpayear %>% 
  mutate(n = 1) %>% 
  pivot_wider(names_from = year, values_from = n)

kelp_complete <- kelp_effort_mpayear %>% 
  expand_grid(sciname = unique(kelp2$sciname)) %>% 
  rename(mpa_orig = affiliated_mpa) %>% 
  left_join(kelp2) %>% 
  mutate_at(vars(kg_per_m3), ~ replace(., is.na(.), 0)) %>% 
  mutate_at(vars(count_per_m3), ~ replace(., is.na(.), 0)) %>% 
  left_join(sp) %>% 
  mutate(bioregion = factor(bioregion, levels = c("North", "Central", "South")))




# Plot -----------------------------------------------------------------------------

## biomass ~ habitat area ----

x_vars <- unique(habitat$habitat_depth)
x_vars <- c("hard_bottom_0_30m", "hard_bottom_30_100m", "hard_bottom_100_200m", "hard_bottom_200m",
            "soft_bottom_0_30m","soft_bottom_30_100m", "soft_bottom_100_200m", "soft_bottom_200m",
            "hard_bottom_biotic_0_30m", "hard_bottom_biotic_30_100m", "hard_bottom_biotic_100_200m",
            "soft_bottom_biotic_0_30m","soft_bottom_biotic_30_100m", "soft_bottom_biotic_100_200m",     
            "biotic_0_30m","biotic_30_100m")  


create_plot <- function(group, x_var, sciname, target_status, assemblage) {
  if (x_var %in% colnames(group) && !all(is.na(group[[x_var]]))) {
    if (sum(!is.na(group$kg_per_m3)) > 30) {
      return(ggplot(group, aes_string(x = x_var, y = "kg_per_m3", color = "site_type")) +
               geom_point() + 
               geom_smooth(method = "glm", formula = y ~ x) +
               labs(
                 title = paste("Species:", sciname,
                               "\nTarget status:", target_status,
                               "\nAssemblage:", assemblage),
                 x = x_var,
                 y = "kg per m^3 for each MPA per year",
                 color = "Site type") +
               theme_minimal() +
               theme(plot.title = element_text(size = 10)) +
               facet_wrap(~bioregion))
    }
  }
  return(ggplot() + theme_void()) # Return blank plot if conditions are not met
}

# Create a PDF file to save the plots
pdf(file.path(fig.dir, "species_plots_glm.pdf"), width = 24, height = 20)  # Adjust size to fit 18 plots

kelp_complete %>%
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



## species distributions ----
pdf(file.path(fig.dir, "species_distributions.pdf"), width = 6, height = 4)  # Adjust size to fit 18 plots

kelp_complete %>%
  group_by(sciname, target_status, assemblage) %>%
  group_split() %>%
  walk(function(group) {
    sciname <- unique(group$sciname)
    target_status <- unique(group$target_status)
    assemblage <- unique(group$assemblage)
    
    if (length(group$kg_per_m3[group$kg_per_m3 > 0]) > 30) {
      plot <- ggplot(group) +
        geom_density(aes(x = log(kg_per_m3+1)), fill = "skyblue2", alpha = 0.5) +
        labs(title = paste("Species:", sciname, "\nTarget Status:", target_status, "\nAssemblage:", assemblage),
             x = "Log+1 transformed kg per m^3 for mpa-year", 
             y = NULL,
             fill = NULL)+
        theme_minimal() +
        facet_wrap(~bioregion)
      print(plot)
    }
  })

dev.off()


## species distributions ----
pdf(file.path(fig.dir, "species_distributions_abund.pdf"), width = 6, height = 4)  # Adjust size to fit 18 plots

kelp_complete %>%
  group_by(sciname, target_status, assemblage) %>%
  group_split() %>%
  walk(function(group) {
    sciname <- unique(group$sciname)
    target_status <- unique(group$target_status)
    assemblage <- unique(group$assemblage)
    
    if (length(group$kg_per_m3[group$kg_per_m3 > 0]) > 30) {
      plot <- ggplot(group) +
        geom_density(aes(x = log(count_per_m3+1)), fill = "skyblue2", alpha = 0.5) +
        labs(title = paste("Species:", sciname, "\nTarget Status:", target_status, "\nAssemblage:", assemblage),
             x = "Log+1 transformed count per m^3 for mpa-year", 
             y = NULL,
             fill = NULL)+
        theme_minimal() +
        facet_wrap(~bioregion)
      print(plot)
    }
  })

dev.off()