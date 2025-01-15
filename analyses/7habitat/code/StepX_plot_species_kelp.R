# Plot species for kelp 
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
int.dir <- "~/ca-mpa/analyses/7habitat/intermediate_data"

# Read  ----------------------------------------------------------------------
# Read the habitat table
habitat_raw <- readRDS(file.path(int.dir, "habitat_buffers_by_site.Rds"))
rownames(habitat_raw) <- NULL

# Read the species table
#sp_raw <- readRDS(file.path(sp.dir, "species_lw_habitat.Rds")) 

# Read the kelp forest monitoring data
kelp_raw <- readRDS(file.path(ltm.dir, "biomass_site_year/kelp_biomass_site_year.Rds"))

kelp_effort <- readRDS(file.path(ltm.dir, "biomass_site_year/kelp_site_year_effort.Rds")) %>% 
  ungroup() 

mpas <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_general.Rds") %>% 
  mutate(implementation_year = as.numeric(format(implementation_date,'%Y')))

## Quick notes:
# 7 missing sites:
# test <- kelp_raw %>% ungroup() %>% 
#   distinct(site, bioregion, affiliated_mpa, site_type) %>% 
#   filter(!site %in% habitat_raw$site)

# Subset kelp sites to those > 5 visits after MPA established  ------------------
kelp_sites <- kelp_raw %>% 
  distinct(year, site, bioregion, affiliated_mpa, site_type) %>% 
  mutate(affiliated_mpa = recode(affiliated_mpa, 
                                 "blue cavern smca" = "blue cavern onshore smca",
                                 "swamis smca" = "swami's smca")) %>% 
  left_join(mpas %>% select(bioregion, affiliated_mpa, implementation_year)) %>% 
  mutate(before = if_else(year <= implementation_year, 1, 0),
         after = if_else(year > implementation_year, 1, 0)) %>% 
  group_by(site, bioregion, affiliated_mpa, site_type) %>% 
  summarize(n_before = sum(before),
            n_after = sum(after),
            n_total = sum(n_before, n_after)) %>% 
  filter(n_after >= 5)

kelp_effort <- kelp_effort %>% 
  filter(site %in% kelp_sites$site) %>% 
  mutate(site_type = if_else(mpa_defacto_designation == "ref", "Reference", "MPA"))

# Build  ----------------------------------------------------------------------
## Habitat -----

# Find the total and average for each habitat class, across all the sites affiliated with each MPA
habitat <- habitat_raw %>% 
  filter(habitat == "Kelp") %>% 
  filter(site %in% kelp_sites$site) %>% 
  group_by(mpa, affiliated_mpa = mpa_orig, site_type, buffer, habitat_depth) %>% 
  summarize(total_area_m2 = sum(area_m2, na.rm = T),
            mean_area_m2 = mean(area_m2, na.rm = T), .groups = 'drop') %>% 
  filter(!is.na(mpa)) 

habitat_wide <- habitat %>% 
  dplyr::select(mpa, affiliated_mpa, site_type, buffer, habitat_depth, mean_area_m2) %>% 
  pivot_wider(names_from = habitat_depth, values_from = mean_area_m2) 
  
habitat_100 <- habitat_wide %>% filter(buffer == 100)

## Kelp ----
kelp <- kelp_raw %>% 
  filter(site %in% kelp_sites$site) %>% 
  group_by(species_code, sciname, target_status, assemblage_new, year, bioregion, affiliated_mpa, site_type) %>% 
  summarize(kg_per_m2 = sum(kg_per_m2),
            count_per_m2 = sum(count_per_m2), .groups = 'drop') %>% 
  mutate(assemblage_new = case_when(sciname == "Paralabrax clathratus" ~ "Hard Bottom Biotic", 
                                    sciname == "Sebastes rastrelliger" ~ "Hard Bottom Biotic", 
                                    sciname == "Sebastes miniatus" ~ "Hard Bottom", 
                                    T~assemblage_new)) 
  
kelp_by_assemblage <- kelp %>% 
  group_by(target_status, assemblage_new, year, bioregion, affiliated_mpa, site_type) %>% 
  summarize(kg_per_m2 = sum(kg_per_m2),
            count_per_m2 = sum(count_per_m2), .groups = 'drop') 

kelp_effort2 <- kelp_effort %>% ungroup() %>% 
  group_by(year, bioregion, affiliated_mpa, site_type) %>% 
  summarize(n = n()) %>% 
  pivot_wider(names_from = year, values_from = n) %>% 
  mutate(n_years = rowSums(across(`2000`:`2023`), na.rm = T)) 

kelp_select <- kelp %>% 
  filter(sciname %in% c("Semicossyphus pulcher", #sheephead
                        "Caulolatilus princeps", # ocean whitefish
                        "Sebastes atrovirens", # kelp rockfish
                        "Ophiodon elongatus", # lingcod
                        "Sebastes entomelas") | # widow rockfish
           species_code %in% c("OYT", #olive yellowtail
                               "PCLA" # kelp bass
                               )) 

test <- kelp_select %>% distinct(species_code, sciname)

# Plot those species

kelp_complete <- kelp_effort %>% 
  expand_grid(species_code = unique(kelp$species_code)) %>% 
  left_join(kelp %>% dplyr::select(-target_status, -assemblage_new), by = c("year", "bioregion", "affiliated_mpa", "site_type", "species_code")) %>% 
  left_join(kelp %>% distinct(species_code, sciname, target_status, assemblage_new) %>% filter(!is.na(assemblage_new)), by = c("species_code", "sciname")) %>% 
  mutate_at(vars(kg_per_m2), ~ replace(., is.na(.), 0)) %>% 
  mutate_at(vars(count_per_m2), ~ replace(., is.na(.), 0)) %>% 
  mutate(bioregion = factor(bioregion, levels = c("North", "Central", "South"))) %>% 
  left_join(habitat_100) 


assemblage_complete <- kelp_effort %>% 
  expand_grid(assemblage_new = unique(kelp$assemblage_new)) %>% 
  filter(!is.na(assemblage_new)) %>% 
  left_join(kelp_by_assemblage) %>% 
  mutate_at(vars(kg_per_m2), ~ replace(., is.na(.), 0)) %>% 
  mutate_at(vars(count_per_m2), ~ replace(., is.na(.), 0)) %>% 
  mutate(bioregion = factor(bioregion, levels = c("North", "Central", "South"))) 

pdf(file.path(fig.dir, "assemblage.pdf"))

assemblage_complete %>%
  group_by(assemblage_new, target_status) %>%
  group_split() %>%
  walk(function(group) {
    assemblage_new <- unique(group$assemblage_new)
    target_status <- unique(group$target_status)
    
    plot <- ggplot(group) +
      geom_point(aes(x = year, y = kg_per_m2, color = site_type)) +
      geom_smooth(aes(x = year, y = kg_per_m2, color = site_type)) +
      labs(title = paste("Assemblage:", assemblage_new, "\nTarget Status:", target_status),
           x = "year", 
           y = "kg per m^2 for mpa-year")+
      theme_minimal() +
      facet_wrap(~bioregion, nrow = 1)
    print(plot)
  })

dev.off()

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
    if (sum(group$kg_per_m2 != 0, na.rm = TRUE) > 30) {
      return(ggplot(group, aes_string(x = x_var, y = "kg_per_m2", color = "site_type")) +
               geom_point() + 
               geom_smooth(method = "glm", formula = y ~ x) +
               labs(
                 title = paste("Species:", sciname,
                               "\nTarget status:", target_status,
                               "\nAssemblage:", assemblage),
                 x = x_var,
                 y = "kg per m^2 for each MPA per year",
                 color = "Site type") +
               theme_minimal() +
               theme(plot.title = element_text(size = 10)) +
               facet_wrap(~bioregion, nrow = 1))
    }
  }
  return(ggplot() + theme_void()) # Return blank plot if conditions are not met
}

# Create a PDF file to save the plots
pdf(file.path(fig.dir, "species_plots_glm.pdf"), width = 24, height = 20)  # Adjust size to fit 18 plots

kelp_complete %>% 
  filter(species_code %in% kelp_select$species_code) %>% 
  rename(assemblage = assemblage_new) %>% 
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
  rename(assemblage = assemblage_new) %>% 
  filter(species_code %in% kelp_select$species_code) %>% 
  group_by(sciname, target_status, assemblage) %>%
  group_split() %>%
  walk(function(group) {
    sciname <- unique(group$sciname)
    target_status <- unique(group$target_status)
    assemblage <- unique(group$assemblage)
    
    if (length(group$kg_per_m2[group$kg_per_m2 > 0]) > 0) {
      plot <- ggplot(group) +
        geom_density(aes(x = log(kg_per_m2+1)), fill = "skyblue2", alpha = 0.5) +
        labs(title = paste("Species:", sciname, "\nTarget Status:", target_status, "\nAssemblage:", assemblage),
             x = "Log+1 transformed kg per m^2 for mpa-year", 
             y = NULL,
             fill = NULL)+
        theme_minimal() +
        facet_wrap(~bioregion, nrow = 1, scales = "free")
      print(plot)
    }
  })

dev.off()


## species distributions ----
pdf(file.path(fig.dir, "species_distributions_abund.pdf"), width = 6, height = 4)  # Adjust size to fit 18 plots

kelp_complete %>%
  rename(assemblage = assemblage_new) %>% 
  filter(species_code %in% kelp_select$species_code) %>% 
  group_by(sciname, target_status, assemblage) %>%
  group_split() %>%
  walk(function(group) {
    sciname <- unique(group$sciname)
    target_status <- unique(group$target_status)
    assemblage <- unique(group$assemblage)
    
    if (length(group$kg_per_m2[group$kg_per_m2 > 0]) > 0) {
      plot <- ggplot(group) +
        geom_density(aes(x = log(count_per_m2+1)), fill = "skyblue2", alpha = 0.5) +
        labs(title = paste("Species:", sciname, "\nTarget Status:", target_status, "\nAssemblage:", assemblage),
             x = "Log+1 transformed count per m^3 for mpa-year", 
             y = NULL,
             fill = NULL)+
        theme_minimal() +
        facet_wrap(~bioregion, nrow = 1, scales = "free")
      print(plot)
    }
  })

dev.off()

# habitat coverage

pdf(file.path(fig.dir, "habitat_sites.pdf"))

habitat %>% 
  select(-total_area_m2) %>% 
  pivot_wider(names_from = site_type, values_from = mean_area_m2) %>% 
  group_by(buffer) %>% 
  group_split() %>% 
  walk(function(group) {
    buffer <- unique(group$buffer)
    
    # Calculate the distance from the 1:1 line
    group <- group %>% 
      mutate(distance_from_line = abs(Reference - MPA) / sqrt(2))
    
    plot <- ggplot(group) +
      geom_point(aes(x = Reference, y = MPA, color = distance_from_line)) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
      facet_wrap(~ habitat_depth) +
      scale_color_gradient(low = "lightblue", high = "darkblue") +
      labs(title = paste("Buffer", buffer),
           x = "Mean area (m2) across all reference sites for a given MPA",
           y = "Mean area (m2) across all MPA sites for a given MPA",
           color = "Distance\nfrom 1:1 line") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 10),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 6),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6),
        strip.text = element_text(size = 8)
      )
    
    print(plot)
  })

dev.off()

test <- habitat %>% 
  select(-total_area_m2) %>% 
  pivot_wider(names_from = site_type, values_from = mean_area_m2) 
