# Cori Lopazanski
# August 2024

# About ------------------------------------------------------------------------------------

# Merge the species, habitat, and monitoring tables into one df for models

# Setup -------------------------------------------------------------------------------------------------------------------------
library(tidyverse) 
library(gt)
library(purrr)

rm(list = ls())

# Directories
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024/2025"
sp.dir <- "/home/shares/ca-mpa/data/sync-data/species_traits/processed"
int.dir <- "~/ca-mpa/analyses/7habitat/intermediate_data"
kw.dir <- "/home/shares/ca-mpa/data/sync-data/kelpwatch/2024/processed"

# Read Data --------------------------------------------------------------------------------------------------------------------
# Area of each habitat by buffer (across all depths)
habitat_combined <- readRDS(file.path(int.dir, "habitat_buffers_by_site_combined_v3.Rds")) 

# Annual kelp canopy cover
habitat_kelp <- readRDS(file.path(kw.dir, "kelp_site_buffers.Rds")) %>% 
  dplyr::select(-habitat, -site_id) %>% 
  distinct() %>% 
  mutate(year = as.numeric(year))

# Add plotting details
fig.dir <- "~/ca-mpa/analyses/7habitat/figures"

my_theme <- theme_minimal(base_family = "Arial") + 
  theme(plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 8),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.margin = margin(t = 0, unit='cm'),
        plot.caption = element_text(size = 8),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 8, face = "bold"),
        panel.background = element_rect(fill = "white", color = NA),  
        plot.background = element_rect(fill = "white", color = NA),
        legend.position = "top",
        panel.spacing = unit(1, "lines"),
        legend.key.size = unit(1, "lines"))

mpa_colors <- c("Reference" = "#6d55aa", "MPA" = "#c42119")

# Helper function that formats the final details for the distribution graphs
format_for_graph <- function(df){
  df %>% 
    mutate(scale = as.numeric(str_extract(habitat_var, "\\d+"))) %>% 
    mutate(habitat = str_remove(habitat_var, "_\\d+")) %>% 
    arrange(desc(habitat), scale) %>% 
    mutate(habitat_var = factor(habitat_var, levels = unique(habitat_var))) %>% 
    mutate(habitat_label = paste0(str_replace_all(habitat_var, "_", " ") %>% str_to_sentence() %>% str_replace_all("cv", "CV") %>% str_replace_all("Tri", "TRI"), "m")) %>% 
    mutate(habitat_label = factor(habitat_label, levels = unique(habitat_label))) %>% 
    dplyr::bind_rows(tibble(habitat_var = c("depth_cv_25", "tri_mean_25", "slope_mean_25", "slope_sd_25",
                                            "depth_cv_50", "tri_mean_50", "slope_mean_50", "slope_sd_50"),
                            scale       = c(25, 25, 25, 25, 50, 50, 50, 50),
                            value       = NA_real_,
                            site_type   = NA_character_)) %>% 
    mutate(habitat_cat = factor(case_when(str_detect(habitat_var, "tri") ~ "TRI",
                                          str_detect(habitat_var, "hard") ~ "Hard bottom",
                                          str_detect(habitat_var, "soft") ~ "Soft bottom",
                                          str_detect(habitat_var, "depth_mean") ~ "Depth mean",
                                          str_detect(habitat_var, "depth_cv") ~ "Depth CV",
                                          str_detect(habitat_var, "slope_mean") ~ "Slope mean",
                                          str_detect(habitat_var, "slope_sd") ~ "Slope sd",
                                          str_detect(habitat_var, "kelp") ~ "Kelp extent (mean)",
                                          str_detect(habitat_var, "aquatic") ~ "Max biotic extent",
                                          T~habitat_var), 
                                levels = c("Hard bottom", "Soft bottom", "Depth mean", "Depth CV", "Kelp extent (mean)", "Max biotic extent", "TRI", "Slope mean", "Slope sd")))
}


# Build function that will iteratively remove sites that exceed the threshold -------
iterative_trim <- function(dat_long, threshold = 30) {
  
  # Calculate the range for MPA and Reference, and flag whether they are within threshold
  compute_ranges <- function(df) {
    df %>%
      group_by(site_type, habitat_var) %>%
      summarise(maxval = max(value, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = site_type, values_from = maxval) %>%
      mutate(allowed = pmin(MPA, Reference) * (1 + threshold/100)) %>%
      select(habitat_var, allowed, MPA, Reference)
  }
  
  # Identify the violations given the current allowed ranges
  check_violations <- function(df, ranges) {
    df %>%
      left_join(ranges, by = "habitat_var") %>%
      mutate(excess = value - allowed,
             pct_exceed = 100 * (value - allowed) / ((value + allowed)/2)) %>%
      filter(excess > 0)
  }
  
  # Check whether removing the most extreme site breaks MPA-REF pairings
  breaks_balance <- function(df, site_to_remove) {
    tmp <- df %>% filter(site != site_to_remove)
    tmp %>%
      group_by(affiliated_mpa, site_type) %>%
      summarise(n = n_distinct(site), .groups = "drop") %>%
      count(affiliated_mpa) %>%
      filter(n < 2) %>%
      nrow() > 0
  }
  
  # Objects to store removed rows and current working df
  removed <- tibble()
  current <- dat_long
  
  repeat {
    
    # Calculate allowed ranges with remaining sites
    ranges <- compute_ranges(current)
    
    # Identify all violating rows
    viol <- check_violations(current, ranges)
    if (nrow(viol) == 0) break
    
    # Identify most extreme remaining site
    offender <- viol %>%
      group_by(site, affiliated_mpa) %>%
      summarise(total_excess = sum(excess), .groups = "drop") %>%
      arrange(desc(total_excess)) %>%
      slice(1)
    
    # Check whether removing site breaks MPA-Ref balance
    balance_flag <- breaks_balance(current, offender$site)
    
    # Define the sites to remove this iteration
    if (balance_flag) {
      removal_sites <- current %>% filter(affiliated_mpa == offender$affiliated_mpa) %>% pull(site) %>% unique()
    } else {
      removal_sites <- offender$site
    }
    
    # Violation rows: only habitat variables that exceeded the allowed range
    det_viol <- viol %>%
      filter(site %in% removal_sites) %>%
      select(site, site_type, affiliated_mpa, habitat_var,
             value, allowed, pct_exceed) %>%
      mutate(breaks_balance = balance_flag)
    
    # Balance-breaking rows: sites that were removed only because the MPA pairing failed
    det_balance <- current %>%
      filter(site %in% removal_sites,
             !site %in% unique(det_viol$site)) %>%
      distinct(site, site_type, affiliated_mpa) %>%
      mutate(habitat_var = NA_character_,
             value = NA_real_,
             allowed = NA_real_,
             pct_exceed = NA_real_,
             breaks_balance = TRUE)
    
    # Combine both
    det <- bind_rows(det_viol, det_balance)
    
    removed <- bind_rows(removed, det)
    
    current <- current %>% filter(!(site %in% removal_sites))
  }

  
  list(
    removed_details = removed,
    remaining = current
  )
}

format_table <- function(df){
  # Format removal table
  df %>% 
    mutate(habitat_var = str_replace_all(habitat_var, "_", " ") %>% 
             str_to_sentence() %>%   
             str_replace_all("cv", "CV") %>%  
             str_replace_all("Tri", "TRI")) %>% 
    mutate(affiliated_mpa = str_to_title(affiliated_mpa) %>% 
             str_replace_all("Smr", "SMR") %>% 
             str_replace_all("Smca", "SMCA")) %>% 
    select(site, site_type, affiliated_mpa, habitat_var, value, pct_exceed, breaks_balance) %>% 
    mutate(pct_exceed = pct_exceed + 30) %>% 
    mutate(breaks_balance = case_when(!breaks_balance ~ "No",
                                      breaks_balance & !is.na(value) ~ "Yes",
                                      breaks_balance & is.na(value) ~ "Removed for balance")) %>% 
    gt() %>% 
    cols_label(site = "Site",
               site_type = "Site Type",
               affiliated_mpa = "MPA",
               habitat_var = "Variable",
               value = "Site Value",
               pct_exceed = "% Difference",
               breaks_balance = "Breaks MPA/Ref Balance") %>% 
    sub_missing(everything(), missing_text = "") %>% 
    fmt_number(columns = c(value, pct_exceed), decimals = 2)
}


# Kelp -------------------------------------------------------------------------------------------------------------------

kelp_raw <- readRDS(file.path(ltm.dir, "kelp_biomass_subset.Rds")) 

kelp <- kelp_raw %>%
  left_join(habitat_combined, by = c("site", "site_type")) %>% 
  left_join(habitat_kelp, by = c("year", "site", "site_type")) 

kelp_sites <- kelp %>% 
  dplyr::select(habitat, site, site_type, affiliated_mpa, all_of(names(habitat_combined))) %>% distinct() %>% 
  pivot_longer(cols = hard_bottom_25:slope_sd_500, names_to = "habitat_var", values_to = "value") %>% 
  filter(!str_detect(habitat_var, "aquatic|soft|tri|slope") & !is.na(site_type)) %>% 
  select(site, site_type, affiliated_mpa, habitat_var, value) 

kelp_trim <- iterative_trim(kelp_sites, threshold = 30)

# Inspect output table
kelp_trim$removed_details %>% format_table()

# Create subset and plot distributions
kelp_subset <- kelp_sites %>% 
  filter(site %in% kelp_trim$remaining$site) %>% 
  format_for_graph() %>% 
  filter(!str_detect(habitat_var, "aquatic|soft|tri|slope"))

ggplot(data = kelp_subset) +
  geom_density(aes(x = value, color = site_type, fill = site_type), alpha = 0.3) + 
  geom_blank() +
  scale_fill_manual(values = mpa_colors)+
  scale_color_manual(values = mpa_colors)+
  labs(x = "Value of habitat characteristic", y = "Density", fill = NULL, color = NULL)+
  my_theme +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ habitat_cat + scale,
    nrow = length(unique(kelp_subset$habitat_cat)),
    ncol = length(unique(kelp_subset$scale)),
    scales = "free")

ggsave(file.path(fig.dir, "si-fig1-kelp.png"), 
       width = 9, height = 6, dpi = 600, units = "in")


# Rock (CCFRP) ----------------------------------------------------------------------------------------------
rock_raw <- readRDS(file.path(ltm.dir, "rock_biomass_subset.Rds")) 

rock <- rock_raw %>% 
  left_join(habitat_combined) %>% 
  left_join(habitat_kelp, by = c("year", "site", "site_type"))

rock_sites <- rock %>% 
  dplyr::select(habitat, site, site_type, affiliated_mpa, all_of(names(habitat_combined))) %>% distinct() %>% 
  pivot_longer(cols = hard_bottom_25:slope_sd_500, names_to = "habitat_var", values_to = "value") %>% 
  filter(!str_detect(habitat_var, "aquatic|soft|tri|slope") & !is.na(site_type)) %>% 
  select(site, site_type, affiliated_mpa, habitat_var, value) 

rock_trim <- iterative_trim(rock_sites, threshold = 30)

# Inspect
rock_trim$removed_details %>% format_table()

# Create subset for plotting
rock_subset <- rock_sites %>% 
  filter(site %in% rock_trim$remaining$site) %>% 
  format_for_graph() %>% 
  filter(!str_detect(habitat_var, "aquatic|soft|tri|slope"))

ggplot(data = rock_subset) +
  geom_density(aes(x = value, color = site_type, fill = site_type), alpha = 0.3) + 
  geom_blank() +
  scale_fill_manual(values = mpa_colors)+
  scale_color_manual(values = mpa_colors)+
  labs(x = "Value of habitat characteristic", y = "Density", fill = NULL, color = NULL)+
  my_theme +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ habitat_cat + scale,
             nrow = length(unique(rock_subset$habitat_cat)),
             ncol = length(unique(rock_subset$scale)),
             scales = "free")

ggsave(file.path(fig.dir, "si-fig2-rock.png"), 
       width = 9, height = 7, dpi = 600, units = "in")


# Surf zone (seines) ----------------------------------------------------------------------------------------------

surf_raw <- readRDS(file.path(ltm.dir, "surf_biomass_subset.Rds")) 

surf <- surf_raw %>% 
  left_join(habitat_combined) %>% 
  left_join(habitat_kelp, by = c("year", "site", "site_type")) 

surf_sites <- surf %>% 
  dplyr::select(habitat, site, site_type, affiliated_mpa, all_of(names(habitat_combined))) %>% distinct() %>% 
  pivot_longer(cols = hard_bottom_25:slope_sd_500, names_to = "habitat_var", values_to = "value") %>% 
  filter(!str_detect(habitat_var, "tri|slope") & !is.na(site_type)) %>% 
  select(site, site_type, affiliated_mpa, habitat_var, value) 

# surf_trim <- iterative_trim(surf_sites, threshold = 30) # will break; removes all sites...

# Calculate the range for MPA and Reference, and flag whether they are within threshold
surf_ranges <- surf_sites %>%
    group_by(site_type, habitat_var) %>%
    summarise(maxval = max(value, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = site_type, values_from = maxval) %>%
    mutate(allowed = pmin(MPA, Reference) * (1 + 30/100)) %>%
    select(habitat_var, allowed, MPA, Reference) 

surf_violations <- surf_sites %>% 
  left_join(surf_ranges, by = "habitat_var") %>%
  mutate(excess = value - allowed,
         pct_exceed = 100 * (value - allowed) / ((value + allowed)/2),
         breaks_balance = NA) %>%
  filter(excess > 0) 

surf_violations %>% format_table()

surf_subset <- surf_sites %>% 
  format_for_graph() %>% 
  filter(!str_detect(habitat_var, "tri|slope"))

ggplot(data = surf_subset) +
  geom_density(aes(x = value, color = site_type, fill = site_type), alpha = 0.3) + 
  geom_blank() +
  scale_fill_manual(values = mpa_colors)+
  scale_color_manual(values = mpa_colors)+
  labs(x = "Value of habitat characteristic", y = "Density", fill = NULL, color = NULL)+
  my_theme +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ habitat_cat + scale,
             nrow = length(unique(surf_subset$habitat_cat)),
             ncol = length(unique(surf_subset$scale)),
             scales = "free")

ggsave(file.path(fig.dir, "si-fig3-surf.png"), 
       width = 9, height = 9, dpi = 600, units = "in")

# Save final subsets with sites removed
kelp2 <- kelp %>% filter(site %in% kelp_trim$remaining$site)
rock2 <- rock %>% filter(site %in% rock_trim$remaining$site)
surf2 <- surf # do not remove any

# Export 
saveRDS(kelp2, file.path(ltm.dir, "combine_tables/kelp_full.Rds")) 
saveRDS(surf2, file.path(ltm.dir, "combine_tables/surf_full.Rds")) 
saveRDS(rock2, file.path(ltm.dir, "combine_tables/ccfrp_full.Rds")) 


# Build table for sites that are removed from kelp and surf zone

kelp_removed  <- kelp_trim$removed_details %>% mutate(habitat = "Kelp forest")
surf_removed  <- surf_violations %>% mutate(habitat = "Surf zone (none removed)") %>% select(!c(MPA, Reference, excess))
rock_removed  <- rock_trim$removed_details  %>% mutate(habitat = "Shallow reef")

all_removed <- bind_rows(rock_removed, kelp_removed, surf_removed)

table <- all_removed %>% 
  mutate(habitat_var = str_replace_all(habitat_var, "_", " ") %>% 
           str_to_sentence() %>%   
           str_replace_all("cv", "CV") %>%  
           str_replace_all("Tri", "TRI")) %>% 
  mutate(affiliated_mpa = str_to_title(affiliated_mpa) %>% 
           str_replace_all("Smr", "SMR") %>% 
           str_replace_all("Smca", "SMCA")) %>% 
  select(habitat, site, site_type, affiliated_mpa, habitat_var, value, pct_exceed, breaks_balance) %>% 
  mutate(pct_exceed = pct_exceed + 30) %>% 
  mutate(breaks_balance = case_when(!breaks_balance ~ "No",
                                    breaks_balance & !is.na(value) ~ "Yes",
                                    breaks_balance & is.na(value) ~ "Removed for balance")) %>% 
  gt(groupname_col = "habitat") %>% 
  cols_label(site = "Site",
             site_type = "Site Type",
             affiliated_mpa = "MPA",
             habitat_var = "Variable",
             value = "Site Value",
             pct_exceed = "% Difference",
             breaks_balance = "Breaks MPA/Ref Balance") %>% 
  sub_missing(everything(), missing_text = "") %>% 
  fmt_number(columns = c(value, pct_exceed), decimals = 2) %>% 
  tab_options(table.font.names = "Times New Roman",
              table.font.size = px(10),
              data_row.padding = px(4),
              row_group.padding = px(6),
              heading.align = "left",
              table.width = pct(60)) %>% 
  tab_style( style = cell_text(font = "Times New Roman", size = px(12)),
             locations = cells_body(columns = everything())) %>% 
  tab_style(style = cell_text(font = "Times New Roman", size = px(12), weight = "bold"),
            locations = cells_column_labels(columns = everything())) %>% 
  tab_style(style = cell_text(font = "Times New Roman", size = px(12), weight = "bold"),
            locations = cells_row_groups()) %>% 
  cols_align( align = "center", columns = 3:8)

gtsave(table, file.path(fig.dir, "si-table1-removals.png"),   vwidth = 1300, vheight = 1000)




