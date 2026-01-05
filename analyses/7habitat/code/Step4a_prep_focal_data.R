# Prep focal dataset for models
# Cori Lopazanski
# March 2025

# This splits the data prep and the modeling piece into two parts

prep_focal_data <- function(type, focal_group, drop_outliers, biomass_variable, data, regions){
  
  print(paste("Starting:", focal_group))
  
  ## 1. Process Data -----------------------------------------------------------------------------
  
  # Filter to the groups interest, convert RE to factors
  if (type == "species") {
    data1 <- data %>%
      filter(species_code == focal_group) %>% 
      filter(region4 %in% regions) %>% 
      rename(biomass = biomass_variable)
    
  } else if (type == "target_status" & focal_group %in% c("targeted", "nontargeted")) {
    data1 <- data %>%
      group_by(year, site, site_type, bioregion, region4, affiliated_mpa, size_km2, age_at_survey,
               target_status, across(matches("^hard|soft|depth|kelp|aquatic|tri"))) %>%
      summarize(biomass = sum(!!sym(biomass_variable), na.rm = T), .groups = 'drop') %>%
      filter(target_status == str_to_sentence(focal_group))
  } else if (type == "target_status" & focal_group == "all"){
    data1 <- data %>%
      group_by(year, site, site_type, bioregion, region4, affiliated_mpa, size_km2, age_at_survey,
               across(matches("^hard|soft|depth|kelp|aquatic"))) %>%
      summarize(biomass = sum(!!sym(biomass_variable), na.rm = T), .groups = 'drop')
  } else if (type == "targeted_vert"){
    data1 <- data %>%
      group_by(year, site, site_type, bioregion, region4, affiliated_mpa, size_km2, age_at_survey,
               target_status, vertical_zonation, across(matches("^hard|soft|depth|kelp|aquatic|tri"))) %>%
      summarize(biomass = sum(!!sym(biomass_variable), na.rm = T), .groups = 'drop') %>%
      filter(target_status == "Targeted") %>% 
      filter(vertical_zonation %in% focal_group)
    
  } else {
    stop("Error in focal group or type.")
  }
  
  # Scale the static variables at the site-level (e.g. don't weight based on obs. frequency)
  site_static <- data1 %>% 
    distinct(site, across(all_of(grep("^hard|soft|depth|aquatic|tri", names(.), value = TRUE)))) %>%
    mutate_at(vars(grep("^hard|soft|depth|aquatic|tri", names(.), value = TRUE)), scale)
  
  if (drop_outliers == "yes") {
    # Remove sites with extreme values in static vars (depth and hard bottom)
    extreme_site <- site_static %>% 
      pivot_longer(cols = depth_cv_100:hard_bottom_500, names_to = "variable", values_to = "value") %>% 
      filter(!str_detect(variable, "depth_sd")) %>% 
      filter(!str_detect(variable, "depth_cv_25")) %>% 
      filter(!between(value, -3.5, 3.5)) %>% 
      pivot_wider(names_from = variable, values_from = value)
    
    # Check balance of remaining sites (ensure still MPA/Ref pairs)
    extreme_site_balance <- data1 %>% 
      filter(!site %in% extreme_site$site) %>% 
      distinct(site, site_type, affiliated_mpa, year) %>% 
      group_by(affiliated_mpa, site_type) %>% 
      summarize(n_site_year = n(), .groups = 'drop') %>% 
      pivot_wider(names_from = site_type, values_from = n_site_year) %>% 
      filter(is.na(MPA) | is.na(Reference))
    
    data2 <- data1 %>%
      filter(!site %in% extreme_site$site) %>%
      filter(!affiliated_mpa %in% extreme_site_balance$affiliated_mpa)
    
    print(paste("  Number of extreme sites dropped:", length(extreme_site$site)))
    print(paste("  Number of extreme MPAs dropped:", length(extreme_site_balance$affiliated_mpa)))
    
  } else {
    data2 <- data1
  }
  
  # Save data2 to environment for inspecting
  assign("data2", data2, envir = .GlobalEnv)
  
  data3 <- data2 %>%
    mutate(year = as.factor(year),
           bioregion = as.factor(bioregion),
           region4 = as.factor(region4),
           affiliated_mpa = as.factor(affiliated_mpa)) %>% 
    # Drop un-scaled static variables
    dplyr::select(!all_of(c(grep("^hard|soft|depth|aquatic|tri", names(.), value = TRUE)))) %>% 
    # Join the scaled static variables
    left_join(site_static, by = "site") %>% 
    # Scale age
    mutate_at(vars(grep("^age|size", names(.), value = TRUE)), scale) %>% 
    # Scale the kelp within each year (so it's relative to the annual average instead of across all years)
    group_by(year) %>%
    mutate_at(vars(grep("^kelp", names(.), value = TRUE)), scale) %>% ungroup() %>% 
    mutate(site = as.factor(site))
  
  # Save final output for the model
  data_sp <- data3
  rm(data1, data2, data3)
  
  # Add a small constant, defined as the minimum value for that species
  const <- if_else(min(data_sp$biomass) > 0, 0, min(data_sp$biomass[data_sp$biomass > 0], na.rm = TRUE))
  data_sp <- data_sp %>% mutate(log_c_biomass = log(biomass + const))
  
  print("  Data prep complete.")
  return(data_sp)
  
  
}
