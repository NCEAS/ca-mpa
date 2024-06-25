#title: "MPA-level meta analysis results table"
#author: "Joshua G. Smith"
#date: "8/1/2023"

rm(list=ls())

#required packages
librarian::shelf(ggplot2, tidyverse, purrr)

#set directories
data_path <- "/home/shares/ca-mpa/data/sync-data/"
biomass_dat <-  paste0(data_path,"monitoring/processed_data/biomass_processed")
fig_dir <- here::here("analyses","1performance_eco","figures")
tab_dir <- here::here("analyses","1performance_eco","tables")
dat_path <- here::here("analyses","1performance_eco","output")

#read data
mpa_meta_results <- readRDS(file.path(dat_path, "mpa_level_meta_results.Rds")) 
habitat_region_results <- readRDS(file.path(dat_path, "habitat_region_meta_results.Rds")) 
habitat_target_results <- readRDS(file.path(dat_path, "habitat_target_meta_results.Rds")) 
region_results <- readRDS(file.path(dat_path, "region_meta_results.Rds")) 
state_results <- readRDS(file.path(dat_path, "state_meta_results.Rds")) 


################################################################################
#extract relevant model output for mpa-level analyses

# Apply the function to each element in 'meta_results'
mpa_meta_out <- mpa_meta_results %>%
  #get the habitats included in the analysis for each MPA
  mutate(tau2 = map_dbl(meta_result, ~ pluck(.x, "tau2")),
         Q = map_dbl(meta_result, ~ pluck(.x, "QE")),
         habitats = map(seq_along(meta_result), ~ meta_result[[.x]][["data"]][["habitat"]]),
         #convert habitats to string
         habitats = map_chr(habitats, ~ str_c(.x, collapse = ", ")),
         #extract years
         years = map(seq_along(meta_result), ~ meta_result[[.x]][["data"]][["year"]]),
         #convert habitats to string
         years = map_chr(years, ~ str_c(.x, collapse = ", "))
         )%>%
  arrange(state_region, mpa, target_status)%>%
  #numerate each unique MPA
  mutate(MPA_number = group_indices(., mpa))%>%
  #drop columns
  dplyr::select(-meta_result, -n_habitat, -zval)%>%
  dplyr::select(MPA_number,state_region,mpa, target_status, everything())%>%
  mutate(estimate = round(estimate,3),
         se = round(se,3),
         pval = round(pval,3),
         ci.lb = round(ci.lb,3),
         ci.ub = round(ci.ub,3),
        tau2 = round(tau2,3),
        Q = round(Q, 3))%>%
  #reformat ecosystem and year
  mutate(Ecosystem_Year = map2(habitats, years, function(ecosystems, years) {
    ecosystems <- str_split(ecosystems, ", ")[[1]]
    years <- str_split(years, ", ")[[1]]
    
    formatted <- paste(ecosystems, " (", years, ")", sep = "")
    return(paste(formatted, collapse = ", "))
  }))%>%
  dplyr::select(-habitats, -years)%>%
  rename("Row" = MPA_number,
         "Region" = state_region,
         "MPA name" = mpa,
         "Target status" = target_status,
         "Effect size" = estimate,
         "Standard error" = se,
         "P-value" = pval,
         "95% lower" = ci.lb,
         "95% upper" = ci.ub,
         "Tau-2" = tau2,
         "Ecosystem (latest year)" = Ecosystem_Year
         ) %>%
  mutate(Region = str_remove(Region, "Coast"),
         `Ecosystem (latest year)` = as.character(`Ecosystem (latest year)`)) 


write.csv(mpa_meta_out, file = file.path(tab_dir,"TableS10_mpa_meta_table.csv"),row.names = FALSE)



################################################################################
#extract relevant model output for habitat x region x target_status

# Apply the function to each element in 'meta_results'
hab_region_meta_out <- habitat_region_results %>%
  #get the habitats included in the analysis for each MPA
  mutate(
         Q = map_dbl(meta_result, ~ pluck(.x, "QE")))%>%
  arrange(habitat, state_region, target_status)%>%
  #drop columns
  dplyr::select(-meta_result,  -zval)%>%
  mutate(take = ifelse(mpa_defacto_class == "smr","No-take","Partial-take"),
         mpa_defacto_class = toupper(mpa_defacto_class))%>%
  dplyr::select(habitat,state_region, mpa_defacto_class, take, target_status, everything())%>%
  mutate(estimate = round(estimate,3),
         se = round(se,3),
         pval = round(pval,3),
         ci.lb = round(ci.lb,3),
         ci.ub = round(ci.ub,3),
         tau2 = round(tau2,3),
         Q = round(Q, 3))%>%
  rename("Ecosystem" = habitat,
         "Region" = state_region,
         "Target status" = target_status,
         "Effect size" = estimate,
         "Standard error" = se,
         "P-value" = pval,
         "95% lower" = ci.lb,
         "95% upper" = ci.ub,
         "Tau-2" = tau2,
         "No. MPAs" = n_mpas,
         "Allowed take" = take,
         "MPA type" = mpa_defacto_class
  ) %>%
  mutate(Region = str_remove(Region, "Coast"),
         Ecosystem = factor(Ecosystem, levels = c("Surf zone","Kelp forest","Shallow reef","Deep reef"))) 


write.csv(hab_region_meta_out, file = file.path(tab_dir,"TableS9_habitat_region_meta_table.csv"),row.names = FALSE)


################################################################################
#extract relevant model output for habitat x target_status


# Apply the function to each element in 'meta_results'
hab_target_meta_out <- habitat_target_results %>%
  #get the habitats included in the analysis for each MPA
  mutate(
    Q = map_dbl(meta_result, ~ pluck(.x, "QE")))%>%
  arrange(habitat, target_status)%>%
  #drop columns
  dplyr::select(-meta_result,  -zval, -state_region)%>%
  mutate(take = ifelse(mpa_defacto_class == "smr","No-take","Partial-take"))%>%
  dplyr::select(habitat, mpa_defacto_class, take, target_status, everything())%>%
  mutate(mpa_defacto_class = toupper(mpa_defacto_class),
         estimate = round(estimate,3),
         se = round(se,3),
         pval = round(pval,3),
         ci.lb = round(ci.lb,3),
         ci.ub = round(ci.ub,3),
         tau2 = round(tau2,3),
         Q = round(Q, 3))%>%
  rename("Ecosystem" = habitat,
         "Target status" = target_status,
         "Effect size" = estimate,
         "Standard error" = se,
         "P-value" = pval,
         "95% lower" = ci.lb,
         "95% upper" = ci.ub,
         "Tau-2" = tau2,
         "Allowed take" = take,
         "MPA type" = mpa_defacto_class,
         "No. MPAs" = n_mpas
  ) %>%
 mutate(Ecosystem = factor(Ecosystem, levels = c("Surf zone","Kelp forest","Shallow reef","Deep reef"))) 


write.csv(hab_target_meta_out, file = file.path(tab_dir,"TableS8_hab_target_meta_table.csv"),row.names = FALSE)


################################################################################
#extract relevant model output for region x target_status


# Apply the function to each element in 'meta_results'
region_meta_out <- region_results %>%
  #get the habitats included in the analysis for each MPA
  mutate(
    Q = map_dbl(meta_result, ~ pluck(.x, "QE")))%>%
  arrange(state_region, target_status)%>%
  #drop columns
  dplyr::select(-meta_result,  -zval, -habitat)%>%
  mutate(take = ifelse(mpa_defacto_class == "smr","No-take","Partial-take"),
         mpa_defacto_class = toupper(mpa_defacto_class))%>%
  dplyr::select(state_region, mpa_defacto_class, take, target_status, everything())%>%
  mutate(estimate = round(estimate,3),
         se = round(se,3),
         pval = round(pval,3),
         ci.lb = round(ci.lb,3),
         ci.ub = round(ci.ub,3),
         tau2 = round(tau2,3),
         Q = round(Q, 3))%>%
  rename("Region" = state_region,
         "Target status" = target_status,
         "Effect size" = estimate,
         "Standard error" = se,
         "P-value" = pval,
         "95% lower" = ci.lb,
         "95% upper" = ci.ub,
         "Allowed take" = take,
         "MPA type" = mpa_defacto_class,
         "Tau-2" = tau2,
         "No. MPA-Ecosystem pairs" = n_mpas
  ) 


write.csv(region_meta_out, file = file.path(tab_dir,"TableS7_region_target_meta_table.csv"),row.names = FALSE)


################################################################################
#extract relevant model output for state x target_status


# Apply the function to each element in 'meta_results'
state_meta_out <- state_results %>%
  #get the habitats included in the analysis for each MPA
  mutate(
    Q = map_dbl(meta_result, ~ pluck(.x, "QE")))%>%
  arrange(target_status)%>%
  #drop columns
  dplyr::select(-meta_result,  -zval, -habitat, -state_region)%>%
  mutate(take = ifelse(mpa_defacto_class == "smr","No-take","Partial-take"),
         mpa_defacto_class = toupper(mpa_defacto_class))%>%
  dplyr::select(mpa_defacto_class, take, target_status, everything())%>%
  mutate(estimate = round(estimate,3),
         se = round(se,3),
         pval = round(pval,3),
         ci.lb = round(ci.lb,3),
         ci.ub = round(ci.ub,3),
         tau2 = round(tau2,3),
         Q = round(Q, 3))%>%
  rename(
         "Target status" = target_status,
         "Effect size" = estimate,
         "Standard error" = se,
         "P-value" = pval,
         "95% lower" = ci.lb,
         "95% upper" = ci.ub,
         "Allowed take" = take,
         "MPA type" = mpa_defacto_class,
         "Tau-2" = tau2,
         "No. MPA-Ecosystem pairs" = n_mpas
  ) 


write.csv(state_meta_out, file = file.path(tab_dir,"TableS6_network_results.csv"),row.names = FALSE)





 
