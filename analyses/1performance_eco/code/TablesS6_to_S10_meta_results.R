#title: "MPA-level meta analysis results table"
#author: "Joshua G. Smith"
#date: "8/1/2023"

rm(list=ls())

#required packages
librarian::shelf(ggplot2, tidyverse, purrr)

#set directories
fig_dir <- here::here("analyses","1performance_eco","figures")
tab_dir <- here::here("analyses","1performance_eco","tables")
dat_path <- here::here("analyses","1performance_eco","output")

#read data
mpa_meta_results <- readRDS(file.path(dat_path, "mpa_level_meta_results2.Rds")) 
habitat_region_results <- readRDS(file.path(dat_path, "habitat_region_meta_results2.Rds")) 
habitat_target_results <- readRDS(file.path(dat_path, "habitat_target_meta_results2.Rds")) 
region_results <- readRDS(file.path(dat_path, "region_meta_results2.Rds")) 
state_results <- readRDS(file.path(dat_path, "state_meta_results2.Rds")) 


################################################################################
#extract relevant model output for mpa-level analyses

mpa_meta_out <- mpa_meta_results %>%
  mutate(tau2 = map_dbl(meta_result, ~ pluck(.x, "tau2")),
         Q = map_dbl(meta_result, ~ pluck(.x, "QE")),
         habitats = map(seq_along(meta_result), ~ meta_result[[.x]][["data"]][["habitat"]]),
         habitats = map_chr(habitats, ~ str_c(.x, collapse = ", ")),
         years = map(seq_along(meta_result), ~ meta_result[[.x]][["data"]][["year"]]),
         years = map_chr(years, ~ str_c(.x, collapse = ", "))) %>%
  arrange(state_region, mpa, target_status) %>%
  mutate(MPA_number = group_indices(., mpa)) %>%
  dplyr::select(-meta_result, -n_habitat, -zval) %>%
  dplyr::select(MPA_number, state_region, mpa, target_status, everything()) %>%
  mutate(estimate = round(estimate, 3),
         se = round(se, 3),
         pval = round(pval, 3),
         ci.lb = round(ci.lb, 3),
         ci.ub = round(ci.ub, 3),
         tau2 = round(tau2, 3),
         Q = round(Q, 3)) %>%
  # Format p-values
  mutate(pval = ifelse(pval < 0.001, "<0.001", as.character(pval))) %>%
  mutate(Ecosystem_Year = map2(habitats, years, function(ecosystems, years) {
    ecosystems <- str_split(ecosystems, ", ")[[1]]
    years <- str_split(years, ", ")[[1]]
    formatted <- paste(ecosystems, " (", years, ")", sep = "")
    return(paste(formatted, collapse = ", "))
  })) %>%
  dplyr::select(-habitats, -years) %>%
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
         "Ecosystem (latest year)" = Ecosystem_Year) %>%
  mutate(Region = str_remove(Region, "Coast"),
         `Ecosystem (latest year)` = as.character(`Ecosystem (latest year)`))

write.csv(mpa_meta_out, file = file.path(tab_dir, "TableS10_mpa_meta_table2.csv"), row.names = FALSE)




################################################################################
hab_region_meta_out <- habitat_region_results %>%
  mutate(
    Q = map_dbl(meta_result, ~ pluck(.x, "QE"))
  ) %>%
  arrange(habitat, state_region, target_status) %>%
  # Drop unnecessary columns
  dplyr::select(-meta_result, -zval) %>%
  mutate(
    take = ifelse(mpa_defacto_class == "smr", "No-take", "Partial-take"),
    mpa_defacto_class = toupper(mpa_defacto_class),
    # Update "Nontargeted" to "Non-targeted"
    target_status = str_replace(target_status, "Nontargeted", "Non-targeted")
  ) %>%
  dplyr::select(habitat, state_region, mpa_defacto_class, take, target_status, everything()) %>%
  # Round numeric values
  mutate(
    estimate = round(estimate, 3),
    se = round(se, 3),
    # Format p-values
    pval = ifelse(pval < 0.001, "<0.001", as.character(round(pval, 3))),
    ci.lb = round(ci.lb, 3),
    ci.ub = round(ci.ub, 3),
    tau2 = round(tau2, 3),
    Q = round(Q, 3)
  ) %>%
  # Rename columns for clarity
  rename(
    "Ecosystem" = habitat,
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
  # Additional formatting
  mutate(
    Region = str_remove(Region, "Coast"),
    Ecosystem = factor(Ecosystem, levels = c("Surf zone", "Kelp forest", "Shallow reef", "Deep reef"))
  ) %>%
  # Order by Ecosystem after the factor levels have been set
  arrange(Ecosystem)



write.csv(hab_region_meta_out, file = file.path(tab_dir,"TableS9_habitat_region_meta_table2.csv"),row.names = FALSE)


################################################################################
#extract relevant model output for habitat x target_status


hab_target_meta_out <- habitat_target_results %>%
  mutate(
    Q = map_dbl(meta_result, ~ pluck(.x, "QE"))
  ) %>%
  arrange(habitat, target_status) %>%
  # Drop unnecessary columns
  dplyr::select(-meta_result, -zval, -state_region) %>%
  mutate(
    take = ifelse(mpa_defacto_class == "smr", "No-take", "Partial-take")
  ) %>%
  dplyr::select(habitat, mpa_defacto_class, take, target_status, everything()) %>%
  # Uppercase MPA class and round numeric columns
  mutate(
    mpa_defacto_class = toupper(mpa_defacto_class),
    estimate = round(estimate, 3),
    se = round(se, 3),
    # Format p-values
    pval = ifelse(pval < 0.001, "<0.001", as.character(round(pval, 3))),
    ci.lb = round(ci.lb, 3),
    ci.ub = round(ci.ub, 3),
    tau2 = round(tau2, 3),
    Q = round(Q, 3),
    # Replace "Nontargeted" with "Non-targeted"
    target_status = str_replace(target_status, "Nontargeted", "Non-targeted")
  ) %>%
  # Rename columns for clarity
  rename(
    "Ecosystem" = habitat,
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
  # Additional formatting
  mutate(
    Ecosystem = factor(Ecosystem, levels = c("Surf zone", "Kelp forest", "Shallow reef", "Deep reef"))
  ) %>%
  # Order by Ecosystem after the factor levels have been set
  arrange(Ecosystem)


write.csv(hab_target_meta_out, file = file.path(tab_dir,"TableS8_hab_target_meta_table2.csv"),row.names = FALSE)


################################################################################
#extract relevant model output for region x target_status


region_meta_out <- region_results %>%
  # Extract Q from meta_result
  mutate(Q = map_dbl(meta_result, ~ pluck(.x, "QE"))) %>%
  arrange(state_region, target_status) %>%
  # Drop unnecessary columns
  dplyr::select(-meta_result, -zval, -habitat) %>%
  mutate(
    take = ifelse(mpa_defacto_class == "smr", "No-take", "Partial-take"),
    mpa_defacto_class = toupper(mpa_defacto_class)
  ) %>%
  dplyr::select(state_region, mpa_defacto_class, take, target_status, everything()) %>%
  # Round numeric values and format p-values
  mutate(
    estimate = round(estimate, 3),
    se = round(se, 3),
    # Format p-values
    pval = ifelse(pval < 0.001, "<0.001", as.character(round(pval, 3))),
    ci.lb = round(ci.lb, 3),
    ci.ub = round(ci.ub, 3),
    tau2 = round(tau2, 3),
    Q = round(Q, 3),
    # Replace "Nontargeted" with "Non-targeted"
    target_status = str_replace(target_status, "Nontargeted", "Non-targeted")
  ) %>%
  # Rename columns for clarity
  rename(
    "Region" = state_region,
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


write.csv(region_meta_out, file = file.path(tab_dir,"TableS7_region_target_meta_table2.csv"),row.names = FALSE)


################################################################################
#extract relevant model output for state x target_status

state_meta_out <- state_results %>%
  # Extract Q from meta_result
  mutate(Q = map_dbl(meta_result, ~ pluck(.x, "QE"))) %>%
  arrange(target_status) %>%
  # Drop unnecessary columns
  dplyr::select(-meta_result, -zval, -habitat, -state_region) %>%
  mutate(
    take = ifelse(mpa_defacto_class == "smr", "No-take", "Partial-take"),
    mpa_defacto_class = toupper(mpa_defacto_class)
  ) %>%
  dplyr::select(mpa_defacto_class, take, target_status, everything()) %>%
  # Round numeric values and format p-values
  mutate(
    estimate = round(estimate, 3),
    se = round(se, 3),
    # Format p-values
    pval = ifelse(pval < 0.001, "<0.001", as.character(round(pval, 3))),
    ci.lb = round(ci.lb, 3),
    ci.ub = round(ci.ub, 3),
    tau2 = round(tau2, 3),
    Q = round(Q, 3),
    # Replace "Nontargeted" with "Non-targeted"
    target_status = str_replace(target_status, "Nontargeted", "Non-targeted")
  ) %>%
  # Rename columns for clarity
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

write.csv(state_meta_out, file = file.path(tab_dir,"TableS6_network_results2.csv"),row.names = FALSE)





 
