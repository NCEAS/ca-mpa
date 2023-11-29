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


################################################################################
#extract relevant model output

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
  arrange(state_region, affiliated_mpa, target_status)%>%
  #numerate each unique MPA
  mutate(MPA_number = group_indices(., affiliated_mpa))%>%
  #drop columns
  dplyr::select(-meta_result, -n_habitat, -zval)%>%
  dplyr::select(MPA_number,state_region,affiliated_mpa, target_status, everything())%>%
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
         "MPA name" = affiliated_mpa,
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


write.csv(mpa_meta_out, file = file.path(tab_dir,"TableSX_mpa_meta_table.csv"),row.names = FALSE)








 
