rm(list = ls())

#biomass response ratios for targeted and nontargeted fish
#Joshua G. Smith
#June 3, 2022



# required packages -------------------------------------------------------

require(ggpubr)
require(tidytext)
require(dplyr)
require(ggplot2)


# load data ---------------------------------------------------------------


data_path <- "/home/shares/ca-mpa/data/sync-data/processed_data"
input_file <- "targeted_nontargeted_fish_biomass.csv" 

means.data <- read.csv(file.path(data_path, input_file))

means.data$mpa_designation <- recode_factor(means.data$mpa_designation, smca="smr") #recode to match defacto SMR

mpa_pairs <- means.data %>%
  group_by(year, group,region4, mpa_class, mpa_designation,target_status)%>%
  mutate(row = row_number())%>%
  pivot_wider(names_from = mpa_designation,
              values_from = c(sum_biomass)
  )%>%
  dplyr::select(-row)%>%
  mutate(across(1:4, ~replace(., lengths(.) == 0, NA)))%>%    
  filter(!is.na(smr))%>%     #remove sites with missing pairs
  filter(!is.na(ref)) #remove sites with missing pairs


# Calculate response ratios -----------------------------------------------------

#add 10% constant to fill zeros
m.smr <- mean(mpa_pairs$smr)*0.1
m.ref <- mean(mpa_pairs$ref)*0.1

mpa_pairs$smr<- m.smr+mpa_pairs$smr
mpa_pairs$ref<- m.ref+mpa_pairs$ref

pairs_stats <- mpa_pairs %>% 
  mutate(logRR  = log(smr/ref)
  )

#export
#path_aurora <- "/home/shares/ca-mpa/data/sync-data/processed_data" 
#write.csv(pairs_stats,file.path(path_aurora, "targeted_nontargeted_biomass_logRRs.csv"), row.names = FALSE)












