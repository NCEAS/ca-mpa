

rm(list=ls())

require(dplyr)
require(stringr)

outdir <- "analyses/1performance_eco/output"

#load data
diversity_dat <- read.csv(file.path(outdir, "all_fish_diversity.csv"))
fishing_pressure <- readRDS(file.path(outdir, "pre_mpa_fishing_pressure_by_mpa.Rds"))

#load MPA traits

data_path <- "/home/shares/ca-mpa/data/sync-data/mpa_traits/processed"
input_file <- "mpa_attributes_clean.csv" 
traits <- read.csv(file.path(data_path, input_file)) 

################################################################################

#select traits
trait_select <- traits %>%
  dplyr::select(long, lat, affiliated_mpa=name, distance_to_port, implementation_date, size_km2,
               port_size, min_depth_m, max_depth_m, depth_range) %>%
  mutate(implementation_year = format(as.Date(implementation_date, format="%m/%d/%Y"),"%Y"))

#Join MPA trait data

diversity_data_mods <- left_join(diversity_dat, trait_select, by="affiliated_mpa") %>%
  mutate(mpa_age = as.numeric(year)-as.numeric(implementation_year))


################################################################################
#Join fishing pressure

#process for join
fishing_pressure$mpa <- tolower(fishing_pressure$mpa)

fishing_pressure1 <- fishing_pressure %>%
                  mutate(mpa = gsub(" \\(no-take\\)","",mpa))

fishing_pressure1$mpa <- recode_factor(fishing_pressure1$mpa,
                                       "año nuevo smr" = "ano nuevo smr")

diversity_data_mods1 <- left_join(diversity_data_mods, 
                                fishing_pressure1,
                                by=c("affiliated_mpa" = "mpa"))
#check for unmatched mpas

fishing_anti <- anti_join(diversity_data_mods, 
                                   fishing_pressure1,
                                   by=c("affiliated_mpa" = "mpa"))


#export
#write.csv(diversity_data_mods1, file.path(outdir, "MPA_fish_diversity_with_mods.csv"))


