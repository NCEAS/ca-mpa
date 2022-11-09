#Joshua G. Smith
#November 9, 2022

rm(list=ls())

require(dplyr)
require(janitor)

inputdir <- "/home/shares/ca-mpa/data/sync-data/species_traits/raw"
outdir <- "/home/shares/ca-mpa/data/sync-data/species_traits/processed"

############################################################################### 
#load dat

affinity_dat_raw <- read.csv(file.path(inputdir, "thermal_affinities_target_status.csv"))%>%
  clean_names()

############################################################################### 
#clean names
affinity_dat1 <- affinity_dat_raw %>%
                rename("common_name"=common_name_of_spp_attribute_table_ucsc_trophic_and_functional_groups)%>%
                select(1:9,20)

#clean thermal_affinity
unique(affinity_dat1$thermal_affinity)

affinity_dat1$thermal_affinity <- recode_factor(affinity_dat1$thermal_affinity,
                  "cosmopolitan (all but tropical and subarctic)"="cosmopolitan")

affinity_dat1$thermal_affinity <- recode_factor(affinity_dat1$thermal_affinity,
                  "cosmopolitan (maybe not subarctic)"="cosmopolitan")

affinity_dat1$thermal_affinity <- recode_factor(affinity_dat1$thermal_affinity,
                  "Delete this since we have both individual species"="NA")

affinity_dat1[affinity_dat1 == "NA"] <- NA
affinity_dat1[affinity_dat1 == "<NA>"] <- NA


############################################################################### 
#save cleaned table

#write.csv(affinity_dat1, file.path(outdir,"thermal_affinities_and_target_status.csv"))





