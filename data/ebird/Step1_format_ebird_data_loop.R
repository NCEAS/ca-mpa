

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(rinat)
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data/"
indir <- file.path(basedir, "ebird/raw/ebd_US-CA_relMay-2022")
outdir <- file.path(basedir, "ebird/intermediate")
plotdir <- "data/ebird/figures"

# Number of rows (~86 million)
# nrow(data.table::fread(file.path(indir, "ebd_US-CA_relMay-2022.txt"), select = 1L))

# The eBird data is gigantic
# To ease processing, I'm going to format in 2 million row chunks
# Then, I'll clip those chunks to MPAs and merge the clipped chunks into a single file


# Format data
################################################################################

# Loop through sets of 1 million
i <- 86
for(i in 1:100){
  
  # Get columns names
  print(i)
  colnames <- read.delim(file.path(indir, "ebd_US-CA_relMay-2022.txt"), 
                         na.strings = "", nrows = 1, header=T) %>% colnames()
  
  # Read data
  row1 <- (i-1)*1e6 + 1
  row2 <- i*1e6
  data_orig <- read.delim(file.path(indir, "ebd_US-CA_relMay-2022.txt"), 
                          na.strings = "", skip=row1, nrows = 1e6, header=F)
  
  # Format data
  data <- data_orig %>% 
    # Rename
    setNames(colnames) %>% 
    janitor::clean_names("snake") %>% 
    rename(record_id=global_unique_identifier,
           date_editted=last_edited_date,
           order_id=taxonomic_order,
           comm_name=common_name,
           sci_name=scientific_name, 
           comm_name_sub=subspecies_common_name, 
           sci_name_sub=subspecies_scientific_name,
           exotic=exotic_code,
           all_spp_reported_yn=all_species_reported, 
           count=observation_count,
           block_code=atlas_block, 
           lat_dd=latitude, 
           long_dd=longitude, 
           survey_date=observation_date,
           survey_time= time_observations_started, 
           survey_id=sampling_event_identifier,
           survey_min=duration_minutes,
           survey_km=effort_distance_km,
           survey_ha=effort_area_ha,
           observers_n=number_observers,
           group_id=group_identifier,
           media_yn=has_media,
           approved_yn=approved, 
           reviewed_yn=reviewed) %>% 
    # Format locality type
    # mutate(locality_type=recode_factor(locality_type, 
    #                                    "S"="State",
    #                                    "C"="County",
    #                                    "PC"="Postal code",
    #                                    "T"="Town",
    #                                    "H"="Hotspot",
    #                                    "P"="Personal")) %>% 
    # Format date
    mutate(survey_date=lubridate::ymd(survey_date)) %>% 
    # Format count
    # X denotes species was present but not counted
    # Therefore, every species present, even if count=NA, was observed
    mutate(count=gsub("X", "", count) %>% as.numeric()) %>% 
    # Format exotic code
    # mutate(exotic=recode_factor(exotic, 
    #                             "N"="Naturalized",
    #                             "P"="Provisional",
    #                             "X"="Escapee")) %>% 
    # Format breeding category
    # mutate(breeding_category=recode_factor(breeding_category,
    #                                        "C1"="Observed", 
    #                                        "C2"="Possible", 
    #                                        "C3"="Probable", 
    #                                        "C4"="Confirmed")) %>% 
    # Arrange
    select(record_id, 
           # Location
           country, country_code, state, state_code, county, county_code, 
           locality, locality_id, locality_type, 
           iba_code, bcr_code, usfws_code, block_code,
           lat_dd, long_dd, 
           # Survey
           survey_id, survey_date, survey_time, survey_min, survey_km, survey_ha,
           project_code, protocol_type, protocol_code, all_spp_reported_yn, trip_comments, 
           # Observer
           observers_n, group_id, observer_id, 
           # Taxonomic id
           order_id, comm_name, sci_name, comm_name_sub, sci_name_sub, category, taxon_concept_id, 
           # Other info
           exotic, breeding_code, breeding_category, behavior_code, age_sex, species_comments,
           # Count
           count, media_yn, approved_yn, reviewed_yn, reason, 
           # Everything
           everything()) %>% 
    # Trim
    select(-c(country, country_code, state, state_code, protocol_code, x))
  
  # Export
  outfile <- paste0("CA_ebird_data_", stringr::str_pad(i, width=3, pad="0", side = "left"), ".Rds")
  saveRDS(data, file.path(outdir, outfile))
  
}


# # Inspect
# str(data)
# 
# # Time
# range(data$survey_date)
# 
# # Location
# # IBA, BCR, USFWS, and Atlas codes ignored for now
# table(data$country)
# table(data$country_code)
# table(data$state)
# table(data$state_code)
# table(data$county)
# table(data$county_code)
# sort(unique(data$locality))
# table(data$locality_type)
# 
# # Survey
# table(data$protocol_code)
# table(data$protocol_type)
# table(data$project_code)
# table(data$all_species_reported)
# 
# # Species
# table(data$exotic)
# table(data$breeding_code) # code be recoded
# table(data$breeding_category)
# table(data$behavior_code) # code be recoded
# sort(unique(data$age_sex))
# 
# # Observation info
# range(data$count, na.rm=T)
# 
# # Extra column
# sort(unique(data$x))

# Plot map
# g <- ggplot(data, aes(x=long_dd, y=lat_dd)) +
#   geom_point(pch=1, size=0.3) +
#   theme_bw()
# g



