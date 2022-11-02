## Import Kelp watch data
# Julien Brun, brun@nceas.ucsb.edu

# Data Source on EDI
# https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-sbc.74.17


# install.packages("librarian")
librarian::shelf(tidyverse, tidync, sf, stars)

data_dir <- "~/Data/CA_MPA/habitat/Kelp/"

kelpwatch_file <- "LandsatKelpBiomass_2022_Q2_withmetadata.nc"

kelpwatch_raw <- tidync(file.path(data_dir, kelpwatch_file))

# kelpwatch_grid <- kelpwatch_raw %>% 
#   hyper_array()

kelpwatch_df <- kelpwatch_raw %>% hyper_tibble()

# 
# # Read the data with stars
# kelp_stars <- stars::read_stars(file.path(data_dir, kelpwatch_file)) # warnings because does not recognize dim
# bio <- kelp_stars$biomass # Extract the array for the biomass data
