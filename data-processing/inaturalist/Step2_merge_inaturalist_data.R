

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(rinat)
library(tidyverse)

# Directories
indir <- "data/inaturalist/raw"
outdir <- "data/inaturalist/processed"
plotdir <- "data/inaturalist/figures"


# Merge data
################################################################################

# Files to merge
files2merge <- list.files(indir)

# Loop through and merge
x <- files2merge[1]
data_orig <- purrr::map_df(files2merge, function(x){

  # Read data
  fdata <- read.csv(file.path(indir, x), as.is=T, na.strings = c("", "NA")) %>%
    mutate(across(everything(), as.character))

})


# Format data
################################################################################

# Format data
data <- data_orig %>%
  # Rename
  rename(date_obs=observed_on,
         date_obs_orig=observed_on_string,
         datetime_obs1=datetime,
         datetime_obs2=time_observed_at,
         datetime_obs_tz=time_zone,
         sci_name=scientific_name,
         lat_dd=latitude,
         long_dd=longitude,
         comm_name=common_name,
         user_name=user_login,
         n_id_agree=num_identification_agreements,
         n_id_disagree=num_identification_disagreements,
         taxa_catg=iconic_taxon_name,
         name_guess=species_guess,
         taxa_id=taxon_id,
         quality=quality_grade) %>%
  # Add year and month
  mutate(date_obs=lubridate::ymd(date_obs),
         year_obs=lubridate::year(date_obs),
         month_obs=lubridate::month(date_obs)) %>%
  # Arrange
  select(year_obs, month_obs, date_obs, date_obs_orig,
         datetime_obs1, datetime_obs2, datetime_obs_tz,
         place_guess, lat_dd, long_dd,
         user_id, user_name,
         taxa_id, taxa_catg, comm_name, sci_name, name_guess,
         n_id_agree, n_id_disagree, quality,
         everything())


# Inspect
str(data)
freeR::complete(data)

# User key
user_key <- data %>%
  select(user_name, user_id) %>%
  unique()
freeR::which_duplicated(user_key$user_name) # user name is unique
freeR::which_duplicated(user_key$user_name) # user id is unique

# Taxa key
taxa_key <- data %>%
  select(taxa_id, taxa_catg, sci_name, comm_name) %>%
  unique()
freeR::which_duplicated(taxa_key$taxa_id) # taxa id is almost unique
freeR::which_duplicated(taxa_key$sci_name) # scientific name is not unique
freeR::which_duplicated(taxa_key$comm_name) # common name is not unique


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "2000_2020_inaturalist_data.Rds"))


