
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data/"
indir <- file.path(basedir, "reef/raw")
outdir <- file.path(basedir, "reef/processed")
plotdir <- "data/reef/figures"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "PACsurveys061522.xlsx"), na="NULL")

# Read MPAs
mpas <- readRDS(file.path(basedir, "gis_data/processed", "CA_mpa_polygons.Rds"))


# Format data
################################################################################

# Convert latitude
conv_lat <- function(lat_chr){
  deg <- substr(lat_chr, 1, 2) %>% as.numeric()
  min <- substr(lat_chr, 4, nchar(lat_chr)) %>% as.numeric()
  lat <- deg + min/60
  return(lat)
}

# Convert longitude
conv_long <- function(long_chr){
  deg <- substr(long_chr, 2, 4) %>% as.numeric()
  min <- substr(long_chr, 6, nchar(long_chr)) %>% as.numeric()
  long <- (deg + min/60) * -1
  return(long)
}


# Format data
data <- data_orig %>% 
  # Rename
  rename(survey_id=formid,
         surveyor_type=exp,
         survey_type=type,
         site_id=geogr, 
         surface_temp_f=stemp,
         bottom_temp_f=btemp,
         visibility_code=visibility,
         current_code=current,
         start_time=start, 
         bottom_time=btime,
         habitat_code=habitat,
         lat_dd_orig=lat, 
         long_dd_orig=lon,
         avg_depth_code=averagedepth,
         max_depth_code=maxdepth) %>% 
  # Format survey type
  mutate(survey_type=recode(survey_type, 
                            "1"="Fish only",
                            "2"="Invertebrate only",
                            "3"="Both fish and invertebrates")) %>% 
  # Format surveyor type
  mutate(surveyor_type=recode(surveyor_type,
                            "E"="Expert", "N"="Novice"),
         surveyor_type=ifelse(surveyor_type=="5", NA, surveyor_type)) %>% 
  # Format date
  mutate(date=lubridate::ymd(date)) %>% 
  # Format temperatures
  mutate(surface_temp_f=ifelse(surface_temp_f==0, NA, surface_temp_f),
         bottom_temp_f=ifelse(bottom_temp_f==0, NA, bottom_temp_f)) %>% 
  # Format visibility
  mutate(visibility=recode(visibility_code, 
                           "0"="",
                           "1"="<10 feet",
                           "2"="10-24 feet",
                           "3"="25-49 feet",
                           "4"="50-74 feet",
                           "5"="75-99 feet",
                           "6"="100-149 feet",
                           "7"=">149 ft"),
         visibility=ifelse(visibility=="", NA, visibility)) %>% 
  # Format current
  mutate(current=recode(current_code, 
                        "0"="",
                        "1"="None",
                        "2"="Weak",
                        "3"="Strong"),
         current=ifelse(current=="", NA, current)) %>% 
  # Format habitat
  mutate(habitat=recode(habitat_code,
                        "0"="",
                        "1"="Kelp forest",
                        "2"="Rocky reef",
                        "3"="Artificial reef",
                        "4"="Sandy bottom",
                        "5"="Open ocean",
                        "6"="Eel grass",
                        "7"="Surf grass",
                        "8"="Pinnacle",
                        "9"="Bull kelp",
                        "10"="Mud/silt bottom",
                        "11"="Cobblestone/boulder field",
                        "12"="Wall",
                        "13"="Mixed"), 
         habitat=ifelse(habitat=="", NA, habitat)) %>% 
  # Format max depth
  mutate(max_depth=recode(max_depth_code, 
                          "0"="",
                          "1"="Snorkel",
                          "2"="<10 feet",
                          "3"="10-19 feet",
                          "4"="20-29 feet",
                          "5"="30-39 feet",
                          "6"="40-49 feet",
                          "7"="50-59 feet",
                          "8"="60-69 feet",
                          "9"="70-79 feet",
                          "10"="80-89 feet",
                          "11"="90-99 feet",
                          "12"="100-109 feet",
                          "13"="110-119 feet",
                          "14"="120-129 feet",
                          "15"="130-139 feet",
                          "16"="140-149 feet"),
         max_depth=ifelse(max_depth=="", NA, max_depth)) %>% 
  # Format average depth
  mutate(avg_depth=recode(avg_depth_code, 
                          "0"="0",
                          "1"="Snorkel",
                          "2"="<10 feet",
                          "3"="10-19 feet",
                          "4"="20-29 feet",
                          "5"="30-39 feet",
                          "6"="40-49 feet",
                          "7"="50-59 feet",
                          "8"="60-69 feet",
                          "9"="70-79 feet",
                          "10"="80-89 feet",
                          "11"="90-99 feet",
                          "12"="100-109 feet",
                          "13"="110-119 feet",
                          "14"="120-129 feet",
                          "15"="130-139 feet",
                          "16"="140-149 feet"),
         avg_depth=ifelse(avg_depth=="", NA, avg_depth)) %>% 
  # Format lat
  mutate(lat_dd_orig=recode(lat_dd_orig, 
                       "33 33. 85"="33 33.85"), 
         lat_dd=conv_lat(lat_dd_orig)) %>% 
  # Format long
  mutate(long_dd_orig=recode(long_dd_orig, 
                            "-117 50. 09"="-117 50.09"), 
         long_dd=conv_long(long_dd_orig)) %>% 
  # Arrange
  select(survey_id, survey_type, site_id, site_name, date,
         surveyor_type,
         start_time, bottom_time,
         habitat_code, habitat, 
         max_depth_code, max_depth,
         avg_depth_code, avg_depth,
         surface_temp_f, bottom_temp_f, 
         visibility_code, visibility,
         current_code, current,
         lat_dd_orig, lat_dd, 
         long_dd_orig, long_dd, everything()) %>% 
  select(-c(long_dd_orig, lat_dd_orig))
  
  
# Inspect
str(data)
freeR::complete(data)

# Survey id unique?
freeR::which_duplicated(data$survey_id)

# Inspect character
table(data$survey_type)
table(data$surveyor_type)
table(data$visibility_code)
table(data$visibility)
table(data$current_code)
table(data$current)
table(data$habitat_code)
table(data$habitat)
table(data$avg_depth_code)
table(data$avg_depth)
table(data$max_depth_code)
table(data$max_depth)

# Inspect numeric
range(data$date)
range(data$surface_temp_f, na.rm=T) # 0s are suspicious
range(data$bottom_temp_f, na.rm=T) # 0s are suspicious
boxplot(data$surface_temp_f)
boxplot(data$bottom_temp_f)

# Coordinates
if(F){
  data %>% 
    filter(!is.na(lat_dd_orig) & is.na(lat_dd)) %>% 
    pull(lat_dd_orig) %>% unique()
  data %>% 
    filter(!is.na(long_dd_orig) & is.na(long_dd)) %>% 
    pull(long_dd_orig) %>% unique()
}

# Plot data
ggplot(data, aes(x=long_dd, y=lat_dd, color=habitat)) +
  geom_point() +
  theme_bw()

# Site key
# Site id is unique but site name is not
# Coordinates are not repeated! Nice!
site_key <- data %>% 
  select(site_id, site_name, long_dd, lat_dd) %>% 
  unique()
freeR::which_duplicated(site_key$site_id)
freeR::which_duplicated(site_key$site_name)

# Sites without coordinates
site_key_no_xy <- site_key %>% 
  filter(is.na(lat_dd))
write.csv(site_key_no_xy, file=file.path(outdir, "REEF_sites_without_xy_data.csv"), row.names = F)


# Mark if inside MPA
################################################################################

# Format MPAs
mpas_simple <- mpas %>% select(name)
mpas_simple_sp <- mpas_simple %>% as(., "Spatial")

# Data with xy
data_xy <- data %>% 
  filter(!is.na(lat_dd))

# Convert to sf
data_sf <- data_xy %>%
  sf::st_as_sf(coords=c("long_dd", "lat_dd"), crs=sf::st_crs(mpas))

# Convert to sp
data_sp <- data_sf %>%
  as(., "Spatial")

# Find points inside MPAs
inside_which_mpa <- sp::over(data_sp, mpas_simple_sp)
inside_which_mpa_chr <- inside_which_mpa$name

# Convert to dataframe
data_sf_df <- data_sf %>%
  sf::st_drop_geometry() %>% 
  mutate(mpa=inside_which_mpa_chr) %>% 
  filter(!is.na(mpa))

# Add MPA to data
data2 <- data %>% 
  left_join(data_sf_df %>% select(survey_id, mpa))


# Export data
################################################################################

# Export data
saveRDS(data2, file=file.path(outdir, "REEF_1994_2022_survey_metadata.Rds"))


