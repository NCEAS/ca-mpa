

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data/"
indir <- file.path(basedir, "beach_visitors/raw")
outdir <- file.path(basedir, "beach_visitors/processed")
plotdir <- "data/beach_visitors/figures"

# Read data
data_orig <- read.csv(file.path(indir, "beach_visitor_data.csv"), na.strings="", as.is=T)

# Read MPA attributes
mpas <- readRDS(file.path(basedir, "mpa_traits/processed", "CA_mpa_metadata.Rds"))

# Associate reference sites with MPAs more explicitly than just the pair

# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  rename(mpa=mpa_name, site=site_name, activity_code=visitor_code) %>% 
  # Add date
  mutate(date=paste(year, month_num, day, sep="-") %>% lubridate::ymd()) %>% 
  # Format site type
  mutate(site_type=recode(site_type, "Ref"="Reference")) %>% 
  # Add visit type
  mutate(activity=recode(activity_code,
                         "DogLeash"="Dogs leashed",  
                         "DogsAll"="All dogs",    
                         "DogUnl"="Dogs unleashed",   
                         "PeopBch"="People on beach",  
                         "PeopleAll"="All people",  
                         "PeopSurf"="People surfing")) %>% 
  # Recode a few MPA names
  mutate(mpa=recode(mpa, 
                    "Ano Nuevo SMR"="Año Nuevo SMR",      
                    "Campus Point SMCA"="Campus Point SMCA (No-Take)",     
                    "Scripps/Matlahuayl SMR"="Matlahuayl SMR")) %>% 
  # Arrange
  select(region, mpa, mpa_type, mpa_status, site, site_code, site_type, site_pair, 
         year, month, month_num, day, date, activity_code, activity, count, everything())

# Inspect
str(data)
freeR::complete(data)

# Inspect
table(data$region)
table(data$mpa)
table(data$mpa_type)
table(data$mpa_status)
table(data$site)
table(data$site_code)
table(data$site_type)
table(data$site_pair)
range(data$year)
range(data$month_num)
table(data$month)
table(data$visitor_code)

# Check that MPA names are right
data$mpa[!data$mpa%in%mpas$mpa] %>% unique()

# Build MPA key
mpa_key <- data %>% 
  select(mpa, mpa_type, mpa_status) %>% unique()
anyDuplicated(mpa_key$mpa)  

# Build site key
site_key <- data %>% 
  select(site, site_code, site_type, site_pair, mpa) %>% 
  unique()


# Format data
################################################################################

# Export
saveRDS(data, file.path(outdir, "2019_2020_beach_visitors_data.Rds"))
         