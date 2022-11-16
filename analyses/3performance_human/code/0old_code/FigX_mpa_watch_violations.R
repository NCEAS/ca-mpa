

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(lubridate)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
gisdir <- file.path(basedir, "gis_data/processed")
datadir <- file.path(basedir, "mpa_watch/processed")
plotdir <- "analyses/3performance_human/figures"

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Read MPA data
mpas_orig <- readRDS(file.path(basedir, "mpa_traits/processed", "CA_mpa_metadata.Rds"))

# Read MPW watch data
data_orig <- readRDS(file.path(datadir, "MPA_Watch_2011_2022_surveys_ca_programs_wide.Rds"))
col_key <- readxl::read_excel(file.path(datadir, "column_key_ca_programs.xlsx"))

# MPA types
types_use <- c("SMR", "SMRMA", "SMCA (No-Take)", "SMCA")


# Build data
################################################################################

# Build data
data_wide <- data_orig %>% 
  # Reduce to MPAs
  filter(survey_type=="MPA") %>% 
  # Add MPA metadata
  left_join(mpas_orig %>% select(mpa, region, type), by="mpa") %>% 
  # Reduce to MPAs of interest
  filter(type %in% types_use) %>% 
  # Order MPA types
  mutate(type=factor(type, levels=types_use)) %>% 
  rename(mpa_type=type) %>% 
  # Simplify
  select(region, mpa_type, mpa, mpa_id, survey_id, survey_type,
         date, time_start2, time_end2, duration_hr, total_activities:comments) %>% 
  select(-comments) %>% 
  # Remove invalid surveys
  filter(duration_hr > 0)

# Inspect
freeR::complete(data_wide)

# Build data
data_long <- data_wide %>% 
  # Gather
  gather(key="activity_orig", value="activity_n", 11:ncol(.)) %>% 
  # Add column data
  left_join(col_key %>% select(activity_orig, activity, activity_type1, activity_type2, activity_type3, activity_type4), by="activity_orig") %>% 
  # Reduce to violations data
  filter(activity_type2=="Violations")

# Inspect
table(data_long$activity_type1)
table(data_long$activity_type2)
table(data_long$activity_type3)
table(data_long$activity_type4)
table(data_long$activity)

# Build data
################################################################################

stats <- data_long %>% 
  # Remove Y/N questions
  filter(!grepl("Y/N", activity)) %>% 
  # Convert values to numeric
  mutate(activity_n=as.numeric(activity_n)) %>% 
  # Group by MPA
  group_by(mpa, mpa_type) %>% 
  summarize(n_surveys=n_distinct(survey_id),
            n_violations_obs=sum(activity_n[activity=="Number of violations observed"]),
            n_violations_rep=sum(activity_n[activity=="Number of violations reported"])) %>% 
  ungroup()

g <- ggplot(stats, aes(x=n_violations_obs, y=mpa_type, group=mpa_type)) +
  geom_boxplot()
g



