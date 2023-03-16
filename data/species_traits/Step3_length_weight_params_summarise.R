
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
datadir <- file.path(basedir, "species_traits/processed")

# Read data
data_orig <- read.csv(file.path(datadir, "species_lw_parameters_from_fishbase_full.csv"), as.is=T)
spp_orig <- read.csv(file.path(datadir, "species_key.csv"), as.is=T)


# Id species of interest
################################################################################

# Retrieve taxa
taxa <- freeR::taxa(species=unique(spp_orig$sciname))

# Species of interest
# Only fish id'ed to species level
spp <- spp_orig %>% 
  select(sciname, level) %>% 
  unique() %>% 
  filter(level=="species") %>% 
  left_join(taxa %>% select(family, genus, sciname, type), by="sciname") %>% 
  filter(type=="fish") %>% 
  select(-level)


# LW params
################################################################################

# Retrieve taxa
taxa_data <- freeR::taxa(species=unique(data_orig$species))

# Species summary
lw_spp <- data_orig %>% 
  filter(species %in% spp$sciname & type=="TL") %>% 
  group_by(species) %>% 
  summarize(a=median(a, na.rm=T),
            b=median(b, na.rm=T)) %>% 
  ungroup()

# Genus summary
lw_gen <- data_orig %>% 
  left_join(taxa_data %>% select(sciname, genus), by=c("species"="sciname")) %>% 
  filter(genus %in% spp$genus & type=="TL") %>% 
  group_by(genus) %>% 
  summarize(a=median(a, na.rm=T),
            b=median(b, na.rm=T)) %>% 
  ungroup()

# Family summary
lw_fam <- data_orig %>% 
  left_join(taxa_data %>% select(sciname, family), by=c("species"="sciname")) %>% 
  filter(family %in% spp$family & type=="TL") %>% 
  group_by(family) %>% 
  summarize(a=median(a, na.rm=T),
            b=median(b, na.rm=T)) %>% 
  ungroup()

# Create final data
data <- spp %>% 
  # Arrange
  select(type, family, genus, sciname) %>% 
  # Add species lw
  left_join(lw_spp, by=c("sciname"="species")) %>% 
  rename(a_spp=a, b_spp=b) %>% 
  # Add genus lw
  left_join(lw_gen, by=c("genus"="genus")) %>% 
  rename(a_gen=a, b_gen=b) %>%
  # Add family lw
  left_join(lw_fam, by=c("family"="family")) %>% 
  rename(a_fam=a, b_fam=b) %>% 
  # Final lw parama
  mutate(lw_source=case_when(!is.na(a_spp) ~ "Species",
                             !is.na(a_gen) & is.na(a_spp) ~ "Genus",
                             !is.na(a_fam) & is.na(a_spp) & is.na(a_gen) ~ "Family",
                             T ~ "Unknown")) %>% 
  mutate(a=case_when(lw_source=="Species" ~ a_spp,
                     lw_source=="Genus" ~ a_gen,
                     lw_source=="Family" ~ a_fam,
                     T ~ 0),
         a=ifelse(a==0, NA, a),
         b=case_when(lw_source=="Species" ~ b_spp,
                     lw_source=="Genus" ~ b_gen,
                     lw_source=="Family" ~ b_fam,
                     T ~ 0),
         b=ifelse(b==0, NA, b))

