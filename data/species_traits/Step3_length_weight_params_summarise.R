# Summarize length weight parameters
# CL 29 Aug 2023

# This script reads the species key and length weight parameter data created in
# Step 1 and Step 2. 


# Setup ------------------------------------------------------------------------------

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
#basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
basedir <- "/home/shares/ca-mpa/data/sync-data/" #Josh
datadir <- file.path(basedir, "species_traits/processed")

# Read length weight parameter data
lw_orig <- read.csv(file.path(datadir, "species_lw_parameters_from_fishbase_full_new.csv"), as.is=T) %>% 
  clean_names() %>% 
  rename(sciname = species)

# Read species key (cleaned taxonomy data for all habitats)
spp_orig <- read.csv(file.path(datadir, "species_key.csv"), as.is=T) %>% 
  clean_names() %>% 
  filter(kingdom == "Animalia") # sorry algae, love you 

# Read taxa from fishbase and sealifebase
sp_fb <- rfishbase::load_taxa("fishbase") %>% 
  mutate(database = "fishbase") %>% 
  select(database, SpecCode, sciname = Species, Genus, Family, Order, Class) 

sp_slb <- rfishbase::load_taxa("sealifebase") %>% 
  mutate(database = "sealifebase") %>% 
  select(database, SpecCode, sciname = Species, Genus, Family, Order, Class)

taxa <- full_join(sp_fb, sp_slb) %>% setNames(tolower(colnames(.))) 

rm(sp_fb, sp_slb)

freeR::complete(data_orig)

# Examine groups of interest -----------------------------------------------------------------
spp <- spp_orig %>% 
  distinct(sciname, level)  %>% 
  filter(level == "species") %>% 
  mutate(in_taxa = if_else(sciname %in% taxa$sciname, "Yes", "No"),
         has_lw = if_else(sciname %in% lw_orig$sciname, "Yes", "No"))

# Genus of interest
gen <- spp_orig %>% 
  distinct(genus, sciname, level) %>% 
  filter(level == "genus") %>% 
  mutate(in_taxa = if_else(genus %in% taxa$genus, "Yes", "No"),
         has_lw = if_else(genus %in% lw_orig$genus, "Yes", "No"))
# Note: 5 genuses will not match: "Acanthinucella" "Cyanoplax" "Mexacanthina" "Neobernaya"  "Diaperoforma"


# Family of interest
fam <- spp_orig %>% 
  distinct(family, sciname, level) %>% 
  filter(level == "family") %>% 
  mutate(in_taxa = if_else(family %in% taxa$family, "Yes", "No"),
         has_lw = if_else(family %in% lw_orig$family, "Yes", "No"))

# Build LW params ---------------------------------------------------------------

# Species summary
lw_spp <- lw_orig %>% 
  filter(sciname %in% spp$sciname) %>% 
  group_by(sciname, type) %>% 
  summarize(a=median(a, na.rm=T),
            b=median(b, na.rm=T),
            n=n()) %>% 
  ungroup()


# Genus summary
lw_gen <- data_orig %>% 
  left_join(taxa_data %>% select(sciname, genus), by=c("species"="sciname")) %>% 
  filter(genus %in% spp$genus & type=="TL") %>% 
  group_by(genus) %>% 
  summarize(a=median(a, na.rm=T),
            b=median(b, na.rm=T)) %>% 
  ungroup()

write.csv(lw_gen, file=file.path(datadir, "fishbase_lw_parameters_by_genus.csv"), row.names = F)

# Family summary
lw_fam <- data_orig %>% 
  left_join(taxa_data %>% select(sciname, family), by=c("species"="sciname")) %>% 
  filter(family %in% spp$family & type=="TL") %>% 
  group_by(family) %>% 
  summarize(a=median(a, na.rm=T),
            b=median(b, na.rm=T)) %>% 
  ungroup()

write.csv(lw_fam, file=file.path(datadir, "fishbase_lw_parameters_by_family.csv"), row.names = F)

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
         b=ifelse(b==0, NA, b)) %>% 
  # Simplify
  select(type, family, genus, sciname, lw_source, a, b) %>% 
  arrange(type, family, genus, sciname)

# Export data
write.csv(data, file=file.path(datadir, "fishbase_lw_parameters_by_species.csv"), row.names = F)


# Add LW to common name
################################################################################

# LW by common name
data2 <- spp_orig %>%
  # Add LW params
  left_join(data %>% select(sciname, family, lw_source, a, b), by="sciname") %>%
  # Simplify
  select(species_id, sciname, lw_source, a, b)

# Export data
write.csv(data2, file=file.path(datadir, "fishbase_lw_parameters_by_species_id.csv"), row.names = F)






