
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ggplot2)
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data/"
indir <- file.path(basedir, "aquamaps/raw")
outdir <- file.path(basedir, "aquamaps/processed")
codedir <- "data/aquamaps"
plotdir <- "data/aquamaps/figures"

# Read species key
spp_key_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/california/cdfw_data/data/public/cdfw_keys/processed/CDFW_species_key.Rds")

# Source code to Download AquaMaps data
source(file.path(codedir, "download_aquamaps.R"))


# Download AquaMaps data
################################################################################

# Reduce to species-specific
spp_key <- spp_key_orig %>% 
  filter(level=="species")

# Species to do
species_done <- list.files(indir) %>% gsub(".csv", "", .) %>% gsub("_", " ", .)
species_todo <- spp_key$sci_name[!spp_key$sci_name %in% species_done] %>% sort()
species_todo <- "Acanthocybium solandri"

# Download data
# species <- c("Gadus morhua", "Pomacanthus paru"); savedir <- outputdir
download_aquamaps(species=species_todo, savedir=indir, wait_factor=3)



