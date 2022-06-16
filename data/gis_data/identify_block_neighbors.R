# Create dataframe which lists block neighbors
# Cori Lopazanski

# Setup
################################################################################
# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(spdep)

# Directories
basedir <- "/Volumes/GoogleDrive-105151121202188525604/Shared drives/NCEAS MPA network assessment/MPA Network Assessment: Working Group Shared Folder/data/sync-data" # Cori Local
datadir <- "/Volumes/GoogleDrive-105151121202188525604/My Drive/Research/NCEAS - California MPA Working Group/fisheries-data"

# Blocks
blocks <- wcfish::blocks

### FYI: Block 1334 has two entries (WA) which creates issues 

# Build Data
################################################################################
# CA blocks only
ca <- blocks %>% 
  filter(block_state == "California")

# Find non-corner neighbors
rook <- st_relate(ca, pattern = "****1****", sparse = FALSE) %>% as.data.frame()
names(rook) = ca$block_id

rook_df <- rook %>% 
  mutate(block_id = ca$block_id) %>% 
  na_if(., "FALSE") %>% 
  pivot_longer(cols = "101":"1032", 
               names_to = "neighbor", 
               values_to = "tf",
               values_drop_na = TRUE) %>% 
  select(block_id, neighbor) %>% 
  filter(!(block_id == neighbor)) %>% 
  mutate(type = "rook")
  
# Find corner neighbors
queen <- st_relate(ca,  pattern = "****0****", sparse = FALSE) %>% as.data.frame()
names(queen) = ca$block_id

queen_df <- queen %>% 
  mutate(block_id = ca$block_id) %>% 
  na_if(., "FALSE") %>% 
  pivot_longer(cols = "101":"1032", 
               names_to = "neighbor", 
               values_to = "tf",
               values_drop_na = TRUE) %>% 
  select(block_id, neighbor) %>% 
  filter(!(block_id == neighbor)) %>% 
  mutate(type = "queen")

# Combine All Neighbors
neighbors <- rbind(rook_df, queen_df) %>% 
  arrange(block_id)


# Export Data
################################################################################
write.csv(neighbors, file.path(datadir, "processed/block_neighbor_id.csv"))
