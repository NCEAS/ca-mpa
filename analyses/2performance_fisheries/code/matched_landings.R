# Try some proof-of-concept landing results with the matched data


# Setup --------------------------------------------------------------------------
## Packages ----
library(tidyverse)

## Directories ----
basedir <- "/Volumes/GoogleDrive-105151121202188525604/Shared drives/NCEAS MPA network assessment/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
datadir <- "/Volumes/GoogleDrive-105151121202188525604/My Drive/Research/NCEAS - California MPA Working Group/fisheries-data/raw"
plotdir <- "analyses/2performance_fisheries/figures"
outdir <- "/Volumes/GoogleDrive-105151121202188525604/My Drive/Research/NCEAS - California MPA Working Group/fisheries-data/processed"

## Read Data ----
landings <- readRDS(file.path(datadir, "CDFW_2000_2020_landings_receipts.Rds"))
matched.2c.all <- readRDS(file.path(getwd(), "analyses", "2performance_fisheries",
                                    "analyses", "blocks", 
                                    "block_counterfactual_key.Rds"))

blocks <- wcfish::blocks
blocks_simple <- blocks %>% sf::st_drop_geometry()


# Build --------------------------------------------------------------------------

# Filter to just blocks in our matched dataset
landings_matched <- landings %>% 
  filter(block_id %in% matched.2c.all$block_id) %>% 
  left_join(., blocks_simple) %>% 
  mutate(region = case_when(block_lat_dd > 39 ~ "North",
                            between(block_lat_dd, 37.18, 39) ~ "North Central",
                            between(block_lat_dd, 34.5, 37.18) ~ "Central",
                            block_lat_dd < 34.5 ~ "South")) %>% 
  left_join(., matched.2c.all) %>% 
  mutate(block_treatment = as.factor(block_treatment))

# Totals by region ----
region <- landings_matched %>% 
  group_by(region, year, block_treatment) %>% 
  summarize(total_landings_usd = sum(value_usd, na.rm = T),
            total_landings_lbs = sum(landings_lb, na.rm = T))


ggplot(data = region) +
  geom_path(aes(x = year, y = total_landings_usd, color = block_treatment)) +
  facet_wrap(~region, scales = "free")

ggplot(data = region) +
  geom_path(aes(x = year, y = total_landings_lbs, color = block_treatment)) +
  facet_wrap(~region, scales = "free")


# Totals for lobster because why not
lobster <- landings_matched %>% 
  filter(species_id == 820 & region == "South") %>% 
  group_by(region, year, block_treatment) %>% 
  summarize(total_landings_usd = sum(value_usd, na.rm = T),
            total_landings_lbs = sum(landings_lb, na.rm = T))

ggplot(data = lobster) +
  geom_path(aes(x = year, y = total_landings_usd, color = block_treatment)) 

ggplot(data = lobster) +
  geom_path(aes(x = year, y = total_landings_lbs, color = block_treatment)) 


