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
  geom_path(aes(x = year, y = total_landings_lbs, color = block_treatment)) +
  theme_bw()+
  labs(y = "Total Landings (lbs)") +
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
  geom_path(aes(x = year, y = total_landings_usd, color = block_treatment)) +
  labs(color = "MPA Block = 1\nControl Block = 0")
  

ggplot(data = lobster) +
  geom_path(aes(x = year, y = total_landings_lbs, color = block_treatment)) +
  labs(color = "MPA Block = 1\nControl Block = 0") 


# Using Replacement Values -----------------------------------------------------

# Filter to just blocks in our matched dataset
landings_matched2 <- landings %>% 
  filter(block_id %in% matched.1c.all$block_id) %>% 
  left_join(., blocks_simple) %>% 
  mutate(region = case_when(block_lat_dd > 39 ~ "North",
                            between(block_lat_dd, 37.18, 39) ~ "North Central",
                            between(block_lat_dd, 34.5, 37.18) ~ "Central",
                            block_lat_dd < 34.5 ~ "South")) %>% 
  left_join(., matched.1c.all) %>% 
  mutate(block_treatment = as.factor(block_treatment))

# Block totals ----
block2 <- landings_matched2 %>% 
  select(region, year, block_id, block_treatment, landings_lb, block_area_km2) %>% 
  group_by(region, year, block_id, block_treatment) %>% 
  summarize(total_landings_lbs = sum(landings_lb, na.rm = T),
            block_area_km2 = min(block_area_km2)) %>% 
  mutate(landings_lbs_km2 = total_landings_lbs/block_area_km2)

# Totals by region ----
region2 <- landings_matched2 %>% 
  select(region, year, block_id, block_treatment, landings_lb, block_area_km2) %>% 
  group_by(region, year, block_id, block_treatment) %>% 
  summarize(total_landings_lbs = sum(landings_lb, na.rm = T),
            block_area_km2 = min(block_area_km2)) %>% 
  ungroup() %>% 
  group_by(region, year, block_treatment) %>% 
  summarize(avg_landings_lbs = mean(total_landings_lbs)) %>% 
  mutate(block_treatment = if_else(block_treatment == 0, "Non-MPA", "MPA"))


ggplot(data = region2) +
  geom_path(aes(x = year, y = avg_landings_lbs, color = block_treatment)) +
  theme_bw()+
  labs(y = "Total Landings (lbs)",
       color = NULL,
       x = "Year") +
  facet_wrap(~region, scales = "free")

ggplot(data = region) +
  geom_path(aes(x = year, y = total_landings_lbs, color = block_treatment)) +
  facet_wrap(~region, scales = "free")

ggplot(data = block2) +
  geom_point(aes(x = year, y = landings_lbs_km2, color = block_treatment)) +
  geom_smooth(aes(x = year, y = landings_lbs_km2, color = block_treatment, fill = block_treatment)) +
  facet_wrap(~region, scales = "free")
