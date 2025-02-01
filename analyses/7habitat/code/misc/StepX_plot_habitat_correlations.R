

# Cori Lopazanski; lopazanski@bren.ucsb.edu
# July 2024 


# Setup   ----------------------------------------------------------------------
rm(list = ls())
gc()

library(tidyverse)
library(janitor)
library(sf)
library(dplyr)
library(purrr)
library(stringr)
library(corrr)

fig.dir <- "~/ca-mpa/analyses/7habitat/figures"
hab.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/combined/combined_mlpa_sites_1000m"
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data"
sp.dir <- "/home/shares/ca-mpa/data/sync-data/species_traits/processed"
int.dir <- "~/ca-mpa/analyses/7habitat/intermediate_data"


# Read  ----------------------------------------------------------------------
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"

pred_kelp <- readRDS(file.path("analyses/7habitat/intermediate_data/kelp_predictors.Rds")) #%>% map(~ .x[.x != "site_type * age_at_survey"])
pred_surf <- readRDS(file.path("analyses/7habitat/intermediate_data/surf_predictors.Rds")) 
pred_rock <- readRDS(file.path("analyses/7habitat/intermediate_data/rock_predictors.Rds"))


data_kelp <- readRDS(file.path(ltm.dir, "combine_tables/kelp_combine_table.Rds")) %>% 
  dplyr::select(year:size_km2, pred_kelp$predictor) 

data_surf <- readRDS(file.path(ltm.dir, "combine_tables/surf_combine_table.Rds")) %>% 
  dplyr::select(year:size_km2, pred_surf$predictor) 

data_rock <- readRDS(file.path(ltm.dir, "combine_tables/ccfrp_combine_table.Rds")) %>% 
  dplyr::select(year:size_km2, pred_rock$predictor)

correlation_matrix <- data_surf %>%
  dplyr::select(!c(year, size_km2, implementation_year)) %>% 
  correlate() %>% 
  rearrange()

rplot(correlation_matrix %>% 
        shave()) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


cor_long <- correlation_matrix %>%
  stretch() %>%
  rename(Var1 = x, Var2 = y, value = r) %>% 
  mutate(buffer1 = sub(".*_", "", Var1),
         buffer2 = sub(".*_", "", Var2),
         habitat1 = sub("_[^_]*$", "", Var1),
         habitat2 = sub("_[^_]*$", "", Var2)) %>% 
  filter(buffer1 == buffer2) %>%
  mutate(buffer = as.numeric(buffer1)) %>%
  filter(as.character(Var1) <= as.character(Var2)) %>%
  filter(!(Var1 == Var2)) %>% 
  select(-buffer1, -buffer2) 

# Plot correlations using ggplot2 and facet by buffer
ggplot(data = cor_long, aes(x = habitat1, y = habitat2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Pearson\nCorrelation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 65, vjust = 1, 
                                   size = 10, hjust = 1)) +
  labs(x = NULL,
       y = NULL) +
  facet_wrap(~ buffer, scales = "free")


