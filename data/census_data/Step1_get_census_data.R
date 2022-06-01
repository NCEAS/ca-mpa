
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)
library(tidycensus)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data/"
indir <- file.path(basedir, "census_data/raw")
outdir <- file.path(basedir, "census_data/processed")
plotdir <- "data/census_data/figures"

# Package
# https://walker-data.com/tidycensus/articles/basic-usage.html

# Set API key
tidycensus::census_api_key(key="974c34380d8224622c765f34050e53a382511e33", install=T, overwrite = T)


# Geta block-level data
################################################################################

# Get total population by census block
ca_pop_2010 <- get_decennial(geography = "block",
                             variables = "P001001",
                             state="California",
                             year = 2010,
                             geometry = TRUE)

head(ca_pop_2010)

# Format block-level data
pop_data <- ca_pop_2010 %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  # Format npeople
  mutate(value=as.numeric(value)) %>% 
  rename(npeople=value) %>% 
  # Break census tracks apart
  separate(name, into=c("block", "block_group", "tract", "county", "state"), sep=", ", remove=T) %>% 
  # Format state, county, tract
  select(-state) %>% 
  mutate(county=gsub("County ", "", county),
         tract=gsub("Census Tract ", "", tract),
         block_group=gsub("Block Group ", "", block_group),
         block=gsub("Block ", "", block)) %>% 
  # Add area
  mutate(area_sqkm=sf::st_area(.) %>% as.numeric() %>% measurements::conv_unit(., from="m2", to="km2")) %>% 
  # Calculate density
  mutate(people_sqkm=npeople/area_sqkm) %>% 
  # Arrange
  select(geoid, county, tract, block_group, block, area_sqkm, npeople, people_sqkm, everything()) %>% 
  select(-variable)

# Inspect 
str(pop_data)
freeR::complete(pop_data)

# Export data
saveRDS(pop_data, file=file.path(outdir, "CA_2010_census_tot_pop_by_block.Rds"))


# Get tract-level data
################################################################################

# Get total population by census block
ca_pop_2010_tract <- get_decennial(geography = "tract",
                                   variables = "P001001",
                                   state="California",
                                   year = 2010,
                                   geometry = TRUE)

head(ca_pop_2010_tract)

# Format tract-level census data
pop_data_tract <- ca_pop_2010_tract %>%
  # Rename
  janitor::clean_names("snake") %>%
  # Format npeople
  mutate(value=as.numeric(value)) %>%
  rename(npeople=value) %>%
  # Break census tracks apart
  separate(name, into=c("tract", "county", "state"), sep=", ", remove=T) %>%
  # Format state, county, tract
  select(-state) %>%
  mutate(county=gsub("County ", "", county),
         tract=gsub("Census Tract ", "", tract)) %>%
  # Add area
  mutate(area_sqkm=sf::st_area(.) %>% as.numeric() %>% measurements::conv_unit(., from="m2", to="km2")) %>%
  # Calculate density
  mutate(people_sqkm=npeople/area_sqkm) %>%
  # Arrange
  select(geoid, county, tract, area_sqkm, npeople, people_sqkm, everything()) %>%
  select(-variable)

# Plot data
g <- ggplot() +
  # Plot census tracts
  geom_sf(data=pop_data_tract, mapping=aes(fill=people_sqkm), color=NA) +
  # Legend
  scale_fill_gradientn(name="Population density\n(people/sqkm)", colors=RColorBrewer::brewer.pal(9, "YlOrRd"), 
                       trans="log10", breaks=c(0.1, 1, 10, 100, 1000, 10000)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw()
g

# Export data
saveRDS(pop_data_tract, file=file.path(outdir, "CA_2010_census_tot_pop_by_tract.Rds"))

