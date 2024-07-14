# Export PMEP Habitat Data (Substrate Component)
# Cori Lopazanski
# January 2024

# About --------------------------------------------------------------------------------
# Read and clean the Pacific Marine and Estuary Partnership data

# Setup --------------------------------------------------------------------------------
rm(list=ls())

# Load required packages
library(tidyverse)
library(gdalUtils)
library(gdalUtilities)
library(sf)

# Directories
sync.dir <- "/home/shares/ca-mpa/data/sync-data"
out.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/substrate"
gdb.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Nearshore_Zones_and_Habitat.gdb" # Aurora


# Export Attribute Table -------------------------------------------------------------
# Check layer names
st_layers(dsn=gdb.dir)

# Read table without geometry
sub_table <- terra::vect(gdb.dir, layer = 'West_Coast_USA_Nearshore_CMECS_Substrate_Habitat', 
                                 what = "attributes")

# Save
saveRDS(sub_table, "West_Coast_USA_Nearshore_CMECS_Substrate_Habitat_Attributes.Rds")


# Export CA subset -------------------------------------------------------------------
# The subset of the substrate component that is within CA 
# to ease processing will use ogr2ogr to save this subset, using only the NS_PolyID
ogr2ogr(src_datasource_name = gdb.dir,
        dst_datasource_name = file.path(out.dir, 'substrate_ca'),
        select = 'NS_PolyID',
        layer = 'West_Coast_USA_Nearshore_CMECS_Substrate_Habitat',
        where = "State='CA'",
        nlt = 'PROMOTE_TO_MULTI')


# Export Sections ------

# Read exported attribute table
att <- readRDS(file.path(out.dir, "West_Coast_USA_Nearshore_CMECS_Substrate_Habitat_Attributes.Rds"))

# Get list of unique sections
pmep_sections <- unique(att$PMEP_Section[att$State=='CA'])

# Run the following in the command line:
# ogr2ogr -f "GPKG" /home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/substrate/substrate_ca/sections/substrate_section_23.gpkg /home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Nearshore_Zones_and_Habitat.gdb West_Coast_USA_Nearshore_CMECS_Substrate_Habitat -where "State='CA' AND PMEP_Section='23'" --config OGR_ORGANIZE_POLYGONS CCW_INNER_JUST_AFTER_CW_OUTER
# ogr2ogr -f "GPKG" /home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/substrate/substrate_ca/sections/substrate_section_52.gpkg /home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Nearshore_Zones_and_Habitat.gdb West_Coast_USA_Nearshore_CMECS_Substrate_Habitat -where "State='CA' AND PMEP_Section='52'" --config OGR_ORGANIZE_POLYGONS CCW_INNER_JUST_AFTER_CW_OUTER
# ogr2ogr -f "GPKG" /home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/substrate/substrate_ca/sections/substrate_section_30.gpkg /home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Nearshore_Zones_and_Habitat.gdb West_Coast_USA_Nearshore_CMECS_Substrate_Habitat -where "State='CA' AND PMEP_Section='30'" --config OGR_ORGANIZE_POLYGONS CCW_INNER_JUST_AFTER_CW_OUTER
# ogr2ogr -f "GPKG" /home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/substrate/substrate_ca/sections/substrate_section_31.gpkg /home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Nearshore_Zones_and_Habitat.gdb West_Coast_USA_Nearshore_CMECS_Substrate_Habitat -where "State='CA' AND PMEP_Section='31'" --config OGR_ORGANIZE_POLYGONS CCW_INNER_JUST_AFTER_CW_OUTER
# ogr2ogr -f "GPKG" /home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/substrate/substrate_ca/sections/substrate_section_32.gpkg /home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Nearshore_Zones_and_Habitat.gdb West_Coast_USA_Nearshore_CMECS_Substrate_Habitat -where "State='CA' AND PMEP_Section='32'" --config OGR_ORGANIZE_POLYGONS CCW_INNER_JUST_AFTER_CW_OUTER
# ogr2ogr -f "GPKG" /home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/substrate/substrate_ca/sections/substrate_section_33.gpkg /home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Nearshore_Zones_and_Habitat.gdb West_Coast_USA_Nearshore_CMECS_Substrate_Habitat -where "State='CA' AND PMEP_Section='33'" --config OGR_ORGANIZE_POLYGONS CCW_INNER_JUST_AFTER_CW_OUTER
# ogr2ogr -f "GPKG" /home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/substrate/substrate_ca/sections/substrate_section_53.gpkg /home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Nearshore_Zones_and_Habitat.gdb West_Coast_USA_Nearshore_CMECS_Substrate_Habitat -where "State='CA' AND PMEP_Section='53'" --config OGR_ORGANIZE_POLYGONS CCW_INNER_JUST_AFTER_CW_OUTER
# ogr2ogr -f "GPKG" /home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/substrate/substrate_ca/sections/substrate_section_40.gpkg /home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Nearshore_Zones_and_Habitat.gdb West_Coast_USA_Nearshore_CMECS_Substrate_Habitat -where "State='CA' AND PMEP_Section='40'" --config OGR_ORGANIZE_POLYGONS CCW_INNER_JUST_AFTER_CW_OUTER
# ogr2ogr -f "GPKG" /home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/substrate/substrate_ca/sections/substrate_section_41.gpkg /home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Nearshore_Zones_and_Habitat.gdb West_Coast_USA_Nearshore_CMECS_Substrate_Habitat -where "State='CA' AND PMEP_Section='41'" --config OGR_ORGANIZE_POLYGONS CCW_INNER_JUST_AFTER_CW_OUTER
# ogr2ogr -f "GPKG" /home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/substrate/substrate_ca/sections/substrate_section_50.gpkg /home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Nearshore_Zones_and_Habitat.gdb West_Coast_USA_Nearshore_CMECS_Substrate_Habitat -where "State='CA' AND PMEP_Section='50'" --config OGR_ORGANIZE_POLYGONS CCW_INNER_JUST_AFTER_CW_OUTER



# Subset to only the data within MPAs ------------------------------------------

## Create raster object of the substrate layer
#substrate_raster <- raster::raster(substrate_ca) # clearly this wouldn't work

# I bet this (below) was using the loaded substrate data (which took 8+ hours) rather
# than the exported shapefiles? Unsure where the errors have come up

# Use st_intersection to create a subset of only those data within MPAs
mpa_intersect <- st_intersection(substrate_ca, mpas)
saveRDS(mpa_intersect, file.path(sync.dir, "habitat_pmep/processed", "mpa_substrate_intersection.Rds"))

# This approach ends up splits polygons based on MPA borders (e.g. greater number 
# of observations because adjacent MPAs will split one poly into two)
mpa_intersect_simple <- mpa_intersect %>% st_drop_geometry()

obs_per_mpa <- mpa_intersect_simple %>% 
  group_by(name) %>% 
  summarize(n_obs = n())

no_substrate <- mpas_simple %>% 
  filter(!(name %in% obs_per_mpa$name))


# Calculate area of each polygon within MPAs
mpa_intersect$mpa_area <- st_area(mpa_intersect)

mpa_intersect_simple <- mpa_intersect %>% st_drop_geometry()

mpa_totals <- mpa_intersect_simple %>% 
  mutate(CMECS_SC_Broad = if_else(CMECS_SC_Category_Code < 1.5,
                                  str_extract(CMECS_SC_Category_Code, "^.{3}"), CMECS_SC_Category_Code)) %>% 
  group_by(CMECS_SC_Broad) %>% 
  summarize(mpa_area = sum(mpa_area),
            mpa_area_km = round(mpa_area/(1*10^6), 3))

state_totals <- substrate_ca_simple %>% 
  mutate(CMECS_SC_Broad = if_else(CMECS_SC_Category_Code < 1.5, 
                                  str_extract(CMECS_SC_Category_Code, "^.{3}"), CMECS_SC_Category_Code)) %>% 
  group_by(CMECS_SC_Broad) %>% 
  summarize(state_area = sum(Shape_Area),
            state_area_km = round(state_area/(1e6), 3))

representation <- full_join(mpa_totals, state_totals) %>% 
  mutate(proportion = mpa_area/state_area) 


library(RColorBrewer)
hab_colors <- c("red4", # anthro
                "burlywood3", "burlywood2", #coarse, fine
                "tan4", #rock
                "white", #unclassified"
                "burlywood1") #unconsolidated





