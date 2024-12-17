# Export PMEP Habitat Data (Substrate Component)
# Cori Lopazanski
# January 2024

# About --------------------------------------------------------------------------------
# Read and clean the Pacific Marine and Estuary Partnership data

# Setup --------------------------------------------------------------------------------
rm(list=ls())
gc()

# Load required packages
library(tidyverse)
library(gdalUtilities)
library(sf)

# Directories
sync.dir <- "/home/shares/ca-mpa/data/sync-data"
out.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed_v2/substrate"
gdb.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Nearshore_Zones_and_Habitat_V2.gdb" # Aurora
#gdb.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Nearshore_Zones_and_Habitat.gdb" # Aurora


# Export Attribute Table -------------------------------------------------------------
# Check layer names
st_layers(dsn=gdb.dir)

# Read table without geometry
sub_table <- terra::vect(gdb.dir, layer = 'West_Coast_USA_Nearshore_CMECS_Substrate_Habitat_V2', 
                                 what = "attributes")

# Save
saveRDS(sub_table, "West_Coast_USA_Nearshore_CMECS_Substrate_Habitat_Attributes.Rds")


# Export Sections from V1 ------

# Read exported attribute table
att <- readRDS(file.path(out.dir, "West_Coast_USA_Nearshore_CMECS_Substrate_Habitat_Attributes.Rds"))

# Get list of unique sections
pmep_sections <- unique(att$PMEP_Section[att$State=='CA'])
pmep_sections

# Run the following in the command line:
# ogr2ogr -f "GPKG" /home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/substrate/substrate_ca/sections/substrate_section_52.gpkg /home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Nearshore_Zones_and_Habitat.gdb West_Coast_USA_Nearshore_CMECS_Substrate_Habitat -where "State='CA' AND PMEP_Section='52'" --config OGR_ORGANIZE_POLYGONS CCW_INNER_JUST_AFTER_CW_OUTER
# ogr2ogr -f "GPKG" /home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/substrate/substrate_ca/sections/substrate_section_30.gpkg /home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Nearshore_Zones_and_Habitat.gdb West_Coast_USA_Nearshore_CMECS_Substrate_Habitat -where "State='CA' AND PMEP_Section='30'" --config OGR_ORGANIZE_POLYGONS CCW_INNER_JUST_AFTER_CW_OUTER
# ogr2ogr -f "GPKG" /home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/substrate/substrate_ca/sections/substrate_section_31.gpkg /home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Nearshore_Zones_and_Habitat.gdb West_Coast_USA_Nearshore_CMECS_Substrate_Habitat -where "State='CA' AND PMEP_Section='31'" --config OGR_ORGANIZE_POLYGONS CCW_INNER_JUST_AFTER_CW_OUTER
# ogr2ogr -f "GPKG" /home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/substrate/substrate_ca/sections/substrate_section_32.gpkg /home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Nearshore_Zones_and_Habitat.gdb West_Coast_USA_Nearshore_CMECS_Substrate_Habitat -where "State='CA' AND PMEP_Section='32'" --config OGR_ORGANIZE_POLYGONS CCW_INNER_JUST_AFTER_CW_OUTER
# ogr2ogr -f "GPKG" /home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/substrate/substrate_ca/sections/substrate_section_33.gpkg /home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Nearshore_Zones_and_Habitat.gdb West_Coast_USA_Nearshore_CMECS_Substrate_Habitat -where "State='CA' AND PMEP_Section='33'" --config OGR_ORGANIZE_POLYGONS CCW_INNER_JUST_AFTER_CW_OUTER
# ogr2ogr -f "GPKG" /home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/substrate/substrate_ca/sections/substrate_section_53.gpkg /home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Nearshore_Zones_and_Habitat.gdb West_Coast_USA_Nearshore_CMECS_Substrate_Habitat -where "State='CA' AND PMEP_Section='53'" --config OGR_ORGANIZE_POLYGONS CCW_INNER_JUST_AFTER_CW_OUTER
# ogr2ogr -f "GPKG" /home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/substrate/substrate_ca/sections/substrate_section_40.gpkg /home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Nearshore_Zones_and_Habitat.gdb West_Coast_USA_Nearshore_CMECS_Substrate_Habitat -where "State='CA' AND PMEP_Section='40'" --config OGR_ORGANIZE_POLYGONS CCW_INNER_JUST_AFTER_CW_OUTER
# ogr2ogr -f "GPKG" /home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/substrate/substrate_ca/sections/substrate_section_41.gpkg /home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Nearshore_Zones_and_Habitat.gdb West_Coast_USA_Nearshore_CMECS_Substrate_Habitat -where "State='CA' AND PMEP_Section='41'" --config OGR_ORGANIZE_POLYGONS CCW_INNER_JUST_AFTER_CW_OUTER
# ogr2ogr -f "GPKG" /home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/substrate/substrate_ca/sections/substrate_section_50.gpkg /home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Nearshore_Zones_and_Habitat.gdb West_Coast_USA_Nearshore_CMECS_Substrate_Habitat -where "State='CA' AND PMEP_Section='50'" --config OGR_ORGANIZE_POLYGONS CCW_INNER_JUST_AFTER_CW_OUTER
# ogr2ogr -f "GPKG" /home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/substrate/substrate_ca/sections/substrate_section_23.gpkg /home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Nearshore_Zones_and_Habitat.gdb West_Coast_USA_Nearshore_CMECS_Substrate_Habitat -where "PMEP_Section='23'" --config OGR_ORGANIZE_POLYGONS CCW_INNER_JUST_AFTER_CW_OUTER


# Export from V2 ------

# No longer using the numbers for the sections, now they use the character descriptors
# Read exported attribute table
att <- readRDS(file.path(out.dir, "West_Coast_USA_Nearshore_CMECS_Substrate_Habitat_Attributes.Rds"))

# ogr2ogr -f "GPKG" /home/shares/ca-mpa/data/sync-data/habitat_pmep/processed_v2/substrate/substrate_ca/sections/substrate_section_30.gpkg /home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Nearshore_Zones_and_Habitat_V2.gdb West_Coast_USA_Nearshore_CMECS_Substrate_Habitat_v2 -where "State='CA' AND PMEP_Section='Cape Mendocino to Point Reyes'" --config OGR_ORGANIZE_POLYGONS CCW_INNER_JUST_AFTER_CW_OUTER
# ogr2ogr -f "GPKG" /home/shares/ca-mpa/data/sync-data/habitat_pmep/processed_v2/substrate/substrate_ca/sections/substrate_section_31.gpkg /home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Nearshore_Zones_and_Habitat_V2.gdb West_Coast_USA_Nearshore_CMECS_Substrate_Habitat_v2 -where "State='CA' AND PMEP_Section='Point Reyes to Point Sur'" --config OGR_ORGANIZE_POLYGONS CCW_INNER_JUST_AFTER_CW_OUTER
# ogr2ogr -f "GPKG" /home/shares/ca-mpa/data/sync-data/habitat_pmep/processed_v2/substrate/substrate_ca/sections/substrate_section_32.gpkg /home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Nearshore_Zones_and_Habitat_V2.gdb West_Coast_USA_Nearshore_CMECS_Substrate_Habitat_v2 -where "State='CA' AND PMEP_Section='Point Sur to Point Arguello including Davidson Seamount'" --config OGR_ORGANIZE_POLYGONS CCW_INNER_JUST_AFTER_CW_OUTER
# ogr2ogr -f "GPKG" /home/shares/ca-mpa/data/sync-data/habitat_pmep/processed_v2/substrate/substrate_ca/sections/substrate_section_33.gpkg /home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Nearshore_Zones_and_Habitat_V2.gdb West_Coast_USA_Nearshore_CMECS_Substrate_Habitat_v2 -where "State='CA' AND PMEP_Section='Point Arguello South including San Juan Seamount'" --config OGR_ORGANIZE_POLYGONS CCW_INNER_JUST_AFTER_CW_OUTER
# ogr2ogr -f "GPKG" /home/shares/ca-mpa/data/sync-data/habitat_pmep/processed_v2/substrate/substrate_ca/sections/substrate_section_40.gpkg /home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Nearshore_Zones_and_Habitat_V2.gdb West_Coast_USA_Nearshore_CMECS_Substrate_Habitat_v2 -where "State='CA' AND PMEP_Section='Point Conception to Palos Verdes'" --config OGR_ORGANIZE_POLYGONS CCW_INNER_JUST_AFTER_CW_OUTER
# ogr2ogr -f "GPKG" /home/shares/ca-mpa/data/sync-data/habitat_pmep/processed_v2/substrate/substrate_ca/sections/substrate_section_41.gpkg /home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Nearshore_Zones_and_Habitat_V2.gdb West_Coast_USA_Nearshore_CMECS_Substrate_Habitat_v2 -where "State='CA' AND PMEP_Section='Palos Verdes to US-Mex Border'" --config OGR_ORGANIZE_POLYGONS CCW_INNER_JUST_AFTER_CW_OUTER
# ogr2ogr -f "GPKG" /home/shares/ca-mpa/data/sync-data/habitat_pmep/processed_v2/substrate/substrate_ca/sections/substrate_section_23.gpkg /home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Nearshore_Zones_and_Habitat_V2.gdb West_Coast_USA_Nearshore_CMECS_Substrate_Habitat_v2 -where "PMEP_Section='Cape Mendocino to Cape Blanco'" --config OGR_ORGANIZE_POLYGONS CCW_INNER_JUST_AFTER_CW_OUTER

# Confirm the right sections are exported 
# In V2 the "Watershed (Landward Zone)" and "Seaward of Section Boundaries or International Waters" have been separated
# Could be that the seaward one is necessary for deep... but probably not! Will omit for now.

att %>% 
  filter(State == "CA") %>% 
  distinct(PMEP_Section)

# ogr2ogr -f "GPKG" /home/shares/ca-mpa/data/sync-data/habitat_pmep/processed_v2/substrate/substrate_ca/sections/substrate_section_32.gpkg /home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Nearshore_Zones_and_Habitat_V2.gdb West_Coast_USA_Nearshore_CMECS_Substrate_Habitat_v2 -where "State='CA' AND PMEP_Section='Point Sur to Point Arguello'" --config OGR_ORGANIZE_POLYGONS CCW_INNER_JUST_AFTER_CW_OUTER
