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

# Directories
out.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/substrate"
gdb.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/PMEP_Nearshore_Zones_and_Habitat.gdb" # Aurora


# Export Attribute Table -------------------------------------------------------------
# Check layer names
st_layers(dsn=gdb.dir)

# Read table without geometry
sub_table <- terra::vect(gdb.dir, layer = 'West_Coast_USA_Nearshore_CMECS_Substrate_Habitat', 
                                 what = "attributes")
beepr::beep()

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


