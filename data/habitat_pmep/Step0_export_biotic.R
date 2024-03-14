# Explore & Process PMEP Habitat Data (Biotic Component)
# Cori Lopazanski
# November 2023

# About --------------------------------------------------------------------------------
# Read and clean the Pacific Marine and Estuary Partnership data

# Columns in full data set:
#     "PMEP_Section"                "PMEP_Zone"                   "CMECS_BC_Code"               
#     "CMECS_BC_Name"               "CMECS_BC_Modifier"           "StateWaters"                 
#     "State"                       "CMECS_BC_Category"           "CMECS_BC_Category_Code"      
#     "CMECS_BC_Cartography_Detail" "PMEP_Region"                 "CMECS_BC_Cartography"       
#     "FaunalBed"                   "AquaticVegetationBed"        "BenthicMacroalgae"           
#     "Kelp"                        "OtherMacroalgae"             "EmergentWetland"             
#     "ScrubShrubWetland"           "ForestedWetland"             "Seagrass"                    
#     "AquaticVascularVegetation"   "FloatingSuspendedBiota"      "Link"                       
#     "PMEP_NSID"                   "NS_PolyID"       "Shape_Length"    "Shape_Area"  "Shape"                                        


# Setup --------------------------------------------------------------------------------
rm(list=ls())

# Load required packages
library(tidyverse)
library(sf)
library(terra)
library(gdalUtils)

# Directories
#gdb.dir <- "/Users/lopazanski/Documents/habitat/PMEP/PMEP_Nearshore_Zones_and_Habitat.gdb" # Local
gdb.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/PMEP_Nearshore_Zones_and_Habitat.gdb" # Aurora
out.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed"

# Read Data ----------------------------------------------------------------------------
# Check layer names
st_layers(dsn=gdb.dir)

# Export Attribute Table ----------------------------------------------------------------------------
biotic_full_table <- terra::vect(gdb.dir, layer = 'West_Coast_USA_Nearshore_CMECS_Biotic_Habitat', 
                                 what = "attributes")

saveRDS(biotic_full_table, file.path(out.dir, "biotic", "West_Coast_USA_Nearshore_CMECS_Biotic_Habitat_Attributes.Rds"))


# Export CA subset -------------------------------------------------------------------
# The subset of the biotic component that is within CA is 274/2045 observations
# to ease processing will use ogr2ogr to save this subset, using only the NS_PolyID
ogr2ogr(src_datasource_name = gdb.dir,
        dst_datasource_name = file.path(out.dir, 'biotic', 'biotic_ca'),
        select = 'NS_PolyID',
        layer = 'West_Coast_USA_Nearshore_CMECS_Biotic_Habitat',
        where = "State='CA'",
        nlt = 'PROMOTE_TO_MULTI')

biotic <- read_sf(dsn = file.path(out.dir,"biotic", "biotic_ca"), 
                  layer = 'West_Coast_USA_Nearshore_CMECS_Biotic_Habitat')
