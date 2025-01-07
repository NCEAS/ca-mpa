# Process all habitat data for the habitat table
# Cori Lopazanski
# January 2025

# This script follows the intended processing pipeline to sequentially process
# all of the habitat variables for each site (e.g. if sites are updated).

# Step 1. Process monitoring sites
# Ensure that the sites are correctly updated from the Step0 processing steps 
# for each monitoring dataset. If they are ready to go:
# source("~/ca-mpa/data/monitoring_data/processing_code/process_2024_versions/Step2_process_monitoring_sites.R")
# source("~/ca-mpa/data/monitoring_data/processing_code/process_2024_versions/Step3_correct_monitoring_site_locations.R")

# Step 2. Get substrate and biotic characteristics for each site
source("~/ca-mpa/data/habitat_pmep/Step1_export_sites.R")
source("~/ca-mpa/data/habitat_pmep/Step2_combine_habitat.R")
source("~/ca-mpa/data/habitat_pmep/Step3_caclulate_buffers.R")

# Step 3. Get annual kelp canopy estimates for each site
source("~/ca-mpa/data/kelpwatch/Step1_export_kelpwatch_data.R")
source("~/ca-mpa/data/kelpwatch/Step2_calculate_kelpwatch_buffers.R")

# Step 4. Get depth for each site
source("~/ca-mpa/analyses/7habitat/code/Step0_process_bathy.R")

