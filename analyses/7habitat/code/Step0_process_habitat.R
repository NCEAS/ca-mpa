# Process all habitat data for the habitat table
# Cori Lopazanski
# January 2025

# This script follows the intended processing pipeline to sequentially process
# all of the habitat variables for each site (e.g. if sites are updated). Can
# comment/uncomment to run pieces as needed. 

# Step 0. Process monitoring data
# Make sure these have each been properly exported. 
# source("~/ca-mpa/data/monitoring_data/process_2024_versions/Step0_fix_surf_site_names.R")
# source("~/ca-mpa/data/monitoring_data/process_2024_versions/Step0_process_ccfrp.R")
# source("~/ca-mpa/data/monitoring_data/process_2024_versions/Step0_process_deep.R")
# source("~/ca-mpa/data/monitoring_data/process_2024_versions/Step0_process_kelp.R")
# source("~/ca-mpa/data/monitoring_data/process_2024_versions/Step0_process_surf.R")
# source("~/ca-mpa/data/monitoring_data/process_2024_versions/Step1_process_biomass.R")

# Step 1. Process monitoring sites
# Ensure that the sites are correctly updated from the Step0 processing steps 
# for each monitoring dataset. If they are ready to go:
# source("~/ca-mpa/data/monitoring_data/process_2024_versions/Step2_process_monitoring_sites.R")
# source("~/ca-mpa/data/monitoring_data/process_2024_versions/Step3_correct_monitoring_site_locations.R")

# Step 2. Get substrate and biotic characteristics for each site  -----
print("Step 2: Substrate and Biotic")
source("~/ca-mpa/data/habitat_pmep/Step1_export_sites.R")
print("Step 2: Combining Substrate and Biotic")
source("~/ca-mpa/data/habitat_pmep/Step2_combine_habitat.R")

# After this, Steps 3-4 can proceed simultaneously if needed:

# Step 3. Calculate the substrate/biotic buffers  -----
print("Step 3: Calculating Buffers")
source("~/ca-mpa/data/habitat_pmep/Step3_calculate_buffers.R")

# Step 4. Get depth for each site  -----
# Beware: Running this for all 2k+ sites takes a long time. Like 10 hours. 
print("Step 4: Processing Bathymetry")
source("~/ca-mpa/analyses/7habitat/code/Step0_process_bathy.R")

# Anytime after Step 1:

# Step 5. Get annual kelp canopy estimates for each site (anytime after Step 1) -----
print("Step 5: Kelp Canopy")
source("~/ca-mpa/data/kelpwatch/Step1_export_kelpwatch_data.R")
source("~/ca-mpa/data/kelpwatch/Step2_calculate_kelpwatch_buffers.R")



# Congrats. Now you're ready for Step_1_build_habitat_table.



