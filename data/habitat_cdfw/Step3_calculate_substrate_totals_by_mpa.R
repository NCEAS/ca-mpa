

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
indir <- file.path(basedir, "habitat_cdfw/raw")
outdir <- file.path(basedir, "habitat_cdfw/processed")
plotdir <- "data/habitat_cdfw/figures"

# Read data
substrate_orig <- raster::raster(file.path(outdir, "CA_bottom_substrate_10m.tiff")) 

# Get MPAs
mpas <- wcfish::mpas_ca %>% 
  sf::st_transform(crs=raster::crs(substrate_orig))

# Habitats
habitats <- c("Soft (0-30m)", 
              "Soft (30-100m)",
              "Soft (100-200m)",
              "Soft (200-3000m)",
              "Hard (0-30m)",
              "Hard (30-100m)",
              "Hard (100-200m)",
              "Hard (200-3000m)")


# Summarize coverage by polygon
################################################################################

# Extracts cells inside MPAs
mpa_cells <- exactextractr::exact_extract(x=substrate_orig,
                                      y=mpas,
                                      coverage_area=T)

# Calcuate MPA habitat composition
x <- 1
mpa_stats <- purrr::map_df(1:length(mpa_cells), function(x){
  
  # MPA
  mpa_do <- mpas$name[x]
  
  # Process data
  sdata <- mpa_cells[[x]] %>% 
    # Habitat stats
    group_by(value) %>% 
    summarise(ncells=n(),
              area_m2=sum(coverage_area)) %>% 
    ungroup() %>% 
    # Habitat area / prop
    mutate(area_km2=area_m2/(1000^2),
           prop=area_km2/sum(area_km2)) %>% 
    # Add MPA and habitat
    mutate(mpa=mpa_do,
           habitat=habitats[value],
           habitat=ifelse(is.na(value), "Unmapped", habitat)) %>% 
    # Arrange
    select(-value) %>% 
    select(mpa, habitat, everything())
  
  
})

# Export
################################################################################

# Export
saveRDS(mpa_stats, file=file.path(outdir, "bottom_substrate_by_mpa.Rds"))





