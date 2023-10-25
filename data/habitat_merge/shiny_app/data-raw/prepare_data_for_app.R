
# Clear
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
basedir <- "/Users/cfree/Library/CloudStorage/GoogleDrive-cfree@ucsb.edu/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
indir <- file.path(basedir, "habitat_merge")
outdir <- "data/habitat_merge/shiny_app/data"

# Read data
data_wide <- readRDS(file.path(indir, "bottom_substrate_by_mpa_wide.Rds"))
data_long <- readRDS(file.path(indir, "bottom_substrate_by_mpa_long.Rds"))
data_orig <- raster::raster(file.path(indir, "CA_bottom_substrate_10m.tif"))

# Export data
saveRDS(data_wide, file=file.path(outdir, "bottom_substrate_by_mpa_wide.Rds"))
saveRDS(data_long, file=file.path(outdir, "bottom_substrate_by_mpa_long.Rds"))

# Projection
nad83_utm <- "+proj=aea +lat_0=0 +lon_0=-120 +lat_1=34 +lat_2=40.5 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +no_defs"

# Get MPAs
mpas_orig <- wcfish::mpas_ca %>% 
  sf::st_transform(crs=nad83_utm)
saveRDS(mpas_orig, file=file.path(outdir, "ca_mpas.Rds"))

# Get land
land <- rnaturalearth::ne_countries(country=c("United States of America", "Mexico"),
                                    scale="large", returnclass = "sf") %>% 
  sf::st_transform(crs=nad83_utm)
saveRDS(land, file=file.path(outdir, "land.Rds"))




# Loop through MPAs and create rasters
################################################################################

# Loop through MPAs
i <- 84
data_out <- purrr::map_df(1:nrow(mpas_orig), function(x){
  
  # MPA
  mpa_name <- mpas_orig$name[x]
  mpa_do <- mpas_orig %>% 
    filter(name==mpa_name)
  
  # Bounding box
  mpa_bbox <- sf::st_bbox(mpa_do)
  mpa_bbox_exp <- mpa_bbox
  
  # Make extent
  mpa_extent <- raster::extent(c(mpa_bbox_exp$xmin,
                               mpa_bbox_exp$xmax,
                               mpa_bbox_exp$ymin,
                               mpa_bbox_exp$ymax))
  
  # Clip raster by bounding box
  mpa_raster <- raster::crop(x=data_orig, y=mpa_extent)
  
  # Convert to xy
  mpa_raster_xy <- mpa_raster %>% 
    # Convert to raster
    raster::as.data.frame(xy=T) %>% 
    # Remove unmapped areas
    rename(substrate=Band_1) %>% 
    filter(!is.na(substrate)) %>% 
    # Format substrate
    mutate(substrate=case_when(substrate==0 ~ "Soft",
                               substrate==1 ~ "Hard")) %>% 
    mutate(substrate=factor(substrate, 
                            levels=c("Soft", "Hard"))) %>% 
    # Add MPA
    mutate(mpa=mpa_name) %>% 
    select(mpa, everything())
  
  # Plot check
  if(F){
    
    # Plot 
    ggplot() +
      # Raster
      geom_tile(data=mpa_raster_xy, mapping=aes(x=x, y=y, fill=substrate)) + 
      # Land
      geom_sf(data=land, color="white", fill="grey70") +
      # MPA
      geom_sf(data=mpa_do, fill=NA, color="black") +
      # Crop
      coord_sf(xlim=mpa_bbox_exp[c(1,3)], ylim=mpa_bbox_exp[c(2,4)]) +
      # Labels
      labs(x="", y="", title=mpa_name) +
      # Legend
      scale_fill_manual(name="", values=c("yellow", "brown"), drop=F) +
      # Theme
      theme_bw()
    
  }
  
  # Return
  mpa_raster_xy
  
})

# Export
saveRDS(data_out, file=file.path(outdir, "mpa_substrate_rasters.Rds"))

