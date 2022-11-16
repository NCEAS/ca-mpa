

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(rinat)
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data/"
indir <- file.path(basedir, "ebird/intermediate")
outdir <- file.path(basedir, "ebird/intermediate_clip")
gisdir <- file.path(basedir, "gis_data/processed")
plotdir <- "data/ebird/figures"

# Read state waters
state_waters_sf <- readRDS(file.path(gisdir, "CA_state_waters_5km_buffer.Rds")) 
state_waters_sp <- state_waters_sf %>% 
  sf::as_Spatial()



# # Ocean raster
# ################################################################################
# 
# # Read ocean raster
# ocean_ras_df <- readRDS(file.path(basedir, "counterfactuals", "counterfactual_layers_shallow_epsg3309.Rds"))
# 
# # Build ocean raster
# ocean_ras <- ocean_ras_df %>% 
#   select(x_epsg3309, y_epsg3309, cell) %>%  
#   raster::rasterFromXYZ(crs = crs("+init=epsg:3309")) %>% 
#   raster::projectRaster(crs=crs("+proj=longlat +datum=WGS84"))
# 
# # Convert back to dataframe
# ocean_ras_df_wgs84 <- ocean_ras %>% 
#   as.data.frame(xy=T) %>% 
#   filter(!is.na(cell))
# 
# # Get land
# usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
# foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")
# 
# # Plot data
# g <- ggplot() +
#   # Plot land
#   geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
#   geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
#   # Plot raster
#   geom_tile(data=ocean_ras_df_wgs84, mapping=aes(x=x, y=y), fill="grey30", alpha=0.5) +
#   # Crop
#   coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
#   # Theme
#   theme_bw()
# g


# Build data
################################################################################

# Files to merge
files2merge <- list.files(indir)

# Setup parallel
library(doParallel)
ncores <- detectCores()
registerDoParallel(cores=ncores)

# Loop through files
x <- files2merge[1]
# data <- purrr::map_df(files2merge, function(x){
# for(x in files2merge){
foreach(x = files2merge[c(17:19, 28, 33:36, 39:63)]) %dopar% {
    
  # Read data
  data_orig <- readRDS(file=file.path(indir, x))
  
  # Convert to sf
  data_sf <- data_orig %>%
    filter(!is.na(lat_dd) & !is.na(long_dd)) %>% 
    sf::st_as_sf(coords=c("long_dd", "lat_dd"), crs=sf::st_crs("+proj=longlat +datum=WGS84"), remove=F) 
  
  # Convert to sp
  data_sp <- data_sf %>% 
    sf::as_Spatial() 
  
  # Clip
  # Super convulated (its sp's fault): sp::over returns a dataframe key, it doesn't actually do an intersect
  data_clip_df <- sp::over(x=data_sp, y=state_waters_sp) # this is a really stupid function, it purposelyfully returns a df
  data_clip_sf <- data_sf %>% 
    mutate(inside_yn=!is.na(data_clip_df$id)) %>% 
    filter(inside_yn==T) 
  data_clip_sp <- data_clip_sf %>% 
    sf::as_Spatial()
  
  # Plot check
  if(F){
    ggplot() +
      geom_sf(data=data_sf, alpha=0.1, pch=1) +
      geom_sf(data=data_clip_sf, pch=1, alpha=0.1, color="red") +
      geom_sf(data=state_waters_sf, fill=NA) +
      theme_bw()
  }

  # Convert to dataframe
  data_out <- data_clip_sf %>%
    # Drop geometry
    sf::st_drop_geometry()
  
  # Export
  saveRDS(data_out, file.path(outdir, gsub(".Rds", "_clip.Rds", x)))
  
  
}
# })


# Export data
# saveRDS(data, file.path(outdir, "CA_ebird_data_inside_state_waters_5km.Rds"))



