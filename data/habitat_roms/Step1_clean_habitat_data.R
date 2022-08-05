

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
indir <- file.path(basedir, "habitat_roms/raw")
outdir <- file.path(basedir, "habitat_roms/processed")
plotdir <- "data/habitat_roms/figures"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "California cells lat long and habitat.xlsx"))

# Convert to regular grid
# https://gis.stackexchange.com/questions/79062/how-to-make-raster-from-irregular-point-data-without-interpolation


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  rename(cell_id=Cell, lat_dd=Latitude, long_dd=Longitude, mpa=MPA, area_sqkm=Area, depth_min_m=`Min Depth`, depth_max_m=`Max Depth`, ) %>% 
  # Simplify
  select(-Cell_MPA) %>% 
  # Gather
  gather(key="habitat", value="value", 8:ncol(.))


# Inspect
str(data)
table(data$habitat)


# Plot data
################################################################################

# Subset
sdata <- data %>% 
  filter(habitat=="Surfgrass")

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Plot data
g <- ggplot(sdata, aes(x=long_dd, y=lat_dd, color=value)) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3, inherit.aes=F) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3, inherit.aes=F) +
  # Plot habitat
  geom_point(size=3) +
  # Labels
  labs(x="", y="") +
  # Crop
  coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
  # Legend
  scale_color_gradientn(name="Surfgrass", colors=RColorBrewer::brewer.pal(9, "Greens")) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw()
g  
  
  