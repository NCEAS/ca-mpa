

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(rinat)
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data/"
indir <- file.path(basedir, "inaturalist/raw/webscraped_2019")
outdir <- file.path(basedir, "inaturalist/processed")
plotdir <- "data/inaturalist/figures"


# Bounding box
################################################################################

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Plot data
g <- ggplot() +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3) +
  # Bounding box
  geom_rect(aes(xmin=-126, xmax=-116, ymin=32.4, ymax=42), fill=NA, color="black") +
  # Crop
  coord_sf(xlim = c(-127, -115), ylim = c(31, 43)) +
  # Theme
  theme_bw()
g


# Scrape data
################################################################################

# Build year month key
date_key <- expand.grid(year=2019:2021, month=1:12) %>%
  arrange(year, month)

# Make 50 slivers
nslivers <- 50
slivers <- seq(32.4, 45, length.out=50)
lat_df <- tibble(lat_lo=slivers[1:49],
                 lat_hi=slivers[2:50])

# Build key
key <- expand.grid(year=2019:2021, 
                   month=1:12,
                   lat_lo=lat_df$lat_lo) %>% 
  # Add lat hi
  left_join(lat_df) %>% 
  # Arrange
  arrange(year, month, lat_lo)


# Loop through and scrape: 1668 did not work
i <- 1690
for(i in 1690:nrow(key)){

  # Grab data
  print(i)
  year_do <- key$year[i]
  month_do <- key$month[i]
  lat_lo <- key$lat_lo[i]
  lat_hi <- key$lat_hi[i]

  # Scrape data
  data <- rinat::get_inat_obs(year=year_do, month=month_do, maxresults=10000,
                              bounds=c(lat_lo, -126, lat_hi, -116))
 
  # Export data
  filename <- paste0(year_do, "_", month_do, "_", round(lat_lo,1), ".csv")
  write.csv(data, file=file.path(indir, filename), row.names=F)

}


# Confirm done?
################################################################################

# Files to do
files2do <- key %>% 
  mutate(filename=paste0(year, "_", month, "_", round(lat_lo,1), ".csv")) %>% 
  pull(filename)

# Files done
files_done <- list.files(indir)
freeR::which_duplicated(files_done) # confusing that some are duplicated

# Are all the files done?
files2do[!files2do %in% files_done]

which(files2do=="2021_11_38.3.csv")

# Which files are wrong?
files_done[!files_done %in% files2do]







