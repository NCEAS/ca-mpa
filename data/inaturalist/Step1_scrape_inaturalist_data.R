

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(rinat)
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data/"
indir <- file.path(basedir, "inaturalist/raw/webscraped")
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
date_key <- expand.grid(year=2000:2021, month=1:12) %>%
  arrange(year, month)

# Loop through and scrape
i <- 17 # starting at 233
for(i in 233:nrow(date_key)){

  # Grab data
  print(i)
  year_do <- date_key$year[i]
  month_do <- date_key$month[i]

  # Attempt at month-level
  data <- try(rinat::get_inat_obs(year=year_do, month=month_do, maxresults=10000,
                              bounds=c(32.4, -126, 42, -116)))
  Sys.sleep(1)

  # If it doesn't work at the month-level
  if(inherits(data, "try-error")){

    # Build dates
    date1 <- paste(year_do, month_do, 1, sep="-") %>% lubridate::ymd()
    date2 <- paste(year_do, month_do+1, 1, sep="-") %>% lubridate::ymd() - 1
    days <- seq(date1, date2, by="1 day") %>% lubridate::day()

    # Loop through dates
    j <- 7
    for(j in 1:length(days)){

      # Scrape
      print(j)
      day_do <- days[j]
      data <- try(rinat::get_inat_obs(year=year_do, month=month_do, day=day_do,
                                  maxresults=10000,
                                  bounds=c(32.4, -126, 42, -116)))
      Sys.sleep(1)

      # Export
      if(!inherits(data, "try-error")){
        filename <- paste0(year_do, "_", month_do, "_", day_do, ".csv")
        write.csv(data, file=file.path(indir, filename), row.names=F)
      }

    }


  }else{
    # Export data
    filename <- paste0(year_do, "_", month_do, ".csv")
    write.csv(data, file=file.path(indir, filename), row.names=F)
  }

}


