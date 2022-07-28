

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data/"
indir <- file.path(basedir, "mpa_watch/raw")
outdir <- file.path(basedir, "mpa_watch/processed")
plotdir <- "data/mpa_watch/figures"

# Read MPA data
mpas <- readRDS(file.path(basedir, "mpa_traits/processed", "CA_mpa_metadata.Rds"))

# Read monitoring data
data_orig <- readRDS(file=file.path(outdir, "MPA_Watch_2011_2022_surveys_ca_programs_wide.Rds"))


# Format data
################################################################################

# Extract site info from data
sites_in_data <- data_orig %>% 
  # Reduce to unique
  select(program, site_id, site, site_type, subsite_id, subsite, subsite_type) %>% 
  unique() %>% 
  # Arrange
  arrange(program, site_id, subsite_id)


# Export data
write.csv(sites_in_data, file=file.path(outdir, "mpa_watch_survey_sites.csv"), row.names=F)


# Read data from MPA Watch team
################################################################################

# Read sites
sites_orig <- readxl::read_excel(file.path(indir, "mpa_watch_survey_sites.xlsx"))

# Format sites
sites <- sites_orig %>% 
  # Rename
  rename(subsite=survey_site, subsite_id=survey_site_id, subsite_type=survey_site_type,
         lat_dd=lat, long_dd=lng) %>% 
  # Arrange
  select(program, site_id, site, site_type, subsite_id, subsite, subsite_type, lat_dd, long_dd) %>% 
  # Format a few MPA names
  mutate(site=recode(site, "A√±o Nuevo SMR"="Año Nuevo SMR"))

# Subsites should be unique
anyDuplicated(sites$subsite_id)
anyDuplicated(sites$subsite)

# Check MPA names
mpa_names <- sort(unique(sites$site[sites$site_type=="MPA"]))
mpa_names[!mpa_names %in% mpas$mpa]

# Export
saveRDS(sites, file=file.path(outdir, "mpa_watch_survey_sites_clean.Rds"))


# Plot data
################################################################################

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# MPA regions
# CA/OR, Alder Creek, Pigeon Point, Point Conception, CA/MEX 
region_lats <- c(39.0, 37.18, 34.5)

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_blank(),
                   legend.text=element_text(size=5.5),
                   legend.title=element_text(size=7),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = "right",
                   legend.key.size = unit(0.4, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot() +
  # Plot regions
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot sites
  geom_point(data=sites, mapping=aes(x=long_dd, y=lat_dd, color=site_type)) +
  # Labels
  labs(x="", y="") +
  scale_color_discrete(name="Site type") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  # Crop
  coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.2, 0.1))
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_mpa_watch_survey_sites.png"), 
       width=3.5, height=5.25, units="in", dpi=600)

