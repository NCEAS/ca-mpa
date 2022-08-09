

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
gisdir <- file.path(basedir, "gis_data/processed")
plotdir <- "data/mpa_watch/figures"

# Read MPA data
mpas <- readRDS(file.path(gisdir, "CA_MPA_polygons.Rds"))

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
  mutate(site=recode(site, "A√±o Nuevo SMR"="Año Nuevo SMR")) %>% 
  # Fill in some missing coords
  # Table Rock LB-SMCA: 33.501924, -117.746359
  # CHSMCA1: 33.434476, -118.503406
  mutate(lat_dd=ifelse(subsite=="Table Rock LB-SMCA", 33.501924, lat_dd),
         long_dd=ifelse(subsite=="Table Rock LB-SMCA", -117.746359, long_dd),
         lat_dd=ifelse(subsite=="CHSMCA1", 33.434476, lat_dd),
         long_dd=ifelse(subsite=="CHSMCA1", -118.503406, long_dd))

# Subsites should be unique
anyDuplicated(sites$subsite_id)
anyDuplicated(sites$subsite)

# Check MPA names
mpa_names <- sort(unique(sites$site[sites$site_type=="MPA"]))
mpa_names[!mpa_names %in% mpas$mpa]



# Identify MPA closest to each control site
################################################################################

# MPAs with MPA Watch data
mpas_use <- mpas %>% 
  filter(name %in% mpa_names)

# Convert MPA Watch sites to sf
sites_sf <- sf::st_as_sf(sites, coords=c("long_dd", "lat_dd"), crs=sf::st_crs(mpas_use))

# Calculate distance (meters) of each site to each MPA
dist_mat <- sf::st_distance(sites_sf, mpas_use)
dist_df <- dist_mat %>% 
  as.data.frame() %>% 
  # Set row/col name
  magrittr::set_rownames(sites$subsite) %>% 
  magrittr::set_colnames(mpas_use$name) %>% 
  rownames_to_column(var="subsite") %>% 
  # Gather
  gather(key="mpa", value="dist_m", 2:ncol(.)) %>% 
  # Convert meters to kilometers
  mutate(dist_km=dist_m %>% as.numeric() / 1000) %>% 
  # Find the closest MPA to each site
  group_by(subsite) %>% 
  arrange(subsite, dist_km) %>% 
  slice(1) %>% 
  ungroup() %>% 
  # Rename
  rename(mpa_closest=mpa, mpa_closest_km=dist_km) %>% 
  select(-dist_m)

# Add distance to site
sites1 <- sites %>% 
  left_join(dist_df, by="subsite")

# Are the MPA sites all closest to the correct MPA? Yes, w/ fuzzy understanding of Laguna Beach
check1 <- sites1 %>% 
  filter(site_type=="MPA") %>% 
  mutate(check=site==mpa_closest)

# Are all 49 MPAs represent in control sites? No
sites1 %>% 
  filter(site_type=="Control") %>% 
  pull(mpa_closest) %>% n_distinct()

hist(sites1$mpa_closest_km[sites1$site_type=="MPA"])
hist(sites1$mpa_closest_km[sites1$site_type=="Control"])

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
  geom_point(data=sites, mapping=aes(x=long_dd, y=lat_dd, fill=site_type), pch=21, size=3, alpha=0.5) +
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

