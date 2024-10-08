# Plot CCFRP drifts



library(tidyverse)
library(sf)

data_path <- "/home/shares/ca-mpa/data/sync-data/"


drifts <- ccfrp_drift %>% 
  filter(!(is.na(st_lon_dd))) %>%
  filter(!(is.na(st_lat_dd))) %>% 
  st_as_sf(coords = c("st_lon_dd", "st_lat_dd"), 
           crs = 4326, agr = "constant", remove = F)

species <- ccfrp_caught_fishes %>% 
  group_by(species_code) %>% 
  summarize(count = n())

# Get MPA polygons
mpas <- readRDS(file.path(data_path, "gis_data/processed/CA_MPA_polygons.Rds"))

# CA/OR, Alder Creek, Pigeon Point, Point Conception, CA/MEX 
region_lats <- c(42, 39.0, 37.18, 34.5, 32.5) %>% rev()

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_blank(),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.4, "cm"),
                   legend.key = element_rect(fill=alpha('blue', 0)),
                   legend.background = element_rect(fill=alpha('blue', 0)))




# Plot data
g <- ggplot() +
  # Plot blocks
  #geom_sf(data=blocks, mapping=aes(fill=mlpa_region, alpha=mpa_yn), lwd=0.2) +
  # Plot region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot MPAs
  geom_sf(data = mpas, fill = "red", alpha = 0.5, lwd = 0.3)+
  # Plot drifts
  geom_sf(data = drifts) +
  # Legend
  #scale_fill_discrete(name="MLPA region") +
  #scale_alpha_manual(name="Block type", values=c(1, 0.2)) +
  # Crop
  coord_sf(xlim = c(-117.4, -117.2), ylim = c(32.75, 33.15)) +
  # Theme
  theme_bw() + my_theme
g


### Filter some of the caught fishes
drift_subset <- drifts %>% filter(between(st_lat_dd, 32.75, 33.2)) %>% 
  filter(between(st_lon_dd, -117.4, -117.2)) %>% 
  select(drift_id, st_lat_dd, st_lon_dd, site_mpa_ref)

subset <- ccfrp_caught_fishes %>% 
  filter(drift_id %in% drift_subset$drift_id)

subset_totals <- subset %>% 
  group_by(drift_id, species_code) %>% 
  summarize(n = n())


## Kelp Bass
klp_join <- subset_totals %>% 
  filter(species_code == "KLP") %>% 
  group_by(drift_id) %>% 
  summarize(total_klp = sum(n)) %>% 
  left_join(., drift_subset) %>% 
  mutate(klp_bins = round(st_lat_dd, 3)) %>% 
  group_by(klp_bins, site_mpa_ref) %>% 
  summarize(klp_per_bin = sum(total_klp))


ggplot(data = klp_join %>% 
         filter(between(klp_bins, 32.8, 32.9))) +
  geom_point(aes(x = klp_bins, y = klp_per_bin, color = site_mpa_ref)) +
  labs(color = NULL) + my_theme

ggplot(data = klp_join %>% 
         filter(between(klp_bins, 32.95, 33.2))) +
  geom_point(aes(x = klp_bins, y = klp_per_bin, color = site_mpa_ref)) + my_theme


# Whitefish
test_join <- subset_totals %>% 
  filter(species_code == "GPR") %>% 
  group_by(drift_id) %>% 
  summarize(total = sum(n)) %>% 
  left_join(., drift_subset) %>% 
  mutate(bins = round(st_lat_dd, 3)) %>% 
  group_by(bins, site_mpa_ref) %>% 
  summarize(per_bin = sum(total))

ggplot(data = test_join) +
  geom_point(aes(x = bins, y = per_bin, color = site_mpa_ref)) +
  labs(color = NULL) + my_theme

ggplot(data = cbz_join %>% 
         filter(between(cbz_bins, 32.95, 33.2))) +
  geom_point(aes(x = cbz_bins, y = cbz_per_bin, color = site_mpa_ref)) + my_theme

