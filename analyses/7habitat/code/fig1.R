# Build conceptual figure for habitat paper
# Cori Lopazanski
# April 2025

library(terra)
library(tidyverse)
library(sf)
library(cowplot)
library(tmaptools)
library(tmap)
library(rnaturalearth)
library(patchwork)

my_theme <- theme_minimal(base_family = "Arial") + 
  theme(plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 8),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.margin = margin(t = 0, unit='cm'),
        plot.caption = element_text(size = 8),
        strip.text = element_text(size = 8, face = "bold"),
        panel.background = element_rect(fill = "white", color = NA),  
        plot.background = element_rect(fill = "white", color = NA))

ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"
fig.dir <- "~/ca-mpa/analyses/7habitat/figures"


historical <- read_sf("/home/shares/ca-mpa/data/sync-data/gis_data/raw/MPA_CA_MPA_preMLPA_2005") %>%  st_as_sf()


# Filter to the sites that are included in the analayses
site_cols <- c("year", "site", "site_type", "bioregion", "region4", "affiliated_mpa")
included_kelp <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/data", "kelp_filtered_targeted_msy_data.rds")) %>% distinct(across(all_of(site_cols))) %>% mutate(habitat = "Kelp forest")
included_rock <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/data", "rock_filtered_targeted_rmsy_data.rds")) %>% distinct(across(all_of(site_cols))) %>% mutate(habitat = "Shallow reef")
included_surf <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/data", "surf_filtered_targeted_m_data.rds")) %>% distinct(across(all_of(site_cols))) %>% mutate(habitat = "Surf zone")

included_sites <- bind_rows(included_kelp, included_rock, included_surf)
rm(included_kelp, included_rock, included_surf)

sites <- readRDS(file.path(ltm.dir, "site_locations_corrected.Rds")) %>% 
  mutate(habitat = if_else(habitat == "Rocky reef", "Shallow reef", habitat)) %>% 
  filter(site %in% included_sites$site) %>% 
  mutate(habitat = factor(habitat, levels = c("Shallow reef", "Kelp forest", "Surf zone")))

mpa_poly <- readRDS(file.path("/home/shares/ca-mpa/data/sync-data/gis_data/processed", "CA_MPA_polygons.Rds")) %>% 
  mutate(affiliated_mpa = str_remove_all(name, regex("\\(No-Take\\)")) %>% 
           str_to_lower() %>% str_trim()) %>% 
  filter(affiliated_mpa %in% included_sites$affiliated_mpa)

# Transform each to have same crs
sites <- st_transform(sites, crs = 4326)
mpa_poly <- st_transform(mpa_poly, crs = 4326)

ca_bb <- st_bbox(c(xmin = -125, xmax = -116, ymin = 32.5, ymax = 42.05),
              crs = st_crs(4326))

ca_land <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf") %>% 
  filter(name == "California")


bg_footprint <- st_union(mpa_poly) %>% 
  st_buffer(95000) %>% 
  st_concave_hull(concavity = 1, ratio = 0.5) %>% 
  st_crop(ca_bb) %>% 
  st_simplify(dTolerance = 4000) %>% 
  st_crop(ca_bb)

# 1. Build main statewide plot with dots for each site, colored by ecosystem ----

eco_colors <- c("Shallow reef" ="#b3332f",  
                "Kelp forest"  = "darkgreen"  ,  
                "Surf zone"    = "#4e79a7")

  # cool medium blue — distinct from green and brown
   # darker natural green — richer and cooler than #7fcd8b
   # warm rust/brown-red — deeper and redder than #b89d7a

tmap_mode("plot")

site_map <- tm_shape(ca_land) +
  tm_polygons(col = "gray80", border.col = NULL) +
  tm_shape(mpa_poly) + tm_borders() +
  tm_shape(sites) +
  tm_dots(fill = "habitat", fill_alpha = 0.8, 
          fill.scale = tm_scale_categorical(values = eco_colors), size = 1,
          fill.legend = tm_legend(title = "", frame = FALSE)) +
  tm_graticules(lines = F) +
  tm_scalebar(position = tm_pos_in(pos.h = "left", pos.v = "bottom")) +
  tm_layout(frame = FALSE,          
    panel.label.bg.color= NA,                     
    panel.label.bg.alpha= 0,
    panel.label.frame = F,
    legend.position =tm_pos_in(pos.h = "left", pos.v = "bottom"),
    inner.margins = c(0.01, 0.03, 0.01, 0.01),    
    outer.margins = c(0.01, 0.01, 0, 0))

site_map

tmap_save(site_map, file.path(fig.dir, "habitat-fig1-sites.png"), dpi = 600)

#geom_text(data=region_labels, mapping=aes(x=long_dd, y=lat_dd, label=label, fontface="bold"), hjust=0, size=2.3) +
#geom_hline(mapping=aes(yintercept=region_lats)) +
# MPA regions
# CA/OR, Alder Creek, Pigeon Point, Point Conception, CA/MEX 
# region_lats <- c(39.0, 37.18, 34.5)
# 
# # Region labels
# region_labels <- tibble(long_dd=c(-123.9, -122.9, -121, -118#, -119.5
# ),
# lat_dd=c(40.5, 38.7, 36, 34.1#, 34.8
# ),
# label=c("North\n(Dec 2012)", "North Central\n(May 2010)", "Central\n(Sep 2007)", "South\n(Jan 2012)"#, "N. Channel\nIslands (2003)"
# ))


# 2. Build inset for point lobos --------------------------------------------------------------------

lobos_sites <- sites %>% filter(site %in% included_sites$site[included_sites$affiliated_mpa == "point lobos smr"]) 
lobos_mpa <- mpa_poly %>%filter(affiliated_mpa == "point lobos smr")

top_edge <- lobos_mpa %>%
  st_cast("MULTILINESTRING") %>%
  st_coordinates() %>%
  as_tibble() %>%
  filter(Y >= round(max(Y), 4)) %>% 
  filter(X > min(X)) %>%
  slice_min(X, n = 1)

lobos_label <- data.frame(
  name = lobos_mpa$name,
  x = top_edge$X - 0.025,
  y = top_edge$Y + 0.015
)

ggplot() +
  geom_sf(data = ca_land, fill = "gray90", color = NA) +
  geom_sf(data = lobos_mpa, fill = "gray80", color = "black") +
  geom_segment(data = lobos_label, aes(x = top_edge$X - 0.02, y = top_edge$Y, xend = x, yend = y), color = "black", linewidth = 0.3) +
  geom_label(data = lobos_label, aes(x = x, y = y, label = name), size = 3) +
  geom_sf(data = lobos_sites, aes(color = habitat), size = 2, alpha = 1, show.legend = F) +
  coord_sf(xlim = c(-121.99, -121.90),
           ylim = c(36.445, 36.585),
           expand = T)+
  scale_color_manual(values = eco_colors) +
  labs(color = NULL)+
  theme_void() +
  theme(legend.position.inside = c(0.01, 0.02),     
        legend.justification = c("left", "bottom"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 9),
        legend.background = element_rect(fill = "white", color = NA),
        legend.key.height = unit(10, "pt"),
        legend.spacing = unit(2, "pt"),
        plot.margin = margin(0, 0, 0, 0))



# Point lobos with habitat data 
habitat <- readRDS(file.path("/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed_v2/combined",
                             "combined_mpas_23_30_31_32.Rds"))

kelp <- terra::rast(file.path("/home/shares/ca-mpa/data/sync-data/kelpwatch/2024/processed",
                          "kelp_canopy_2014.tif"))

lobos_habitat <- habitat %>% 
  filter(affiliated_mpa == "point lobos smr") %>% 
  mutate(habitat_class = if_else(habitat_class %in% c("Aquatic Vascular Vegetation", "Aquatic Vegetation Bed"), "Max Biotic Extent", habitat_class)) %>% 
  mutate(habitat_class = factor(habitat_class, levels = c("Max Biotic Extent", "Hard Bottom", "Soft Bottom"))) %>% 
  arrange(habitat_class)

inset_ext <- terra::ext(lobos_habitat)
inset_ext <- project(inset_ext, from = crs(lobos_habitat), to = crs(kelp))

kelp_crop <- crop(kelp, inset_ext)
kelp_crop <- terra::project(kelp_crop, crs(lobos_sites))

kelp_poly <- as.polygons(kelp_crop) %>% 
  st_as_sf() %>% 
  st_union() %>% st_as_sf() %>% 
  mutate(habitat_class = "Kelp (Shown for 2014)")


hab_colors <- c(
  "Hard Bottom" = "#b89d7a",     # medium taupe-brown
  "Soft Bottom" = "#e6d8ab",     # warm sand
  "Max Biotic Extent" = "#7fcd8b",  # stronger green with visibility
  "Kelp (Shown for 2014)" = "#ce8300"
)


lobos_inset <- ggplot() +
  geom_sf(data = ca_land, fill = "gray90", color = NA) +
  geom_sf(data = lobos_habitat %>% filter(habitat_class %in% c("Hard Bottom", "Soft Bottom")), aes(fill = habitat_class)) + 
  geom_sf(data = lobos_habitat %>% filter(!habitat_class %in% c("Hard Bottom", "Soft Bottom")), 
          aes(fill = habitat_class)) + 
  geom_sf(data = kelp_poly, aes(fill = habitat_class))+
 # geom_tile(data = kelp_df, aes(x = x, y = y, fill = habitat_class)) + 
  geom_sf(data = lobos_mpa, fill = "transparent", color = "black", lwd = 0.7) +
  geom_segment(data = lobos_label, aes(x = top_edge$X - 0.02, y = top_edge$Y, xend = x, yend = y), color = "black", linewidth = 0.3) +
  geom_label(data = lobos_label, aes(x = x, y = y, label = name), size = 4) +
  geom_sf(data = lobos_sites, aes(color = habitat), size = 3, alpha = 1, show.legend = F) +
  coord_sf(xlim = c(-122.02, -121.92),
           ylim = c(36.445, 36.585),
           expand = T)+
  scale_color_manual(values = eco_colors) +
  scale_fill_manual(values = hab_colors) +
  labs(color = NULL, fill = NULL)+
  theme_void() +
  theme(legend.position = c(0.88, 0.93),     
        legend.text = element_text(size = 10),
        legend.background = element_rect(fill = NA, color = NA),
        legend.key.height = unit(10, "pt"),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.spacing = unit(2, "pt"),
        plot.margin = margin(0, 0, 0, 0))

lobos_inset
ggsave(file.path(fig.dir, "habitat-fig1-lobos.png"), plot = lobos_inset, width = 4.5, height = 5.5, units = 'in', dpi = 600)

# 3. Build inset for sites --------------------------------------------------------------------

site_buffers <- readRDS(file.path("/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed_v2/combined/buffers",
                                  "habitat_buffers_500m.Rds")) %>% 
  filter(site %in% lobos_sites$site)

site_buffers2 <- site_buffers %>% 
  mutate(habitat = if_else(habitat == "Rocky reef", "Shallow reef", habitat)) %>% 
  mutate(habitat_class = if_else(habitat_class %in% c("Aquatic Vascular Vegetation", "Aquatic Vegetation Bed"), "Max Biotic Extent", habitat_class)) %>% 
  mutate(habitat_class = factor(habitat_class, levels = c("Hard Bottom", "Soft Bottom", "Max Biotic Extent"))) %>% 
  group_by(habitat, site, site_type, habitat_class, depth_zone) %>% 
  summarize(geometry = st_union(geometry), .groups = 'drop') %>% 
  arrange(habitat_class)

unique(site_buffers2$site)

# To pick which sites, can run this to explore which to include:
tmap_mode('view')

tm_shape(site_buffers2 %>% filter(habitat_class != "Max Biotic Extent")) +
  tm_polygons(fill = "habitat_class",
              fill.scale = tm_scale_categorical(values = hab_colors)) +
  tm_shape(site_buffers2 %>% filter(habitat_class == "Max Biotic Extent")) +
  tm_polygons(fill = "habitat_class",
              fill.scale = tm_scale_categorical(values = hab_colors), 
              fill.legend = tm_legend_hide()) +
  tm_shape(lobos_sites) +
  tm_dots(fill = "habitat", 
          fill.scale = tm_scale_categorical(values = eco_colors),
          size = 1) 

# Process plots for each site
sites_sub <- st_transform(lobos_sites, crs = st_crs(site_buffers2))
kelp_sub <- st_transform(kelp_poly, crs = st_crs(site_buffers2))

site_plot_data <- site_buffers2 %>%
  group_split(habitat, site_type, site) %>%
  map(~ {
    dat <- .x
    kelp <- st_intersection(kelp_sub, dat)
    label <- paste(unique(dat$habitat), unique(dat$site_type), unique(dat$site), sep = " | ")
    
    plot <- ggplot(dat) +
      geom_sf(aes(fill = habitat_class), color = NA, alpha = 1) +
      geom_sf(data = kelp, aes(fill = habitat_class)) + 
      scale_fill_manual(values = hab_colors, drop = FALSE) +
      ggforce::geom_circle(data = sites_sub %>% filter(site == unique(dat$site)),
                  aes(x0 = st_coordinates(geometry)[,1],
                      y0 = st_coordinates(geometry)[,2],
                      r = 500,
                      color = habitat),
                  fill = NA, linewidth = 1) +
      ggforce::geom_circle(data = sites_sub %>% filter(site == unique(dat$site)),
                           aes(x0 = st_coordinates(geometry)[,1],
                               y0 = st_coordinates(geometry)[,2],
                               r = 250,
                               color = habitat),
                           fill = NA, linewidth = 1) +
      ggforce::geom_circle(data = sites_sub %>% filter(site == unique(dat$site)),
                           aes(x0 = st_coordinates(geometry)[,1],
                               y0 = st_coordinates(geometry)[,2],
                               r = 50,
                               color = habitat),
                           fill = NA, linewidth = 1) +
      scale_color_manual(values = eco_colors, drop = FALSE) +
      coord_sf(expand = T) +
      theme_void() +
      theme(legend.position = "none",
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA)
      )
    
    list(name = label, plot = plot)
  })

plot_list <- map(site_plot_data, "plot")
names(plot_list) <- map_chr(site_plot_data, "name")
mpa_inset <- plot_list[[6]] # weston uc
mpa_inset
ggsave(file.path(fig.dir, "habitat-fig1-mpa-inset.png"), plot = mpa_inset, dpi = 600, width = 3, height = 3)

ref_inset <- plot_list[[10]] # soberanes
ref_inset
ggsave(file.path(fig.dir, "habitat-fig1-ref-inset.png"), plot = ref_inset, dpi = 600, width = 3, height = 3)

rock_inset <- plot_list[[15]]
rock_inset
ggsave(file.path(fig.dir, "habitat-fig1-rock-inset.png"), plot = rock_inset, dpi = 600, width = 3, height = 3)

# Test a combination plot with all the sites (this didn't work very well)
# Parse names into metadata
plot_df <- tibble(
  name = names(plot_list),
  plot = plot_list
) %>%
  separate(name, into = c("habitat", "site_type", "site"), sep = " \\| ")

# Nest by habitat × site_type
grouped <- plot_df %>%
  group_by(habitat, site_type) %>%
  group_modify(~ tibble(patch = list(plot_grid(plotlist = .x$plot, nrow = ceiling(length(.x$plot) / 4))))) %>% 
  ungroup()

# Rebuild grid with labels; One row per habitat, columns = MPA/Reference
habitats <- unique(grouped$habitat)
site_types <- unique(grouped$site_type)

rows <- map(habitats, function(h) {
  row_plots <- grouped %>%
    filter(habitat == h) %>%
    arrange(match(site_type, site_types)) %>%
    pull(patch)
  
  row_label <- ggdraw() + draw_label(h, fontface = "bold", size = 12, x = 0, hjust = 0)
  plot_grid(row_label, plotlist = row_plots, nrow = 1, rel_widths = c(0.2, rep(1, length(row_plots))))
})

col_labels <- plot_grid(NULL,
                        plot_grid(plotlist = map(site_types, ~ ggdraw() + draw_label(.x, fontface = "bold", hjust = 0.5)),
                                  nrow = 1), rel_heights = c(0.1, 1))

# Step 5: Combine everything
final_plot <- plot_grid(col_labels, plot_grid(plotlist = rows, ncol = 1), nrow = 2, rel_heights = c(0.1, 1))

# Done!
final_plot

# 4. Create additional visuals for the presentation --------------------------------------------

ggplot() +
  geom_sf(data = ca_land, fill = "gray90", color = NA) +
  geom_sf(data = lobos_habitat %>% filter(habitat_class %in% c("Hard Bottom", "Soft Bottom")), 
          aes(fill = habitat_class), show.legend = F) + 
  geom_sf(data = lobos_habitat %>% filter(!habitat_class %in% c("Hard Bottom", "Soft Bottom")), 
          aes(fill = habitat_class), show.legend = F) + 
  geom_sf(data = kelp_poly, aes(fill = habitat_class), show.legend = F)+
  geom_sf(data = lobos_mpa, fill = "transparent", color = "black", lwd = 0.7) +
 # geom_segment(data = lobos_label, aes(x = top_edge$X - 0.02, y = top_edge$Y, xend = x, yend = y), color = "black", linewidth = 0.3) +
 # geom_label(data = lobos_label, aes(x = x, y = y, label = name), size = 4) +
#  geom_sf(data = lobos_sites, aes(color = habitat), size = 3, alpha = 1, show.legend = F) +
  coord_sf(xlim = c(-121.98, -121.925),
           ylim = c(36.48, 36.53),
           expand = T)+
 # scale_color_manual(values = eco_colors) +
  scale_fill_manual(values = hab_colors) +
  labs(color = NULL, fill = NULL)+
  theme_void() +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = margin(0, 0, 0, 0))

ggsave(file.path(fig.dir, "habitat-pres-lobos.png"), dpi = 600, width = 6, height = 6)

