# Build conceptual figure for habitat paper
# Cori Lopazanski
# April 2025


library(tidyverse)
library(sf)
library(cowplot)
library(tmaptools)
library(tmap)
library(rnaturalearth)
library(patchwork)

ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"



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

# 1. Build main statewide plot with dots for each site, colored by ecosystem

eco_colors <- c("Shallow reef"  = "firebrick",
                "Kelp forest" = "darkgreen",
                "Surf zone"   = "steelblue")

tmap_mode("plot")

tm_shape(ca_land) +
  tm_polygons(col = "gray80", border.col = NULL) +
  tm_shape(mpa_poly) + tm_borders() +
  tm_shape(sites) +
  tm_dots(fill = "habitat", fill_alpha = 0.8, 
          fill.scale = tm_scale_categorical(values = eco_colors), size = 0.5,
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

## Test the stacked panels for each ecosystem
panels <- lapply(names(eco_colors), function(h) {
  ids <- unique(included_sites$affiliated_mpa[included_sites$habitat == h])
  mpa_sub <- mpa_poly %>% 
    filter(affiliated_mpa %in% ids)
  
  tm_shape(bg_footprint) +
   tm_polygons(col = "white", border.col = NULL) + 
    tm_shape(ca_land, bbox = bb) +
    tm_polygons(col = "gray90", border.col = NULL) +
    #tm_borders(col = "black", lwd = 1.5) + 
    tm_shape(mpa_sub) + tm_polygons(fill = "gray80", lwd = 1.5) +
    tm_shape(subset(sites, habitat == h)) +
    tm_dots(size = 0.8, fill = eco_colors[h], fill_alpha = 0.5) +
    tm_title(h, size = 0.8) +
    tm_layout(bg.color = "transparent",
      frame = FALSE,
      inner.margins = c(0,0,0,0),
      outer.margins = c(0,0,0,0))
  
   
})

grobs <- lapply(panels, tmap_grob)

ggdraw(xlim = c(0, 2), ylim = c(0, 1), clip = "off") +
  draw_grob(grobs[[1]], x = 0, y = 0, width = 1, height = 1, hjust = 0) +
  draw_grob(grobs[[2]], x = 0.45, y = 0, width = 1, height = 1, hjust = 0) +
  draw_grob(grobs[[3]], x = 0.9, y = 0, width = 1, height = 1, hjust = 0)


# Function to make a map for one habitat
make_habitat_map <- function(h) {
  ids <- unique(included_sites$affiliated_mpa[included_sites$habitat == h])
  mpa_sub <- mpa_poly %>% filter(affiliated_mpa %in% ids)
  sites_h <- sites %>% filter(habitat == h)
  
  ggplot() +
    geom_sf(data = bg_footprint, fill = "grey90", color = NA) +
    geom_sf(data = ca_land, color = "black", fill = "white", lwd = 0.5) +
    # geom_sf(data = usa, fill = "gray90", color = NA) +
    geom_sf(data = mpa_sub, fill = "gray80", color = NA, linewidth = 0.3) +
    geom_sf(data = sites_h, aes(color = habitat), size = 2, alpha = 0.5, show.legend = FALSE) +
    scale_color_manual(values = eco_colors) +
    coord_sf(xlim = c(ca_bb["xmin"], ca_bb["xmax"]),
             ylim = c(ca_bb["ymin"], ca_bb["ymax"]),
             expand = FALSE) +
    theme_void() +
    ggtitle(h) +
    theme(
      plot.title = element_text(hjust = 0, size = 10, face = "bold"),
      plot.margin = margin(0, 0, 0, 0)
    )
}

# Create each habitat map
main_map <- make_habitat_map("Shallow reef")
inset_1  <- make_habitat_map("Kelp forest")
inset_2  <- make_habitat_map("Surf zone")

# Combine with overlapping insets
p <- ggdraw(xlim = c(0., 2)) +
  draw_plot(main_map, x = 0,    y = 0,    width = 1,   height = 1) +
  draw_plot(inset_1,  x = 0.45, y = 0, width = 1, height = 1) +
  draw_plot(inset_2,  x = 0.9, y = 0, width = 1, height = 1)

p
ggsave("fig1.png", plot = p, dpi = 600, width = 6.5, height = 6.5, bg = "white", units = c("in"))


# 2. Build inset for point lobos 
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

# Point lobos with habitat data 
habitat <- readRDS(file.path("/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed_v2/combined",
                             "combined_mpas_23_30_31_32.Rds"))
lobos_habitat <- habitat %>% 
  filter(affiliated_mpa == "point lobos smr") %>% 
  mutate(habitat_class = if_else(habitat_class %in% c("Aquatic Vascular Vegetation", "Aquatic Vegetation Bed"), "Max Biotic Extent", habitat_class)) %>% 
  mutate(habitat_class = factor(habitat_class, levels = c("Max Biotic Extent", "Hard Bottom", "Soft Bottom"))) %>% 
  arrange(habitat_class)

hab_colors <- c(
  "Hard Bottom" = "#b89d7a",     # medium taupe-brown
  "Soft Bottom" = "#e6d8ab",     # warm sand
  "Max Biotic Extent" = "#7fcd8b"  # stronger green with visibility
)

ggplot() +
  geom_sf(data = ca_land, fill = "gray90", color = NA) +
  geom_sf(data = lobos_habitat %>% filter(habitat_class %in% c("Hard Bottom", "Soft Bottom")), aes(fill = habitat_class), color = NA) + 
  geom_sf(data = lobos_habitat %>% filter(!habitat_class %in% c("Hard Bottom", "Soft Bottom")), 
          aes(fill = habitat_class), color = NA) + 
  geom_sf(data = lobos_mpa, fill = "transparent", color = "black", lwd = 0.7) +
  geom_segment(data = lobos_label, aes(x = top_edge$X - 0.02, y = top_edge$Y, xend = x, yend = y), color = "black", linewidth = 0.3) +
  geom_label(data = lobos_label, aes(x = x, y = y, label = name), size = 4) +
  geom_sf(data = lobos_sites, aes(color = habitat), size = 2, alpha = 1, show.legend = F) +
  coord_sf(xlim = c(-121.99, -121.90),
           ylim = c(36.445, 36.585),
           expand = T)+
  scale_color_manual(values = eco_colors) +
  scale_fill_manual(values = hab_colors) +
  labs(color = NULL, fill = NULL)+
  theme_void() +
  theme(legend.position = c(0.8, 0.98),     
        legend.justification = c("right", "top"),
        legend.text = element_text(size = 10),
        legend.background = element_rect(fill = NA, color = NA),
        legend.key.height = unit(10, "pt"),
        legend.spacing = unit(2, "pt"),
        plot.margin = margin(0, 0, 0, 0))


# Now try the site-level plots?
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
tmap_mode('plot')

tm_shape(site_buffers2) +
  tm_polygons(fill = "habitat_class", col = NULL,
              fill.scale = tm_scale_categorical(values = hab_colors), fill_alpha = 0.8) +
  tm_facets_grid(rows = "habitat", columns = "site_type", free.coords = T)


tm_shape(site_buffers2 %>% filter(habitat_class != "Max Biotic Extent")) +
  tm_polygons(fill = "habitat_class",
              fill.scale = tm_scale_categorical(values = hab_colors)) +
  tm_shape(site_buffers2 %>% filter(habitat_class == "Max Biotic Extent")) +
  tm_polygons(fill = "habitat_class",
              fill.scale = tm_scale_categorical(values = hab_colors), 
              fill.legend = tm_legend_hide()) +
#  tm_shape(lobos) +
 # tm_borders(col = "black") +
  tm_shape(lobos_sites) +
  tm_dots(fill = "habitat", 
          fill.scale = tm_scale_categorical(values = eco_colors),
          size = 1) 
 # tm_labels(text = "site") 

sites_sub <- st_transform(sites_sub, crs = st_crs(site_buffers2))

site_plot_data <- site_buffers2 %>%
  group_split(habitat, site_type, site) %>%
  map(~ {
    dat <- .x
    label <- paste(unique(dat$habitat), unique(dat$site_type), unique(dat$site), sep = " | ")
    
    plot <- ggplot(dat) +
      geom_sf(aes(fill = habitat_class), color = NA, alpha = 0.9) +
      scale_fill_manual(values = hab_colors, drop = FALSE) +
      geom_circle(data = sites_sub %>% filter(site == unique(dat$site)),
                  aes(x0 = st_coordinates(geometry)[,1],
                      y0 = st_coordinates(geometry)[,2],
                      r = 500,
                      color = habitat),
                  fill = NA, linewidth = 1) +
      scale_color_manual(values = eco_colors, drop = FALSE) +
      coord_sf(expand = FALSE) +
      theme_void() +
      theme(
        legend.position = "none",
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA)
      )
    
    list(name = label, plot = plot)
  })

plot_list <- map(site_plot_data, "plot")
names(plot_list) <- map_chr(site_plot_data, "name")
plot_list[3]

# Step 1: Parse names into metadata
plot_df <- tibble(
  name = names(plot_list),
  plot = plot_list
) %>%
  separate(name, into = c("habitat", "site_type", "site"), sep = " \\| ")

# Step 2: Nest by habitat × site_type
grouped <- plot_df %>%
  group_by(habitat, site_type) %>%
  group_modify(~ tibble(patch = list(plot_grid(plotlist = .x$plot, nrow = ceiling(length(.x$plot) / 4))))) %>% 
  ungroup()

# Step 3: Rebuild grid with labels
# One row per habitat, columns = MPA/Reference
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

# Step 4: Add top column labels
col_labels <- plot_grid(
  NULL,
  plot_grid(plotlist = map(site_types, ~ ggdraw() + draw_label(.x, fontface = "bold", hjust = 0.5)),
            nrow = 1),
  nrow = 2,
  rel_heights = c(0.1, 1)
)

# Step 5: Combine everything
final_plot <- plot_grid(col_labels, plot_grid(plotlist = rows, ncol = 1), nrow = 2, rel_heights = c(0.1, 1))

# Done!
final_plot

plot_list[1]

