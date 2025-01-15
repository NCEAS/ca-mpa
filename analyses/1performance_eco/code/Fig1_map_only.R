

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse, sf)

#NOTE: maptools is required, but has been removed from CRAN. To install the 
#archived tool use:

package_url <- "https://cran.r-project.org/src/contrib/Archive/maptools/maptools_1.1-8.tar.gz"
temp_dir <- tempdir()
download.file(package_url, destfile = paste0(temp_dir, "/maptools_1.1-8.tar.gz"))
install.packages(paste0(temp_dir, "/maptools_1.1-8.tar.gz"), repos = NULL, type = "source")

#ggsn is available from:
devtools::install_github('oswaldosantos/ggsn')


# Directories
basedir <- here::here("analyses","1performance_eco","output")
aurora <- "/home/shares/ca-mpa/data/sync-data/"
gisdir <- "/home/shares/ca-mpa/data/sync-data/gis_data/processed"
plotdir <- here::here("analyses","1performance_eco","figures")

# Read data
mpa_dat <- readRDS(file.path(basedir, "biomass_richness_diversity.Rds"))

unique(mpa_dat$state_name)

# Read data
state_waters_poly <- readRDS(file.path(gisdir, "CA_state_waters_polygons.Rds"))
state_waters_line <- readRDS(file.path(gisdir, "CA_state_waters_polyline.Rds"))
mpas_orig <- readRDS(file.path(aurora, "mpa_traits/processed", "CA_mpa_metadata.Rds"))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")


################################################################################


# Format sites
sites <- mpa_dat %>% 
  #match case with mpas_orig
  mutate(habitat = factor(habitat, levels = c("Surf zone","Kelp forest","Shallow reef","Deep reef")),
         state_region = factor(state_region, levels = c("North Coast","North Central Coast","Central Coast","South Coast")),
         state_name = factor(state_name),
         state_name = str_to_title(state_name),
         state_name = sub("([A-Za-z]+)$", "\\U\\1", state_name, perl = TRUE),
         #rename MPAs to match mpa_orig for join
         state_name = ifelse(state_name == "Campus Point SMCA","Campus Point SMCA (No-Take)",
                                 ifelse(state_name == "Blue Cavern Onshore SMCA","Blue Cavern Onshore SMCA (No-Take)",
                                        ifelse(state_name == "Point Vicente SMCA","Point Vicente SMCA (No-Take)",state_name)))
         
         )%>%
  #join spatial data with each mpa
  left_join(mpas_orig, by = c("state_name" = "mpa")) %>%
  dplyr::select(habitat, year, state_region,state_name, 
                mpa_full, mpa_short, mlpa, authority, type, ccr, ccr_int, region_code, 
                block_id, area_sqkm, long_dd, lat_dd, everything())

# Check names
sites_in_data <- sort(unique(sites$state_name))
sites_in_data[!sites_in_data %in% mpas_orig$state_name]


# Summarize
################################################################################

# Build data
habitats_per_mpa <- sites %>%
  group_by(state_region, state_name) %>%
  summarise(unique_habitats = n_distinct(habitat))


# MPAs
mpas <- mpas_orig %>% 
  # Reduce to MPAs of interest
  filter(mlpa=="MLPA") %>% 
  rename(state_name = mpa)%>%
  # Add stats
  left_join(habitats_per_mpa)

# Subset
mpas_zero <- mpas %>% filter(is.na(unique_habitats))
mpas_data <- mpas %>% filter(!is.na(unique_habitats))


# Plot data
################################################################################

# MPA regions
# CA/OR, Alder Creek, Pigeon Point, Point Conception, CA/MEX 
region_lats <- c(39.0, 37.18, 34.5)

# Region labels
region_labels <- tibble(long_dd=c(-123.9, -122.9, -121, -118#, -119.5
                                  ),
                        lat_dd=c(40.5, 38.7, 36, 34.1#, 34.8
                                 ),
                        label=c("North\n(Dec 2012)", "North Central\n(May 2010)", "Central\n(Sep 2007)", "South\n(Jan 2012)"#, "N. Channel\nIslands (2003)"
                                ))

# Theme
base_theme <-  theme(axis.text=element_text(size=7, color = "black"),
                     axis.title=element_text(size=8,color = "black"),
                     legend.text=element_text(size=7,color = "black"),
                     legend.title=element_text(size=8,color = "black"),
                     plot.tag=element_text(size=8,color = "black"),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.key = element_rect(fill=alpha('blue', 0)),
                     legend.background = element_rect(fill=alpha('blue', 0)),
                     #facets
                     strip.text = element_text(size=7, face = "bold",color = "black")
                     )

# Build inset
g1_inset <-  ggplotGrob(
  ggplot() +
    # Plot land
    geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
    geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
    # Plot box
    annotate("rect", xmin=-126, xmax=-117, ymin=32.5, ymax=42, color="black", fill=NA, lwd=0.8) +
    #add north arrow
    ggsn::north(x.min = -130, x.max = -105, 
                y.min = 29, y.max = 52,
                location = "topright", 
                scale = 0.3, 
                symbol = 10)+
    # Labels
    labs(x="", y="") +
    # Crop
    coord_sf(xlim = c(-130, -105), ylim = c(29, 52)) +
    #add USA text
    geom_text(data = data.frame(x = -110, y = 38, label = "USA"), aes(x = x, y = y, label = label), color = "black", size = 3.5)+
    # Theme
    theme_bw() + base_theme +
    theme( plot.margin = unit(rep(0, 4), "null"),
           panel.margin = unit(rep(0, 4), "null"),
           panel.background = element_rect(fill='transparent'), #transparent panel bg
           # plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
           axis.ticks = element_blank(),
           axis.ticks.length = unit(0, "null"),
           axis.ticks.margin = unit(0, "null"),
           axis.text = element_blank(),
           axis.title=element_blank())
)

g1_inset

# Plot data
g1 <- ggplot() +
  # Plot regions
  #geom_hline(mapping=aes(yintercept=region_lats)) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot state waters
  geom_sf(data=state_waters_line, color="grey40", lwd=0.1) +
  # Plot MPAs
  geom_point(data=mpas_data, mapping=aes(x=long_dd, y=lat_dd, size=unique_habitats), color="grey20") +
  #geom_point(data=mpas_zero, mapping=aes(x=long_dd, y=lat_dd), shape="x", size=3) +
  # Plot region labels
  geom_text(data=region_labels, mapping=aes(x=long_dd, y=lat_dd, label=label, fontface="bold"), hjust=0, size=2.3) +
  # Labels
  labs(x="", y="") +
  scale_size_continuous(name="Ecosystems \nmonitored") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  #add scale bar
  ggsn::scalebar(x.min = -126.7, x.max = -118, 
                 y.min = 32.5, y.max = 42,
                 #anchor=c(x=-124.7,y=41),
                 location="bottomleft",
                 dist = 100, dist_unit = "km",
                 transform=TRUE, 
                 model = "WGS84",
                 st.dist=0.018,
                 st.size=3,
                 border.size=.5,
                 height=.01
  )+
  # Crop
  coord_sf(xlim = c(-126.5, -117), ylim = c(32.5, 42)) +
  #add inset
  annotation_custom(grob = g1_inset, 
                    xmin = -126.98, 
                    xmax = -125,
                  ymin = 40.55)+
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title=element_blank(),
        legend.position = c(0.13, 0.2),
        legend.key.size = unit(0.4, "cm"))
g1

# Export figure
ggsave(g1, filename=file.path(plotdir, "Fig1_map_only2.png"), 
       width=4, height=6, units="in", dpi=600)






