

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(rinat)
library(tidyverse)

# Directories
indir <- "data/inaturalist/raw"
outdir <- "data/inaturalist/processed"
plotdir <- "data/inaturalist/figures"

# Read MPAs
mpas <- readRDS("data/gis_data/processed/CA_MPA_polygons_smrs_smcas.Rds")
mpas_simple <- mpas %>% select(name)
mpas_simple_sp <- mpas_simple %>% as(., "Spatial")

# Read iNaturalist data
data_orig <- readRDS(file=file.path(outdir, "2000_2020_inaturalist_data.Rds"))


# Read data
################################################################################

# Convert to sf
data_sf <- data_orig %>%
  sf::st_as_sf(coords=c("long_dd", "lat_dd"), crs=sf::st_crs(mpas))

# Convert to sp
data_sp <- data_sf %>%
  as(., "Spatial")

# Find points inside MPAs
inside_which_mpa <- sp::over(data_sp, mpas_simple_sp)
inside_which_mpa_chr <- inside_which_mpa$name

# Add MPA inside to dataframe
data_sf1 <- data_sp %>%
  as("sf") %>%
  mutate(mpa=inside_which_mpa_chr)

# Convert to dataframe
data_sf1_df <- data_sf1 %>%
  sf::st_drop_geometry()

# Export data
saveRDS(data_sf1_df, file.path(outdir, "2000_2020_inaturalist_data_inside_mpas.Rds"))


# Build data
################################################################################

# Stats
stats <- data_sf1_df %>%
  # Points INSIDE MPAs
  filter(!is.na(mpa)) %>%
  # Number by year and category
  group_by(year_obs, taxa_catg) %>%
  summarize(n=n(),
            n_users=n_distinct(user_id)) %>%
  ungroup() %>%
  # Calculate proportion
  group_by(year_obs) %>%
  mutate(prop=n/sum(n)) %>%
  ungroup()


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.3, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot(stats, aes(x=year_obs, y=n, fill=taxa_catg)) +
  geom_bar(stat="identity", color="grey30", lwd=0.2) +
  # Labels
  labs(x="Year", y="Number of submissions") +
  scale_fill_discrete(name="Taxa type") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.2, 0.6))
g1

# Plot data
g2 <- ggplot(stats, aes(x=year_obs, y=prop, fill=taxa_catg)) +
  geom_bar(stat="identity", color="grey30", lwd=0.2) +
  # Labels
  labs(x="Year", y="Proportion of submissions") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g2

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, heights=c(0.7, 0.3))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_inat_submissions_over_time.png"),
       width=6.5, height=4.5, units="in", dpi=600)
