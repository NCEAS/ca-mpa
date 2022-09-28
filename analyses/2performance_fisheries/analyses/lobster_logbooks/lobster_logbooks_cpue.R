

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
plotdir <- "analyses/2performance_fisheries/analyses/lobster_logbooks/figures" 

# Read logbook data
data_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/california/cdfw_data/data/confidential/lobster_logbooks/processed/CDFW_2000_2020_logbook_data.Rds")

# MPAs
mpas <- wcfish::mpas_ca %>% 
  sf::st_as_sf() %>% 
  filter(type!="SMP")

# Projections
utm11 <- "+proj=utm +zone=11 +datum=NAD83"

# # Helper function
# ################################################################################
# 
# # Distance to nearest MPA
# lat <- 32; long <- -118
# calc_mpa_near <- function(lat, long){
#   
#   # Convert lat/long to pt
#   pt_df <- tibble(lat=lat, long=long)
#   pt_sf <- sf::st_as_sf(pt_df, coords=c("long", "lat"), crs=sf::st_crs(mpas))
#   
#   # Identify closest MPA
#   near_id <- sf::st_nearest_feature(pt_sf, mpas)
#   near_name <- mpas$name[near_id]
#   near_sf <- mpas %>% 
#     filter(name==near_name)
#   
#   # Identify distance to closest MPA
#   near_m <- sf::st_distance(pt_sf, near_sf) %>% as.numeric()
#   near_km <- near_m / 1000
#   
#   # Merge
#   out <- tibble(nearest_mpa=near_name,
#                 nearest_mpa_km=near_km)
#   return(out)
# 
# }
# 
# 
# # Format data
# ################################################################################
# 
# # Format data
# data <- data_orig %>% 
#   # Reduce to data with lat/longs
#   filter(!is.na(lat_dd) & !is.na(long_dd) & !is.na(n_kept) & !is.na(n_traps_pulled) & !is.na(n_nights)) %>% 
#   # Compute CPUE
#   mutate(trap_nights=n_traps_pulled * n_nights,
#          cpue=n_kept/trap_nights)
# 
# # Compute nearest MPA and distance
# x <- 1
# near_key <- purrr::map_df(1:nrow(data), function(x){
#   
#   lat <- data$lat_dd[x]
#   long <- data$long_dd[x]
#   out <- calc_mpa_near(lat, long)
#   
# })
# 
# # Add MPA stats to data
# data2 <- cbind(data, near_key)
# 
# 
# # Plot data
# ################################################################################
# 
# # Plot data
# g <- ggplot(data2, aes(x=nearest_mpa_km, y=cpue)) +
#   geom_point() +
#   # Labels
#   labs(x="Distance to nearest MPA (km)", y="Catch per trap-night") +
#   # Theme
#   theme_bw() 
# g


# Build data
################################################################################

# Format data
data <- data_orig %>% 
  # Reduce to data with lat/longs
  filter(!is.na(lat_dd) & !is.na(long_dd) & !is.na(n_kept) & !is.na(n_traps_pulled) & !is.na(n_nights)) %>% 
  # Compute CPUE
  mutate(trap_nights=n_traps_pulled * n_nights,
         cpue=n_kept/trap_nights)

# Convert to SP
data_sp <- sf::st_as_sf(data, coords=c("long_dd", "lat_dd"), crs=sf::st_crs(mpas)) %>% 
  sf::st_transform(crs=utm11) %>% 
  # slice(1:10) %>% 
  sf::as_Spatial()

# Convert MPAs to SP
mpas_sp <- mpas %>% 
  sf::st_transform(crs=utm11) %>% 
  sf::as_Spatial()

# Calculate distance of each point to an MPA
dist_mat <- rgeos::gDistance(data_sp, mpas_sp, byid=T)

# Format distance matrix
dist_df <- dist_mat %>% 
  as.data.frame() %>% 
  # Add MPA id
  mutate(mpa=mpas$name) %>% 
  select(mpa, everything()) %>% 
  # Gather
  gather(key="row_id", value="dist_m", 2:ncol(.)) %>% 
  # Find closest MPA to each point
  group_by(row_id) %>% 
  arrange(dist_m) %>% 
  slice(1) %>% 
  ungroup() %>% 
  # Convert row id to number and sort
  mutate(row_id=as.numeric(row_id)) %>% 
  arrange(row_id) %>% 
  select(row_id, mpa, dist_m)

# Add distance back onto data
data1 <- data %>% 
  cbind(dist_df %>% select(mpa, dist_m)) %>% 
  # Add distance in kilometers
  mutate(dist_km=dist_m/1000) %>% 
  # Remove outliers
  filter(dist_km <=5 & cpue<=5) %>% 
  # Reduce to MPAs with more than 50 observations
  group_by(mpa) %>% 
  mutate(mpa_n_obs=n()) %>% 
  ungroup() %>% 
  filter(mpa_n_obs>=50)



# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data1, aes(x=dist_m/1000, y=cpue)) +
  facet_wrap(~mpa) +
  geom_point(pch=21, color="grey50", alpha=0.5) +
  geom_smooth(method = "loess", fill="grey40", color="black") +
  # Labels
  labs(x="Distance to nearest MPA (km)", y="Catch per trap-night") +
  # Limits
  scale_x_continuous(lim=c(0, 5)) +
  scale_y_continuous(lim=c(0, 5)) +
  # Theme
  theme_bw() + my_theme
g




# Plot data
################################################################################


# Plot MPAs
g <- geo





