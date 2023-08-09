# Explore lobster block data

# Setup ------------------------------------------------------------------------
# Clear workspace
rm(list = ls())

## Packages ----
library(tidyverse)

## Directories ----
basedir <- "/Users/lopazanski/Library/CloudStorage/GoogleDrive-lopazanski@ucsb.edu/Shared drives/NCEAS MPA network assessment/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
datadir <- "/Users/lopazanski/Library/CloudStorage/GoogleDrive-lopazanski@ucsb.edu/My Drive/Research/NCEAS - California MPA Working Group/fisheries-data/raw"
plotdir <- "analyses/2performance_fisheries/figures"
outdir <- "/Volumes/GoogleDrive-105151121202188525604/My Drive/Research/NCEAS - California MPA Working Group/fisheries-data/processed"

## Read Data ----
landings <- readRDS(file.path(datadir, "CDFW_2000_2020_landings_receipts.Rds"))
port_key <- readRDS(file.path(datadir, "CDFW_port_key.Rds"))
blocks <- wcfish::blocks 


# Build ------------------------------------------------------------------------
# lobster
lobster <- landings %>% 
  filter(species_id == 820)

# how many vessels?
length(unique(lobster$vessel_id))
# 502

# how many fishers?
length(unique(lobster$fisher_id))
# 407

# how many business ids?
length(unique(lobster$business_id))
# 387


# Has the average price changed?
avg_annual <- lobster %>% 
  group_by(year) %>% 
  summarize(avg_price = mean(price_usd_lb, na.rm = T),
            unique_vessel = length(unique(vessel_id)),
            n = n())

# Note: this does not exclude personal consumption (price = 0 usd)

# Plot 
# Theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=10),
                   strip.text=element_text(size=10),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

ggplot()+
  geom_point(data = avg_annual_price,
             aes(x = year, y = avg_price)) +
  geom_vline(xintercept = 2017, color = "purple") +
  annotate(geom="text", x = 2018.4, y = 3, label = "Trap Limit", color = "purple")+
  geom_vline(xintercept = 2012, color = "red") +
  annotate(geom="text", x = 2014, y = 3, label = "MPA Network", color = "red") +
  labs(x = "Year",
       y = "Average price per pound (USD)") +
  my_theme

# How many blocks does each vessel fish in?
vessel_patterns <- lobster %>% 
  group_by(vessel_id) %>% 
  summarize(n_years = length(unique(year)),
            n_days = length(unique(date)),
            n_obs = n(),
            n_blocks = length(unique(block_id)))

ggplot(data = vessel_patterns %>% 
         filter(n_years > 5)) +
  geom_histogram(aes(x = n_blocks))

ggplot(data = vessel_patterns %>% 
         filter(n_years > 5) %>% 
         filter(n_days > 21)) +
  geom_histogram(aes(x = n_days))

ggplot(data = vessel_patterns) +
  geom_jitter(aes(x = n_years, y = n_blocks))

# How many vessels in each block each year?
block_patterns <- lobster %>% 
  group_by(block_id, year) %>% 
  summarize(n_vessels = length(unique(vessel_id)))

# Compare lobster catch in MPA and non-MPA blocks -------------------------------

# Read block stats
block_stats <- readRDS("analyses/2performance_fisheries/analyses/blocks/block_all_covariates.Rds") 

blocks_comb <- blocks %>%
  left_join(., block_stats) %>% 
  select(block_id, block_treatment) %>% 
  mutate(block_treatment = if_else(is.na(block_treatment), 0, 1))

# Summarize across vessels
lobster_vessel <- lobster %>% 
  filter(gear == "Crab or lobster trap") %>% 
  filter(vessel_id > 0) %>% 
  group_by(vessel_id, year, block_id) %>% 
  summarize(catch = sum(landings_lb, na.rm = T),
            receipts = n(),
            days = length(unique(date))) %>% 
  mutate(catch_per_day = catch/days)

# Create dataframe
data <- left_join(lobster_vessel, blocks_comb, by = "block_id")

# Create catch summaries 
# Across all years - total catch per vessel day
data2 <- data %>% 
  group_by(vessel_id, block_id) %>% 
  summarize(total_catch = sum(catch_per_day)) %>% 
  group_by(block_id) %>% 
  summarize(mean_catch = mean(total_catch, na.rm = T))

# Add geom
data3 <- left_join(data2, blocks_comb) %>% 
  drop_na()

# Plot -------
  
# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# MPA regions
# CA/OR, Alder Creek, Pigeon Point, Point Conception, CA/MEX 
region_lats <- c(39.0, 37.18, 34.5)

my_theme <-  theme(axis.text=element_text(size=6),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_blank(),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = "none",
                   legend.key.size = unit(0.3, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))




ggplot() +
  # Plot the blocks
  geom_sf(data = blocks_comb, color = "grey60", lwd = 0.1, alpha = 0.3) +
  # Plot treatment blocks
  #geom_sf(data = blocks_comb %>% 
 #           filter(block_treatment == 1), color = "white", lwd = 0.5, alpha = 0.3) +
  # Plot catch
  geom_sf(data = data3, mapping = aes(fill = mean_catch, geometry = geometry)) +
  # Plot treatment blocks
  geom_sf(data = blocks_comb %>% filter(block_treatment == 1), 
          color = "white", lwd = 0.6, alpha = 0.3) +
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  geom_hline(mapping=aes(yintercept=region_lats), color="grey60", linetype="dotted") +
  coord_sf(xlim = c(-125, -117), ylim = c(32.5, 42)) 


g <- ggplot() +
  # Plot blocks
  geom_sf(data=data, mapping=aes(fill=pair), color="grey60", lwd=0.1, alpha=0.3) + # block_type
  # Plot regions
  geom_hline(mapping=aes(yintercept=region_lats), color="grey60", linetype="dotted") +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot lines
  geom_path(data=pair_lines, mapping=aes(x=block_long_dd, y=block_lat_dd, group=pair, color=pair)) +
  # Labels 
  labs(x="", y="") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  # Legend
  # scale_fill_manual(name="Block type", values=viridis(2)) +
  scale_fill_manual(values=colors ) +
  scale_color_manual(values=colors ) +
  # Crop
  coord_sf(xlim = c(-125, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + my_theme
g

# # Export
# ggsave(g, filename=file.path(plotdir, "FigX_matched_blocks.png"), 
#        width=3.5, height=4.8, units="in", dpi=600)


# Zoom
g <- ggplot() +
  # Plot blocks
  geom_sf(data=data, mapping=aes(fill=pair), color="grey60", lwd=0.1, alpha=0.3) + # block_type
  # Plot regions
  geom_hline(mapping=aes(yintercept=region_lats), color="grey60", linetype="dotted") +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot lines
  geom_path(data=pair_lines, mapping=aes(x=block_long_dd, y=block_lat_dd, group=pair, color=pair)) +
  # Labels 
  labs(x="", y="") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  # Legend
  # scale_fill_manual(name="Block type", values=viridis(2)) +
  scale_fill_manual(values=colors ) +
  scale_color_manual(values=colors ) +
  # Crop
  coord_sf(xlim = c(-121, -117), ylim = c(32.5, 35)) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_matched_blocks_south.png"), 
       width=5, height=4, units="in", dpi=600)
