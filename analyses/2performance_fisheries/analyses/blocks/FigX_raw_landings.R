# Explore landings by species and species groups

# Read data #################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(janitor)
library(sf)
library(rfishbase)

# Directories
basedir <- "/Users/lopazanski/Library/CloudStorage/GoogleDrive-lopazanski@ucsb.edu/Shared drives/NCEAS MPA network assessment/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
gisdir <- file.path(basedir, "gis_data/processed")
datadir <- "analyses/2performance_fisheries/analyses/blocks/data"
fishdir <- "/Users/lopazanski/Documents/github/nceas/CDFW-fishing-data"

# Read data
mpas <- readRDS(file.path(gisdir, "CA_MPA_polygons.Rds"))
blocks <- readRDS(file.path(datadir, "blocks_by_mlpa_region_w_mpa_stats.Rds"))
landings_raw <- readRDS(file.path(fishdir, "CDFW_2000_2020_landings_receipts.Rds"))
#sp_key <- readRDS(file.path(fishdir, "CDFW_species_key.Rds"))# This seems incomplete
sp_key <- read_csv(file.path(fishdir, "CDFW_species_key.csv"))

# Load fishbase taxa info & append to speacies key
fishbase <- load_taxa(collect = T) 
sp_key_new <- left_join(sp_key, fishbase, by = c("sci_name" = "Species"))

# Clean landings data ##########################################################
landings <- landings_raw %>% 
  # remove invalid blocks
  filter(!(block_type %in% c("Invalid"))) 

# Explore ######################################################################
# Number of unique vessels in entire dataset
length(unique(landings$vessel_id)) #5938

# Number of unique fishers in entire dataset
length(unique(landings$fisher_id)) #7906

# Number of different gear specifications
length(unique(landings$gear)) #76
length(unique(landings$gear_type)) #12


# Number of receipts with unknown/NA vessels in dataset
vessel_na <- landings %>% 
  filter(vessel_id == -1) #23891

# Total catch by gear type for entire dataset
gear_catch <- landings %>% 
  group_by(gear_type) %>% 
  summarize(total_lbs = sum(landings_lb, na.rm = T))

# Number of species in landings
length(unique(landings$species_id)) # 352

# Total landings in entire dataset
total_lbs <- sum(landings$landings_lb, na.rm = T)         

# Total landings per species with percent of total landings
species_totals <- landings %>% 
  group_by(species) %>% 
  summarize(total_lb = sum(landings_lb, na.rm = T),
            pct = round(total_lb/total_lbs*100,4)) %>% 
  arrange(-total_lb) %>% 
  mutate(cum_sum = cumsum(pct))

# Create species groupings #####################################################

## 1. Coastal Pelagic Species: anchovy, sardine, mackerel ----
cps <- sp_key %>% 
  filter(grepl("anchovy|sardine|mackerel", comm_name))

cps_totals <- landings %>% 
  filter(species_id %in% cps$spp_code_num) %>% 
  group_by(species) %>% 
  summarize(total_lb = sum(landings_lb, na.rm = T),
            pct = round(total_lb/total_lbs*100,4)) %>% 
  arrange(-total_lb)

cps_lbs <- sum(cps_totals$total_lb)

cps_gears <- landings %>% 
  filter(species_id %in% cps$spp_code_num) %>% 
  group_by(gear_type) %>% 
  summarize(total_lb = sum(landings_lb, na.rm = T),
            pct = round(total_lb/cps_lbs*100,4))

## 2. Salmonids -----
salmon <- sp_key %>% filter(grepl("Salmon|salmon", comm_name)) %>% 
  filter(!(grepl("hark", comm_name))) %>% 
  filter(!(grepl("roe", comm_name)))

salmon_totals <- landings %>% 
  filter(species_id %in% salmon$spp_code_num) %>% 
  group_by(species) %>% 
  summarize(total_lb = sum(landings_lb, na.rm = T),
            pct = round(total_lb/total_lbs*100,4)) %>% 
  arrange(-total_lb)

## 3. Groundfish -----

### a. Leopard shark - sp code 153 -----

### b. Sanddabs (speckled, Packific, longfin, unspecified) -----
gf_sand <- sp_key %>% filter(grepl("Sanddab|sanddab", comm_name))

gf_sand_totals <- landings %>% 
  filter(species_id %in% gf_sand$spp_code_num) %>% 
  group_by(species) %>% 
  summarize(total_lb = sum(landings_lb, na.rm = T),
            pct = round(total_lb/total_lbs*100,4)) %>% 
  arrange(-total_lb)

### c. Flounder & turbots -----
# (flounder - arrowtooth, starry, unspecified; turbots — curlfin, diamond, hornyhead, spotted, unspecified)
gf_flound <- sp_key %>%  filter(grepl("Flounder|flounder|Turbot|turbot", comm_name))

gf_flound_totals <- landings %>% 
  filter(species_id %in% gf_flound$spp_code_num) %>% 
  group_by(species) %>% 
  summarize(total_lb = sum(landings_lb, na.rm = T),
            pct = round(total_lb/total_lbs*100,4)) %>% 
  arrange(-total_lb)

### d. Soles (except Petrale, Dover, Rex) -----
gf_sole <- sp_key %>% filter(grepl("Sole|sole", comm_name)) %>% 
  filter(!(grepl("Petrale", comm_name))) %>% 
  filter(!(grepl("Dover", comm_name))) %>% 
  filter(!(grepl("Rex", comm_name)))

### e. CA Halibut (222) ----

### f. Cabezon, scorpionfish, & sculpin -----
gf_scorp <- sp_key %>% 
  filter(grepl("Cabezon|scorp|sculp", comm_name))

### g. Lingcod (195) ----

### h. Petrale sole (209) ----

### i. Pacific halibut (221) ----

### j. North pacific hake (whiting) (495) ----

### k. Rockfish spp. ----
gf_rock <- sp_key %>% 
  filter(grepl("rockfish", comm_name))
## NOTE: 2 rockfish are labeled "Sabastes" genus (likely need to correct)

gf_rock_totals <- landings %>% 
  filter(species_id %in% gf_rock$spp_code_num) %>% 
  group_by(species) %>% 
  summarize(total_lb = sum(landings_lb, na.rm = T),
            pct = round(total_lb/total_lbs*100,4)) %>% 
  arrange(-total_lb)

gf_rock_lbs <- sum(gf_rock_totals$total_lb)

gf_rock_gears <- landings %>% 
  filter(species_id %in% gf_rock$spp_code_num) %>% 
  group_by(gear_type) %>% 
  summarize(total_lb = sum(landings_lb, na.rm = T),
            pct = round(total_lb/gf_rock_lbs*100,4))


### l. Sablefish (190) ----

### m. Spiny dogfish (152) ----

### n. Soles (dover, rex) ----
gf_dover_rex <- sp_key %>% 
  filter(grepl("Dover|Rex", comm_name))

### o. Thornyheads ----
gf_thorny <- sp_key %>% 
  filter(grepl("Thorny|thorny", comm_name))

## 4. Gamefish ----

### a. Seabass (Serranidae, Epinephelinae) ----
game_bass <- sp_key_new %>% 
  filter(grepl("Serranid|Epinephelid", Family)|grepl("Paralabrax", sci_name)|grepl("rouper",comm_name))

### b. Bonito (3) ----

### c. Sheephead (145) -----

### d. Barracuda (130) ----

## 5. Highly Migratory Species ----

### a. Swordfish ----
hms_sword <- sp_key %>% filter(grepl("Swordfish", comm_name))

### b. Tunas ----
hms_tuna <- sp_key %>% filter(grepl("tuna", comm_name)|grepl("Thun", sci_name))

### c. Thresher (Alopias spp)
hms_thre <- sp_key %>% filter(grepl("Alop", sci_name))

### d. Shortfin mako (Isurus oxyrinchus) (151) ----

### e. Blue shark (Prionace glauca) (167) ----

### f. Hammerhead sharks (Sphyrna spp.) (158) ----

## 6. Market Squid (711) ----

## 7. Echinoderms ----

### a. Urchin ----
urchin <- sp_key %>% filter(grepl("urch", comm_name))

### b. Sea cucumber ----
cukes <- sp_key %>% filter(grepl("cucu", comm_name))

## 8. Dungeness Crab (800) ----

## 9. Other crustaceans ----

### a. Rock crab ----
oth_crab <- sp_key %>% filter(grepl("Cancer", sci_name))

### b. Spiny lobster (820) ----

### c. Spot prawn (815) ----

### d. Pink shrimp (812) ----


# Visualize blocks with MPAs ####################################################
# MPA regions
# CA/OR, Alder Creek, Pigeon Point, Point Conception, CA/MEX 
region_lats <- c(42, 39.0, 37.18, 34.5, 32.5) %>% rev()

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")
ca <- usa %>% filter(name == "California")

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
ggplot() +
  # Blocks
  geom_sf(data=blocks %>% 
            filter(!(block_type == "Offshore") & block_id < 900), 
          mapping=aes(alpha=mpa_yn), lwd=0.2) +
  # MPAS
  geom_sf(data = mpas,
          mapping=aes(fill = type), alpha = 0.5) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Legend
  scale_fill_discrete(name="MPA type") +
  scale_alpha_manual(name="Block type", values=c(1, 0.2)) +
  # Crop
  coord_sf(xlim = c(-122.75, -121.4), ylim = c(36, 37.2)) +
  # Theme
  theme_bw() + my_theme 


# Plot data

offshore_blocks <- blocks %>% 
  filter(block_type == "Offshore") 

test <- st_snap(offshore_blocks, ca)
  
  
ggplot() +
  # Blocks
  geom_sf(data=offshore_blocks %>% filter(block_state == "California"), 
          mapping=aes(fill=mlpa_region), lwd=0.2) +
  geom_sf(data=blocks %>% filter(block_state == "California"), 
          mapping=aes(fill=mlpa_region), alpha = 0.8, lwd=0.2) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Legend
  scale_fill_discrete(name="MLPA region") +
  scale_alpha_manual(name="Block type", values=c(1, 0.2)) +
  # Crop
  # Theme
  theme_bw() + my_theme 
  
# Landings Plots ###############################################################

## CPS ----
ggplot() +
  # Blocks
  geom_sf(data = blocks, lwd = 0.2) +
  # Landings 
  geom_sf(data = landings %>% 
            filter(species_id %in% cps$spp_code_num) %>% 
            group_by(block_id) %>% 
            summarize(total_lbs = sum(landings_lb, na.rm = T)/1000) %>% 
            left_join(., blocks) %>% 
            st_as_sf(), aes(fill = total_lbs)) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  theme_bw() + my_theme + 
  labs(title = "Coastal pelagic species (anchovy, sardine, mackerel)",
       fill = "Thousand lbs.")

## Salmonids ----
ggplot() +
  # Blocks
  geom_sf(data = blocks, lwd = 0.2) +
  # Landings 
  geom_sf(data = landings %>% 
            filter(species_id %in% salmon$spp_code_num) %>% 
            group_by(block_id) %>% 
            summarize(total_lbs = sum(landings_lb, na.rm = T)/1000) %>% 
            left_join(., blocks) %>% 
            st_as_sf(), aes(fill = total_lbs)) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  theme_bw() + my_theme + 
  labs(title = "Salmonids",
       fill = "Thousand lbs.")

## Groundfish ----
ggplot() +
  # Blocks
  geom_sf(data = blocks, lwd = 0.2) +
  # Landings 
  geom_sf(data = landings %>% 
            filter(species_id == 153) %>% 
            group_by(block_id) %>% 
            summarize(total_lbs = sum(landings_lb, na.rm = T)/1000) %>% 
            left_join(., blocks) %>% 
            st_as_sf(), aes(fill = total_lbs)) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  theme_bw() + my_theme + 
  labs(title = "Leopard shark",
       fill = "Thousand lbs.")

ggplot() +
  # Blocks
  geom_sf(data = blocks, lwd = 0.2) +
  # Landings 
  geom_sf(data = landings %>% 
            filter(species_id %in% gf_sand$spp_code_num) %>% 
            group_by(block_id) %>% 
            summarize(total_lbs = sum(landings_lb, na.rm = T)/1000) %>% 
            left_join(., blocks) %>% 
            st_as_sf(), aes(fill = total_lbs)) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  theme_bw() + my_theme + 
  labs(title = "Sanddabs (speckled, Pacific, longfin, unspecified)",
       fill = "Thousand lbs.")


ggplot() +
  # Blocks
  geom_sf(data = blocks, lwd = 0.2) +
  # Landings 
  geom_sf(data = landings %>% 
            filter(species_id %in% gf_flound$spp_code_num) %>% 
            group_by(block_id) %>% 
            summarize(total_lbs = sum(landings_lb, na.rm = T)/1000) %>% 
            left_join(., blocks) %>% 
            st_as_sf(), aes(fill = total_lbs)) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  theme_bw() + my_theme + 
  labs(title = "Flounder & turbots",
       fill = "Thousand lbs.")


ggplot() +
  # Blocks
  geom_sf(data = blocks, lwd = 0.2) +
  # Landings 
  geom_sf(data = landings %>% 
            filter(species_id %in% gf_sole$spp_code_num) %>% 
            group_by(block_id) %>% 
            summarize(total_lbs = sum(landings_lb, na.rm = T)/1000) %>% 
            left_join(., blocks) %>% 
            st_as_sf(), aes(fill = total_lbs)) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  theme_bw() + my_theme + 
  labs(title = "Soles (excluding Petrale, Dover, Rex)",
       fill = "Thousand lbs.")

ggplot() +
  # Blocks
  geom_sf(data = blocks, lwd = 0.2) +
  # Landings 
  geom_sf(data = landings %>% 
            filter(species_id == 222) %>% 
            group_by(block_id) %>% 
            summarize(total_lbs = sum(landings_lb, na.rm = T)/1000) %>% 
            left_join(., blocks) %>% 
            st_as_sf(), aes(fill = total_lbs)) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  theme_bw() + my_theme + 
  labs(title = "California halibut",
       fill = "Thousand lbs.")

ggplot() +
  # Blocks
  geom_sf(data = blocks, lwd = 0.2) +
  # Landings 
  geom_sf(data = landings %>% 
            filter(species_id %in% gf_scorp$spp_code_num) %>% 
            group_by(block_id) %>% 
            summarize(total_lbs = sum(landings_lb, na.rm = T)/1000) %>% 
            left_join(., blocks) %>% 
            st_as_sf(), aes(fill = total_lbs)) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  theme_bw() + my_theme + 
  labs(title = "Cabezon, sculpin, scorpionfish",
       fill = "Thousand lbs.")

ggplot() +
  # Blocks
  geom_sf(data = blocks, lwd = 0.2) +
  # Landings 
  geom_sf(data = landings %>% 
            filter(species_id == 195) %>% 
            group_by(block_id) %>% 
            summarize(total_lbs = sum(landings_lb, na.rm = T)/1000) %>% 
            left_join(., blocks) %>% 
            st_as_sf(), aes(fill = total_lbs)) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  theme_bw() + my_theme + 
  labs(title = "Lingcod",
       fill = "Thousand lbs.")

ggplot() +
  # Blocks
  geom_sf(data = blocks, lwd = 0.2) +
  # Landings 
  geom_sf(data = landings %>% 
            filter(species_id == 209) %>% 
            group_by(block_id) %>% 
            summarize(total_lbs = sum(landings_lb, na.rm = T)/1000) %>% 
            left_join(., blocks) %>% 
            st_as_sf(), aes(fill = total_lbs)) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  theme_bw() + my_theme + 
  labs(title = "Petrale sole",
       fill = "Thousand lbs.")

ggplot() +
  # Blocks
  geom_sf(data = blocks, lwd = 0.2) +
  # Landings 
  geom_sf(data = landings %>% 
            filter(species_id == 221) %>% 
            group_by(block_id) %>% 
            summarize(total_lbs = sum(landings_lb, na.rm = T)/1000) %>% 
            left_join(., blocks) %>% 
            st_as_sf(), aes(fill = total_lbs)) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  theme_bw() + my_theme + 
  labs(title = "Pacific halibut",
       fill = "Thousand lbs.")

ggplot() +
  # Blocks
  geom_sf(data = blocks, lwd = 0.2) +
  # Landings 
  geom_sf(data = landings %>% 
            filter(species_id == 495) %>% 
            group_by(block_id) %>% 
            summarize(total_lbs = sum(landings_lb, na.rm = T)/1000) %>% 
            left_join(., blocks) %>% 
            st_as_sf(), aes(fill = total_lbs)) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  theme_bw() + my_theme + 
  labs(title = "Pacific whiting",
       fill = "Thousand lbs.")

ggplot() +
  # Blocks
  geom_sf(data = blocks, lwd = 0.2) +
  # Landings 
  geom_sf(data = landings %>% 
            filter(species_id %in% gf_rock$spp_code_num) %>% 
            group_by(block_id) %>% 
            summarize(total_lbs = sum(landings_lb, na.rm = T)/1000) %>% 
            left_join(., blocks) %>% 
            st_as_sf(), aes(fill = total_lbs)) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  theme_bw() + my_theme + 
  labs(title = "Rockfish (Sebastes spp.)",
       fill = "Thousand lbs.")

ggplot() +
  # Blocks
  geom_sf(data = blocks, lwd = 0.2) +
  # Landings 
  geom_sf(data = landings %>% 
            filter(species_id == 190) %>% 
            group_by(block_id) %>% 
            summarize(total_lbs = sum(landings_lb, na.rm = T)/1000) %>% 
            left_join(., blocks) %>% 
            st_as_sf(), aes(fill = total_lbs)) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  theme_bw() + my_theme + 
  labs(title = "Sablefish",
       fill = "Thousand lbs.")

ggplot() +
  # Blocks
  geom_sf(data = blocks, lwd = 0.2) +
  # Landings 
  geom_sf(data = landings %>% 
            filter(species_id == 152) %>% 
            group_by(block_id) %>% 
            summarize(total_lbs = sum(landings_lb, na.rm = T)/1000) %>% 
            left_join(., blocks) %>% 
            st_as_sf(), aes(fill = total_lbs)) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  theme_bw() + my_theme + 
  labs(title = "Spiny dogfish",
       fill = "Thousand lbs.")

ggplot() +
  # Blocks
  geom_sf(data = blocks, lwd = 0.2) +
  # Landings 
  geom_sf(data = landings %>% 
            filter(species_id %in% gf_dover_rex$spp_code_num) %>% 
            group_by(block_id) %>% 
            summarize(total_lbs = sum(landings_lb, na.rm = T)/1000) %>% 
            left_join(., blocks) %>% 
            st_as_sf(), aes(fill = total_lbs)) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  theme_bw() + my_theme + 
  labs(title = "Soles - Dover & Rex",
       fill = "Thousand lbs.")

ggplot() +
  # Blocks
  geom_sf(data = blocks, lwd = 0.2) +
  # Landings 
  geom_sf(data = landings %>% 
            filter(species_id %in% gf_thorny$spp_code_num) %>% 
            group_by(block_id) %>% 
            summarize(total_lbs = sum(landings_lb, na.rm = T)/1000) %>% 
            left_join(., blocks) %>% 
            st_as_sf(), aes(fill = total_lbs)) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  theme_bw() + my_theme + 
  labs(title = "Thornyheads (Sebastolobus spp.)",
       fill = "Thousand lbs.")


## Gamefish ----

ggplot() +
  # Blocks
  geom_sf(data = blocks, lwd = 0.2) +
  # Landings 
  geom_sf(data = landings %>% 
            filter(species_id %in% game_bass$spp_code_num) %>% 
            group_by(block_id) %>% 
            summarize(total_lbs = sum(landings_lb, na.rm = T)/1000) %>% 
            left_join(., blocks) %>% 
            st_as_sf(), aes(fill = total_lbs)) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  theme_bw() + my_theme + 
  labs(title = "Seabass (Serranidae, Epinephelinae)",
       fill = "Thousand lbs.")

ggplot() +
  # Blocks
  geom_sf(data = blocks, lwd = 0.2) +
  # Landings 
  geom_sf(data = landings %>% 
            filter(species_id == 3) %>% 
            group_by(block_id) %>% 
            summarize(total_lbs = sum(landings_lb, na.rm = T)/1000) %>% 
            left_join(., blocks) %>% 
            st_as_sf(), aes(fill = total_lbs)) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  theme_bw() + my_theme + 
  labs(title = "Bonito (Sarda chiliensis)",
       fill = "Thousand lbs.")

ggplot() +
  # Blocks
  geom_sf(data = blocks, lwd = 0.2) +
  # Landings 
  geom_sf(data = landings %>% 
            filter(species_id == 145) %>% 
            group_by(block_id) %>% 
            summarize(total_lbs = sum(landings_lb, na.rm = T)/1000) %>% 
            left_join(., blocks) %>% 
            st_as_sf(), aes(fill = total_lbs)) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  theme_bw() + my_theme + 
  labs(title = "California sheephead (Semicossyphus pulcher)",
       fill = "Thousand lbs.")

ggplot() +
  # Blocks
  geom_sf(data = blocks, lwd = 0.2) +
  # Landings 
  geom_sf(data = landings %>% 
            filter(species_id == 130) %>% 
            group_by(block_id) %>% 
            summarize(total_lbs = sum(landings_lb, na.rm = T)/1000) %>% 
            left_join(., blocks) %>% 
            st_as_sf(), aes(fill = total_lbs)) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  theme_bw() + my_theme + 
  labs(title = "California barracuda",
       fill = "Thousand lbs.")

## HMS ----

ggplot() +
  # Blocks
  geom_sf(data = blocks, lwd = 0.2) +
  # Landings 
  geom_sf(data = landings %>% 
            filter(species_id %in% hms_sword$spp_code_num) %>% 
            group_by(block_id) %>% 
            summarize(total_lbs = sum(landings_lb, na.rm = T)/1000) %>% 
            left_join(., blocks) %>% 
            st_as_sf(), aes(fill = total_lbs)) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  theme_bw() + my_theme + 
  labs(title = "Swordfish",
       fill = "Thousand lbs.")

ggplot() +
  # Blocks
  geom_sf(data = blocks, lwd = 0.2) +
  # Landings 
  geom_sf(data = landings %>% 
            filter(species_id %in% hms_tuna$spp_code_num) %>% 
            group_by(block_id) %>% 
            summarize(total_lbs = sum(landings_lb, na.rm = T)/1000) %>% 
            left_join(., blocks) %>% 
            st_as_sf(), aes(fill = total_lbs)) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  theme_bw() + my_theme + 
  labs(title = "Tunas",
       fill = "Thousand lbs.")


ggplot() +
  # Blocks
  geom_sf(data = blocks, lwd = 0.2) +
  # Landings 
  geom_sf(data = landings %>% 
            filter(species_id %in% hms_thre$spp_code_num) %>% 
            group_by(block_id) %>% 
            summarize(total_lbs = sum(landings_lb, na.rm = T)/1000) %>% 
            left_join(., blocks) %>% 
            st_as_sf(), aes(fill = total_lbs)) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  theme_bw() + my_theme + 
  labs(title = "Thresher sharks (Alopias spp.)",
       fill = "Thousand lbs.")

ggplot() +
  # Blocks
  geom_sf(data = blocks, lwd = 0.2) +
  # Landings 
  geom_sf(data = landings %>% 
            filter(species_id == 151) %>% 
            group_by(block_id) %>% 
            summarize(total_lbs = sum(landings_lb, na.rm = T)/1000) %>% 
            left_join(., blocks) %>% 
            st_as_sf(), aes(fill = total_lbs)) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  theme_bw() + my_theme + 
  labs(title = "Shortfin mako",
       fill = "Thousand lbs.")

ggplot() +
  # Blocks
  geom_sf(data = blocks, lwd = 0.2) +
  # Landings 
  geom_sf(data = landings %>% 
            filter(species_id == 167) %>% 
            group_by(block_id) %>% 
            summarize(total_lbs = sum(landings_lb, na.rm = T)/1000) %>% 
            left_join(., blocks) %>% 
            st_as_sf(), aes(fill = total_lbs)) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  theme_bw() + my_theme + 
  labs(title = "Blue shark",
       fill = "Thousand lbs.")

ggplot() +
  # Blocks
  geom_sf(data = blocks, lwd = 0.2) +
  # Landings 
  geom_sf(data = landings %>% 
            filter(species_id == 158) %>% 
            group_by(block_id) %>% 
            summarize(total_lbs = sum(landings_lb, na.rm = T)/1000) %>% 
            left_join(., blocks) %>% 
            st_as_sf(), aes(fill = total_lbs)) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  theme_bw() + my_theme + 
  labs(title = "Hammerhead (Sphyrna spp.)",
       fill = "Thousand lbs.")

## Market squid ----

ggplot() +
  # Blocks
  geom_sf(data = blocks, lwd = 0.2) +
  # Landings 
  geom_sf(data = landings %>% 
            filter(species_id == 711) %>% 
            group_by(block_id) %>% 
            summarize(total_lbs = sum(landings_lb, na.rm = T)/1000) %>% 
            left_join(., blocks) %>% 
            st_as_sf(), aes(fill = total_lbs)) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  theme_bw() + my_theme + 
  labs(title = "Market squid",
       fill = "Thousand lbs.")


## Urchin ----
ggplot() +
  # Blocks
  geom_sf(data = blocks, lwd = 0.2) +
  # Landings 
  geom_sf(data = landings %>% 
            filter(species_id %in% urchin$spp_code_num) %>% 
            group_by(block_id) %>% 
            summarize(total_lbs = sum(landings_lb, na.rm = T)/1000) %>% 
            left_join(., blocks) %>% 
            st_as_sf(), aes(fill = total_lbs)) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  theme_bw() + my_theme + 
  labs(title = "Urchins",
       fill = "Thousand lbs.")

## Cucumber ----
ggplot() +
  # Blocks
  geom_sf(data = blocks, lwd = 0.2) +
  # Landings 
  geom_sf(data = landings %>% 
            filter(species_id %in% cukes$spp_code_num) %>% 
            group_by(block_id) %>% 
            summarize(total_lbs = sum(landings_lb, na.rm = T)/1000) %>% 
            left_join(., blocks) %>% 
            st_as_sf(), aes(fill = total_lbs)) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  theme_bw() + my_theme + 
  labs(title = "Sea cucumbers",
       fill = "Thousand lbs.")

## Dungeness crab ----
ggplot() +
  # Blocks
  geom_sf(data = blocks, lwd = 0.2) +
  # Landings 
  geom_sf(data = landings %>% 
            filter(species_id == 800) %>% 
            group_by(block_id) %>% 
            summarize(total_lbs = sum(landings_lb, na.rm = T)/1000) %>% 
            left_join(., blocks) %>% 
            st_as_sf(), aes(fill = total_lbs)) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  theme_bw() + my_theme + 
  labs(title = "Dungeness crab",
       fill = "Thousand lbs.")

## Rock crab (Cancer spp.) ----
ggplot() +
  # Blocks
  geom_sf(data = blocks, lwd = 0.2) +
  # Landings 
  geom_sf(data = landings %>% 
            filter(species_id %in% oth_crab$spp_code_num) %>% 
            group_by(block_id) %>% 
            summarize(total_lbs = sum(landings_lb, na.rm = T)/1000) %>% 
            left_join(., blocks) %>% 
            st_as_sf(), aes(fill = total_lbs)) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  theme_bw() + my_theme + 
  labs(title = "Rock crab (Cancer spp.)",
       fill = "Thousand lbs.")

## Spiny lobster ----
ggplot() +
  # Blocks
  geom_sf(data = blocks, lwd = 0.2) +
  # Landings 
  geom_sf(data = landings %>% 
            filter(species_id == 820) %>% 
            group_by(block_id) %>% 
            summarize(total_lbs = sum(landings_lb, na.rm = T)/1000) %>% 
            left_join(., blocks) %>% 
            st_as_sf(), aes(fill = total_lbs)) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  theme_bw() + my_theme + 
  labs(title = "California spiny lobster",
       fill = "Thousand lbs.")

## Spot prawn ----
ggplot() +
  # Blocks
  geom_sf(data = blocks, lwd = 0.2) +
  # Landings 
  geom_sf(data = landings %>% 
            filter(species_id == 815) %>% 
            group_by(block_id) %>% 
            summarize(total_lbs = sum(landings_lb, na.rm = T)/1000) %>% 
            left_join(., blocks) %>% 
            st_as_sf(), aes(fill = total_lbs)) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  theme_bw() + my_theme + 
  labs(title = "Spot prawn (Pandalus platyceros)",
       fill = "Thousand lbs.")

## Pink shrimp ----
ggplot() +
  # Blocks
  geom_sf(data = blocks, lwd = 0.2) +
  # Landings 
  geom_sf(data = landings %>% 
            filter(species_id == 812) %>% 
            group_by(block_id) %>% 
            summarize(total_lbs = sum(landings_lb, na.rm = T)/1000) %>% 
            left_join(., blocks) %>% 
            st_as_sf(), aes(fill = total_lbs)) +
  # Region lines
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  theme_bw() + my_theme + 
  labs(title = "Pacific pink shrimp (Pandalus jordani)",
       fill = "Thousand lbs.")

# Examine 4-Digit Blocks -------------------------------------------------


four <- landings %>% filter(block_type == "Offshore")
three <- landings %>% filter(!(block_type == "Offshore"))

four_vessels <- data.frame(vessel_id = unique(four$vessel_id))
three_vessels <- data.frame(vessel_id = unique(three$vessel_id))

only_four_vessels <- landings %>% 
  filter(vessel_id %in% four_vessels$vessel_id 
         & !(vessel_id %in% three_vessels$vessel_id)) %>% 
  distinct(vessel_id)
# there are 331 vessels that always report 4-digit latitude areas

only_three_vessels <- landings %>% 
  filter(vessel_id %in% three_vessels$vessel_id 
         & !(vessel_id %in% four_vessels$vessel_id)) %>% 
  distinct(vessel_id)
# there are 3010 vessels that always report 3-digit spatial blocks

both_type_vessels <- landings %>% 
  filter(vessel_id %in% three_vessels$vessel_id 
         & vessel_id %in% four_vessels$vessel_id) %>% 
  distinct(vessel_id)
# there are 2597 that report in both 3- and 4-digit blocks

## Read created species group key ---------------
sp_group <- readRDS(file.path(datadir, "species_fishery_group_key.Rds"))

## Join species group key with landings ---------
landings <- left_join(landings, sp_group, by = c("species_id" = "spp_code_num"))

# Landings not included in current groups
landings_dropped <- landings %>% 
  filter(is.na(fishery))

length(landings_dropped$receipt_id)/length(landings$receipt_id) # 12%

vessels_all <- landings %>%
  group_by(fishery_group) %>% 
  summarize(n_total = length(unique(vessel_id)))

# Counting unique vessels for 3-digit locations
vessels_3 <- landings %>% 
  filter(nchar(as.character(block_id)) == 3) %>% 
  group_by(fishery_group) %>%
  summarise(vessels = list(unique(vessel_id)),
            n_3 = length(unique(vessel_id))) 

# Counting unique vessels for 4-digit locations
vessels_4 <- landings %>% 
  filter(nchar(as.character(block_id)) == 4) %>% 
  group_by(fishery_group) %>%
  summarise(vessels = list(unique(vessel_id)),
            n_4 = length(unique(vessel_id)))

# Identifying vessels operating in both 3-digit and 4-digit locations
vessels_in_both <- vessels_3 %>%
  inner_join(vessels_4, by = "fishery_group") %>%
  rowwise() %>%
  mutate(unique_vessels = list(intersect(vessels.x, vessels.y))) %>%
  unnest(cols = c(unique_vessels)) %>%
  group_by(fishery_group) %>%
  summarise(n_both = n_distinct(unique_vessels))

# Combining results
vessels <- vessels_3 %>% 
  select(fishery_group, n_3) %>% 
  left_join(vessels_4 %>% select(fishery_group, n_4)) %>% 
  left_join(vessels_in_both) %>% 
  left_join(vessels_all) %>% 
  mutate(n_3 = n_3 - n_both,
         n_4 = n_4 - n_both) # remove doublecount

# Display final results
final_results

# How many vessels contribute to the catch ? -----
vessel <- landings %>% 
  group_by(vessel_id) %>% 
  summarize(total_catch = sum(landings_lb)) %>% 
  arrange(desc(total_catch)) %>% 
  mutate(cum_catch = cumsum(total_catch),
         cum_percent = cum_catch/total_lbs*100) 

# Display landings by grouping time series


fishery_totals <- landings %>% 
  group_by(fishery_group) %>% 
  summarize(total_lb = sum(landings_lb, na.rm = T)) %>% 
  mutate(fishery_group = fct_reorder(fishery_group, -total_lb))

annual_landings <- landings %>% 
  group_by(fishery_group,year) %>% 
  summarize(total_lbs = sum(landings_lb, na.rm = T)) %>% 
  mutate(fishery_group = factor(fishery_group, levels(fishery_totals$fishery_group)))

species_totals <- landings %>% 
  group_by(species, fishery_group) %>% 
  summarize(total_lb = sum(landings_lb, na.rm = T),
            pct = round(total_lb/total_lbs*100,4)) %>% 
  arrange(-total_lb) %>% 
  mutate(cum_sum = cumsum(pct))

ggplot(annual_landings %>% drop_na()) +
  geom_path(aes(x = year, y = total_lbs, 
                color = fishery_group)) +
  scale_x_continuous(limits = c(2000,2020), expand = c(0,0))+
  scale_y_continuous(limits = c(0, 3e8), expand = c(0,0))+
  labs(x = "Year", y = "Total landings (lbs)", color = NULL) +
  theme_classic()

groundfish_totals <- landings %>% 
  filter(fishery_group == "groundfish") %>% 
  group_by(species, year) %>% 
  summarize(total_species_lbs = sum(landings_lb, na.rm = T))

ggplot(groundfish_totals) +
  geom_path(aes(x = year, y = total_species_lbs, color = species))

ggplot(landings %>% 
         filter())