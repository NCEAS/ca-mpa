#Joshua G. Smith
#January 5. 2023

rm(list=ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/home/shares/ca-mpa/data/sync-data" # Josh
#basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data/"
datadir1 <- file.path(basedir, "monitoring/processed_data/community_climate_derived_data")
datadir2 <- file.path(basedir, "mpa_traits/processed")
plotdir <- "analyses/5community_climate_ecology/figures"

# Read data
kelp_data <- read.csv(file.path(datadir1, "kelp_swath_mpa_year_statewide.csv"))
intertidal <- read.csv(file.path(datadir1, "rocky_statewide.csv"))
CCFRP <- read.csv(file.path(datadir1, "CCFRP_statewide.csv"))
deep_reef <- read.csv(file.path(datadir1, "deep_reef_statewide.csv"))
mpa_attributes_gen <- readRDS(file.path(datadir2, "mpa_attributes_general.Rds")) %>%
                      dplyr::select(affiliated_mpa, latitude_centroid, longitude_centroid, mpa_class)

# Read MPA traits
mpas_data <- readRDS(file.path(basedir, "mpa_traits/processed/CA_mpa_metadata.Rds"))


# Process and select variables of interest
################################################################################

kelp_data1 <- kelp_data %>%
              filter(year >= 2007,
                     mpa_defacto_class == "smr")%>%
              distinct(year, region3, region4, affiliated_mpa, mpa_designation = mpa_defacto_designation)%>%
              mutate(dummy_var = 1,
                     habitat = "Kelp forest")

intertidal1 <- intertidal %>%
  filter(year >= 2007)%>%
  distinct(year, region3, region4, affiliated_mpa, mpa_designation)%>%
  mutate(dummy_var = 1,
         habitat = "Rocky intertidal")%>%
  #drop unpaired reference sites
  filter(!(affiliated_mpa == "none" |
             affiliated_mpa == "anacapa island special closure"))

CCFRP1 <- CCFRP %>%
  filter(year >= 2007,
         mpa_class == "smr")%>%
  distinct(year, region3, region4, affiliated_mpa, mpa_designation)%>%
  mutate(dummy_var = 1,
         habitat = "Shallow reef")%>%
  filter(!(affiliated_mpa == "trinidad smr"))

deep_reef1 <- deep_reef %>%
  filter(year >= 2007,
         mpa_defacto_class == "smr")%>%
  distinct(year, region3, region4, affiliated_mpa, mpa_designation= mpa_defacto_designation)%>%
  mutate(dummy_var = 1,
         habitat = "Deep reef")


# Build data
data <- bind_rows(kelp_data1, intertidal1, CCFRP1, deep_reef1) %>% 
  # Rename
  rename(mpa=affiliated_mpa) %>% 
  # Format MPA name
  mutate(mpa=stringr::str_to_title(mpa),
         mpa=gsub("Smr", "SMR", mpa),
         mpa=gsub("Smca", "SMCA", mpa),
         mpa=recode(mpa,
                    "Point Vicente SMCA"="Point Vicente SMCA (No-Take)",      
                    "Blue Cavern Onshore SMCA"="Blue Cavern Onshore SMCA (No-Take)",   
                    "Campus Point SMCA"="Campus Point SMCA (No-Take)",   
                    "Swamis SMCA"="Swami's SMCA",          
                    "Laguna Beach SMCA"="Laguna Beach SMCA (No-Take)",       
                    "Ano Nuevo SMR"="Año Nuevo SMR")) %>% 
  # Format habitat
  mutate(habitat=recode_factor(habitat,
                              "Rocky intertidal"="Rocky intertidal\ninverts/algae",
                              "Kelp forest"="Kelp forest\nfishes/inverts/algae",
                              "Rocky reef fishes"="Shallow reef",
                              "Deep reef fishes"="Deep reef")) %>% 
  # Add region
  left_join(mpas_data %>% select(mpa, region)) %>% 
  mutate(region=recode_factor(region,
                              "North Coast"="North\nCoast",
                              "North Central Coast"="North Central\nCoast",
                              "Central Coast"="Central\nCoast",
                              "South Coast"="South\nCoast"))

#
unique(data$mpa)[!unique(data$mpa) %in% mpas_data$mpa]

# Inspect
str(data)



# Plot data
################################################################################

# Theme
base_theme <- theme(axis.text=element_text(size=6, color = "black"),
                    axis.title=element_blank(),
                    strip.text=element_text(size=7, color = "black", face = "bold"),
                    strip.background = element_blank(),
                    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,color = "black",),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.background = element_rect(fill=alpha('blue', 0)))


# Plot data
g <- ggplot(data, aes(x=year, y=mpa)) +
  facet_grid(region ~ habitat, scales="free_y", space="free_y") +
  geom_tile(fill="SteelBlue") +
  # Reference lines
  geom_vline(xintercept = c(2013.5, 2016.5), linetype="dashed") +
  # Labels
  labs(x="", y="") +
  scale_x_continuous(breaks=seq(2005, 2020, 5)) +
  # Theme
  theme_bw() + base_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigS1_data_availability_new.png"), 
       width=6.5, height=6, units="in", dpi=600)










