
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Users/cfree/Library/CloudStorage/GoogleDrive-cfree@ucsb.edu/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data/"
datadir <- file.path(basedir, "habitat_merge")
plotdir <- 

# Read data
data_orig <- raster::raster(file.path(datadir, "CA_bottom_substrate_10m.tif"))

# Projections
wgs84 <- sf::st_crs("+proj=longlat +datum=WGS84")

# Get MPAs
mpas <- wcfish::mpas_ca %>% 
  sf::st_transform(crs=raster::crs(data_orig))
mpas_df <- mpas %>% 
  sf::st_drop_geometry() %>% 
  mutate(region=recode(region,
                       "CCSR"="Central",
                       "NCCSR"="North Central",
                       "NCSR"="North", 
                       "SCSR"="South",
                       "SFBSR"="San Francisco Bay"))


# Build data
################################################################################

# Extracts cells inside MPAs
mpa_cells <- exactextractr::exact_extract(x=data_orig,
                                          y=mpas,
                                          coverage_area=T)

# Calcuate MPA habitat composition
x <- 1
mpa_stats <- purrr::map_df(1:length(mpa_cells), function(x){
  
  # MPA
  mpa_do <- mpas$name[x]
  
  # Process data
  sdata <- mpa_cells[[x]] %>% 
    # Habitat stats
    group_by(value) %>% 
    summarise(ncells=n(),
              area_m2=sum(coverage_area)) %>% 
    ungroup() %>% 
    # Habitat area / prop
    mutate(area_km2=area_m2/(1000^2),
           prop=area_km2/sum(area_km2)) %>% 
    # Add MPA and habitat
    mutate(mpa=mpa_do,
           habitat=case_when(value==1 ~ "Hard",
                             value==0 ~ "Soft",
                             T ~ "Unmapped")) %>% 
    # Arrange
    select(-value) %>% 
    select(mpa, habitat, everything())
  
  
})

# Wide data
data_wide <- mpa_stats %>% 
  # Simplify
  select(mpa, habitat, prop) %>% 
  # Spread
  mutate(habitat=tolower(habitat)) %>% 
  spread(key="habitat", value="prop") %>% 
  # Add zeros
  mutate_all(~replace(., is.na(.), 0)) %>% 
  # Add MPA info
  left_join(mpas_df %>% select(name, type, region), by=c("mpa"="name")) %>% 
  # Arrange
  select(region, mpa, type, everything())

# Long data
data_long <- mpa_stats %>% 
  # Add MPA info
  left_join(mpas_df %>% select(name, type, region), by=c("mpa"="name")) %>% 
  # Arrange
  select(region, mpa, type, everything())


# Export
saveRDS(data_long, file=file.path(datadir, "bottom_substrate_by_mpa_long.Rds"))
saveRDS(data_wide, file=file.path(datadir, "bottom_substrate_by_mpa_wide.Rds"))




# Plot data
################################################################################

# Stats
stats <- data_wide %>% 
  # Remove
  filter(region!="San Francisco Bay") %>% 
  filter(type != "Special Closure") %>%  
  # Recode region
  mutate(region=factor(region, levels=c("North", "North Central", "Central", "South"))) %>% 
  # Arrange
  arrange(region, desc(hard), desc(soft))

# Prep data for plotting
data_plot <- data_long %>% 
  # Remove SF bay
  filter(region!="San Francisco Bay") %>% 
  filter(type != "Special Closure") %>% 
  # Recode region
  mutate(region=factor(region, levels=c("North", "North Central", "Central", "South"))) %>% 
  # Recode habitat
  mutate(habitat=factor(habitat, levels=c("Unmapped", "Soft", "Hard"))) %>% 
  # Order MPA
  mutate(mpa=factor(mpa, levels=stats$mpa))

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=7),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot(data_plot %>% filter(region!="South"), aes(y=mpa, x=prop, fill=habitat)) +
  facet_grid(region~., scales="free_y", space="free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Substrate coverage (%)", y="") +
  scale_x_continuous(labels=scales::percent) +
  # Legend
  scale_fill_manual(name="Substrate", values=c("grey70", "yellow", "brown")) +
  # Theme
  theme_bw() + my_theme + 
  theme(legend.position = "none")
g1

# Plot data
g2 <- ggplot(data_plot %>% filter(region=="South"), aes(y=mpa, x=prop, fill=habitat)) +
  facet_grid(region~., scales="free_y", space="free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Substrate coverage (%)", y="") +
  scale_x_continuous(labels=scales::percent) +
  # Legend
  scale_fill_manual(name="", values=c("grey70", "yellow", "brown"),
                    guide = guide_legend(reverse = TRUE)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "top",
        legend.key.size = unit(0.5, "cm"))
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1)
g

# Export
ggsave(g, filename=file.path(datadir, "substrate_by_mpa.png"), 
       width=6.5, height=6.5, units="in", dpi=600)
