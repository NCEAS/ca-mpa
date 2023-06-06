

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(patchwork)

# Chris Directories
basedir <- "/Users/cfree/Library/CloudStorage/GoogleDrive-cfree@ucsb.edu/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data" #Chris
datadir <- file.path(basedir, "monitoring/processed_data/community_climate_derived_data/statewide_data")
plotdir <- "analyses/5community_climate_ecology/figures"

# Josh Directories
# basedir <- "/home/shares/ca-mpa/data/sync-data"
# datadir <- file.path(basedir, "monitoring/processed_data/community_climate_derived_data/statewide_data")
# plotdir <- "analyses/5community_climate_ecology/figures"

# Read data
data_orig2 <- read.csv(file.path(datadir, "mpa_betadisp_mod_run2.csv"), as.is=T)

# Read MPA metadata
mpas_data <- readRDS(file.path(basedir, "mpa_traits/processed/CA_mpa_metadata.Rds"))


# Format data
################################################################################

# Format data
data <- data_orig2 %>% 
  # Rename
  janitor::clean_names("snake") %>%
  rename(site_type=mpa_type) %>% 
  # Format MPA name
  mutate(mpa=stringr::str_to_title(mpa) %>% gsub("Smr", "SMR", .) %>% gsub("Smca", "SMCA", .)) %>% 
  mutate(mpa=recode(mpa, 
                    "Ano Nuevo SMR"="Año Nuevo SMR",
                    "Blue Cavern Onshore SMCA"="Blue Cavern Onshore SMCA (No-Take)",
                    "Campus Point SMCA"="Campus Point SMCA (No-Take)",
                    "Point Vicente SMCA"="Point Vicente SMCA (No-Take)")) %>% 
  # Add region
  left_join(mpas_data %>% select(mpa, region, lat_dd), by="mpa") %>% 
  # Create type
  mutate(period=paste(period_1, period_2, sep="-"),
         process=recode_factor(period,
                               "before-during"="Resistance",
                               "before-after"="Recovery")) %>% 
  # Arrange
  select(habitat, region, lat_dd, mpa, site_type, process, period, distance) %>% 
  # Spread
  spread(key="site_type", value="distance") %>% 
  rename(dist_ref=ref, dist_mpa=smr) %>% 
  # Calculate proportion prevented
  #mutate(prop=(dist_ref-dist_mpa)/dist_ref) %>% 
  #calculate percent shift relative to reference site
  mutate(prop=(dist_mpa-dist_ref)/dist_mpa) %>% 
  # Remove sites that aren't MPAs
  filter(!is.na(region) & !is.na(dist_mpa)) 

# Check MPA names
mpa_names <- sort(unique(data$mpa))
mpa_names[!mpa_names %in% mpas_data$mpa]

# Clean data
data2 <- data %>%
  #Drop Natural Bridges since it is not a typical KF MPA
  mutate(region = recode(region,
                         "North Central Coast" = "North",
                         "Central Coast" = "Central",
                         "South Coast" = "South"))%>%
  filter(!(habitat == "Kelp forest inverts and algae" &
             mpa == "Natural Bridges SMR")) %>%
  filter(!(habitat == "Kelp forest fishes" &
             mpa == "Natural Bridges SMR"))%>%
  mutate(mpa = factor(mpa)) %>%
  pivot_longer(cols=c("dist_ref","dist_mpa"),names_to="MPA_type", values_to = "distance") %>%
  rename("dist_perc" = "prop") %>%
  #get MPAs only
  filter(MPA_type == "dist_mpa") %>%
  #arrange habitats
  mutate(habitat=recode_factor(habitat,
                               "Rocky intertidal"="Rocky\nintertidal",
                               "Kelp forest inverts and algae"="Kelp forest\ninverts/algae",
                               "Kelp forest fishes"="Kelp forest\nfishes"))



# Plot data
################################################################################

# Base theme
base_theme <- theme(axis.text=element_text(size=7),
                    axis.title=element_blank(),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=7),
                    strip.text=element_text(size=7),
                    plot.tag=element_text(size=8),
                    plot.title=element_blank(),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.key.size = unit(0.3, "cm"),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Schematic theme
schem_theme <- theme_minimal() +
               theme(legend.position="none", 
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     plot.tag=element_text(size=8),
                     plot.title = element_text(size=7),
                     axis.text=element_text(size=6),
                     axis.title = element_blank(),
                     axis.text.x = element_blank(),
                     axis.text.y=element_text(color=c("#377EB8", "#E41A1C")))

# Colors
RColorBrewer::brewer.pal(3, "Set1")

# Plot schematic 1
toy1 <- tibble(site=factor(c("Inside", "Outside"), levels=c("Outside", "Inside")),
               distance=c(0.5, 0.7))
schem1 <- ggplot(toy1, aes(y=site, yend=site, xend=distance, color=site)) +
  geom_segment(x=0, arrow = arrow(length=unit(0.30, "cm"))) +
  # Labels
  #labs(title = "MPA prevents shifts")+
  labs(title="Shift distance less in MPA", tag="A") +
  scale_color_manual(values=c("#377EB8", "#E41A1C")) +
  # Limits
  lims(x=c(0, 0.8)) +
  # Theme
  schem_theme
schem1

# Plot schematic 2
toy2 <- tibble(site=factor(c("Inside", "Outside"), levels=c("Outside", "Inside")),
               distance=c(0.7, 0.5))
schem2 <- ggplot(toy2, aes(y=site, yend=site, xend=distance, color=site)) +
  geom_segment(x=0, arrow = arrow(length=unit(0.30, "cm"))) +
  # Labels
  #labs(title="MPA exacerbates shifts") +
  labs(title="Shift distance greater in MPA", tag="B") +
  scale_color_manual(values=c( "#377EB8", "#E41A1C")) +
  # Limits
  lims(x=c(0, 0.8)) +
  # Theme
  schem_theme
schem2

# Plot data
g1 <- ggplot(data2, aes(x=habitat, y=mpa, size=distance, fill=dist_perc, color="")) +
  facet_grid(region~process, space="free", scale="free") +
  geom_point(pch=21, color="black") +
  # Labels
  labs(x="", y="", tag="C") +
  # Legend
  scale_size_continuous(name="Shift distance\n(smaller = more resilient)") +
  scale_fill_gradient2(name="Percent of shift\nexacerbated (red)\nor reduced (blue)",
                       midpoint=0, high="darkred", low="navy", mid="white", # high="#E41A1C", low="#377EB8"
                       labels=scales::percent) +
  # guides(size = guide_legend(order = 1),
  #        fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", order=2)) +
  # scale_fill_gradient2(name="Prop. of shift\nexacerbated (red)\nor reduced (blue)",
  #                      midpoint=0, high="#E41A1C", low="#377EB8", mid="white") +
  #trick ggplot into placing NAs in legend
  scale_color_manual(values=NA) +
  #reduce space between x items
  #scale_x_discrete(expand = c(-1, -2)) +
  #ggh4x::force_panelsizes(cols = c(0.001, 0.001)) +
  #set legend order
  guides(size = guide_legend(order = 1),
         fill = guide_colorbar(order = 2, ticks.colour = "black", frame.colour = "black"),
         color = guide_legend(order = 3, "No paired \nreference site", override.aes=list(fill="gray60")))+
  # Theme
  theme_bw() + base_theme +
  theme(axis.title = element_blank(),
        plot.tag.position = c(-0.051,0.99), # this is to move the C tag under the A tag
        plot.margin = unit(c(0,2,1,2), "lines"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
g1

  
# Merge
layout_matrix <- matrix(c(1,2,
                          3,3), ncol=2, byrow=T)
g1_full <- gridExtra::grid.arrange(schem1, schem2, g1, 
                                   layout_matrix=layout_matrix,
                                   heights=c(0.1, 0.8))
g1_full


# Export
cowplot::save_plot(g1_full, filename=file.path(plotdir, "Fig5_resistance_recovery_statewide_new.png"), 
     base_width=6.5, base_height=6.5, units="in", dpi=600, bg="white", base_asp=0.8)



