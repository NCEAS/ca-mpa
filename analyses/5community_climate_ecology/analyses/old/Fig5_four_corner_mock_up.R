

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(patchwork)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data/monitoring/processed_data"
# basedir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/community_climate_derived_data/"
gisdir <- file.path(basedir, "gis_data/processed")
plotdir <- "analyses/5community_climate_ecology/figures"

# Read composition data
load(file.path(basedir, "comp_data.rda"))
comp_orig <- comp_data
rm(comp_data)

# Read four-corner data
load(file.path(basedir,"four_corner_output.rda"))
coef_orig <- coef_out
rm(coef_out)


# Format data
################################################################################

# Parameters
guilds <- c("Cold temperate", "Warm temperate", "Subtropical", "Tropical", "Cosmopolitan")
indicators <- c("SST", "MOCI", "CUTI", "BEUTI")

# Composition
##########################################

# Format data
comp <- comp_orig %>% 
  # Rename
  rename(guild=thermal_affinity, 
         habitat=group,
         biomass=group_total) %>% 
  # Format guild
  mutate(guild=stringr::str_to_sentence(guild),
         guild=factor(guild, levels=guilds)) %>% 
  # Format habitat
  mutate(habitat=recode_factor(habitat,
                               "Rocky intertidal"="Rocky intertidal",        
                               "kelp forest fishes"="Kelp forest fishes",
                               "Kelp forest inverts and algae"="Kelp forest invertebrates/algae",
                               "Rocky reef fishes"="Rocky reef",
                               "Deep reef fishes"="Deep reef")) %>% 
  # Calculate percentage
  group_by(habitat, year) %>% 
  mutate(prop=biomass/sum(biomass)) %>% 
  ungroup()

# Inspect
table(comp$guild)
table(comp$habitat)


# Four corner
##########################################

# Format data
coef <- coef_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(guild=thermal_affinity, 
         habitat=group, 
         indicator=environmental_variables) %>% 
  # Format indicator
  mutate(indicator=factor(indicator, levels=indicators)) %>% 
  # Format guild
  mutate(guild=factor(guild, levels=guilds)) %>% 
  # Format habitat
  mutate(habitat=recode_factor(habitat,
                               "Rocky intertidal"="Rocky intertidal",        
                               "kelp forest fish"="Kelp forest fishes",
                               "Kelp forest inverts and algae"="Kelp forest inv/alg",
                               "Rocky reef fishes"="Rocky reef",
                               "Deep reef fish"="Deep reef")) %>% 
  # Cap beta
  mutate(beta_cap=pmax(-1.5, beta))

# Inspect
table(coef$indicator)
table(coef$guild)
table(coef$habitat)

# Plot data
################################################################################

# Theme
base_theme <-  theme(axis.text=element_text(size=7),
                     axis.title=element_text(size=8),
                     legend.text=element_text(size=7),
                     legend.title=element_text(size=8),
                     strip.text=element_text(size=8),
                     plot.tag=element_text(size=9),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.key = element_rect(fill=alpha('blue', 0)),
                     legend.background = element_rect(fill=alpha('blue', 0)))

# Plot composition
g1 <- ggplot(comp, aes(x=year, y=prop, fill=guild)) +
  facet_wrap(~habitat, ncol=1) +
  geom_bar(stat="identity", position = position_fill(reverse = TRUE)) +
  # Refence lines
  geom_vline(xintercept = c(2013.5, 2016.5), linetype="dashed") +
  # Labels
  labs(x="Year\n", y="Percent of community", tag="A") +
  scale_x_continuous(breaks=2007:2020) +
  scale_y_continuous(labels=scales::percent) +
  # Legend
  scale_fill_manual(name="",
                    values=c(RColorBrewer::brewer.pal(4, "Spectral") %>% rev(), "plum3")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.3, "cm"),
        legend.margin = margin(-5,0,5,0), # 3 is to align x-axis of panels
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g1

# Plot four corner
g2 <- ggplot(coef, aes(x=indicator, y=guild, fill=beta_cap)) +
  facet_wrap(~habitat, ncol=1) +
  geom_raster() +
  # Labels
  labs(x="Indicator", y="Thermal affinity", tag="B") +
  # Legend
  scale_fill_gradient2(name="Beta",
                       midpoint=0,
                       low="darkred", high="navy", mid="white") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.3, "cm"),
        legend.margin = margin(-5,0,0,0),
        # axis.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, ncol=2, widths=c(0.66,  0.34))
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig5_species_composition_mockup.png"),
      width=6.5, height=6.5, units="in", dpi=600)










