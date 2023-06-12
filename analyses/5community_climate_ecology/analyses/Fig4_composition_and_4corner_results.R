

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(patchwork)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data/" #Chris
# basedir <- "/home/shares/ca-mpa/data/sync-data/" #Josh
gisdir <- file.path(basedir, "gis_data/processed")
plotdir <- "analyses/5community_climate_ecology/figures"

# Read composition data
load(file.path(basedir, "monitoring/processed_data/comp_data.rda"))
comp_orig <- comp_data
rm(comp_data)

# Read four-corner data
load(file.path(basedir,"monitoring/processed_data/community_climate_derived_data/four_corner_output_anom.rda"))
coef_anom <- coef_out
rm(coef_out)

load(file.path(basedir,"monitoring/processed_data/community_climate_derived_data/four_corner_output_abs.rda"))
coef_abs <- coef_out
rm(coef_out)


# Format data
################################################################################

# Parameters
guilds <- c("Cold temp.", "Warm temp.", "Subtropical", "Tropical", "Cosmopolitan")
indicators <- c("SST", "SBT", "MOCI", "BEUTI")

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
         guild=gsub("temperate", "temp.", guild),
         guild=factor(guild, levels=guilds)) %>% 
  # Format habitat
  mutate(habitat=recode_factor(habitat,
                               "Rocky intertidal"="Rocky intertidal",        
                               "kelp forest fishes"="Kelp forest fish",
                               "Kelp forest inverts and algae (swath)"="Kelp forest invertebrates/algae (swath)",
                               "Kelp forest inverts and algae (upc)"="Kelp forest invertebrates/algae (UPC)",
                               "Rocky reef fishes"="Shallow reef",
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
coef <- coef_anom %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(guild=thermal_affinity, 
         habitat=group, 
         indicator=environmental_variables) %>% 
  # Format indicator
  mutate(indicator=recode(indicator, "BT"="SBT"), 
         indicator=factor(indicator, levels=indicators)) %>% 
  # Format guild
  mutate(guild=recode_factor(guild,
                             "Cold temperate"="Cold temp.",
                             "Warm temperate"="Warm temp.",
                             "Subtropical"="Subtropical",
                             "thermal_affinitytropical"="Tropical",
                             "Cosmopolitan"="Cosmo.")) %>% 
  # Format habitat
  mutate(habitat=recode_factor(habitat,
                               "Rocky intertidal"="Rocky intertidal",        
                               "kelp forest fish"="Kelp forest fish",
                               "Kelp forest inverts and algae (swath)"="Kelp inv/alg (s)",
                               "Kelp forest inverts and algae (upc)"="Kelp inv/alg (U)",
                               "Rocky reef fish"="Shallow reef",
                               "Deep reef fish"="Deep reef")) %>% 
  # Standardize beta
  group_by(habitat) %>% 
  mutate(beta_sd=scale(beta, center=F, scale=T)) %>% 
  ungroup()
  
# Inspect
table(coef$indicator)
table(coef$guild)
table(coef$habitat)

# Plot data
################################################################################

# Theme
base_theme <-  theme(axis.text=element_text(size=8),
                     axis.title=element_text(size=9),
                     legend.text=element_text(size=8),
                     legend.title=element_text(size=9),
                     strip.text=element_text(size=9),
                     plot.tag=element_text(size=10),
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
  geom_bar(stat="identity", position = position_fill(reverse = TRUE), color="grey30", linewidth=0.2) +
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
        legend.key.size = unit(0.2, "cm"),
        legend.margin = margin(-5,0,5,0), # 3 is to align x-axis of panels
        axis.text.x = element_text(angle = 45, hjust=1))
g1

# Plot all four corner results
g_4corner <- ggplot(coef, aes(x=indicator, y=guild, fill=beta_sd)) +
  facet_wrap(~habitat, ncol=1) +
  geom_tile() +
  # Plot 0s (no interaction term)
  geom_point(data=coef %>% filter(beta==0), shape="x") +
  # Labels
  labs(x="Indicator", y="Thermal affinity", tag="B") +
  # Legend
  scale_fill_gradient2(name="Effect",
                       midpoint=0,
                       breaks=c(-1.5, 0, 2), 
                       labels=c("-", "0", "+"),
                       low="navy", high="darkred", mid="white") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.2, "cm"),
        legend.margin = margin(-5,0,0,0),
        # axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1))
g_4corner 

# Merge
g_all1 <- gridExtra::grid.arrange(g1, g_4corner,
                                  widths=c(0.68,  0.32))
g_all1

# Export
ggsave(g_all1, filename=file.path(plotdir, "Fig4_composition_and_4corner_results4.png"),
       width=6.5, height=7.5, units="in", dpi=600)


# Plot data - indiv 4corner plot approach for absolute temp as supp fig
################################################################################

# Format data
coef_abs <- coef_abs %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(guild=thermal_affinity, 
         habitat=group, 
         indicator=environmental_variables) %>% 
  # Format indicator
  mutate(indicator=recode(indicator, "BT"="SBT"), 
         indicator=factor(indicator, levels=indicators)) %>% 
  # Format guild
  mutate(guild=recode_factor(guild,
                             "Cold temperate"="Cold temp.",
                             "Warm temperate"="Warm temp.",
                             "Subtropical"="Subtropical",
                             "thermal_affinitytropical"="Tropical",
                             "Cosmopolitan"="Cosmo.")) %>% 
  # Format habitat
  mutate(habitat=recode_factor(habitat,
                               "Rocky intertidal"="Rocky intertidal",        
                               "kelp forest fish"="Kelp forest fish",
                               "Kelp forest inverts and algae (swath)"="Kelp inv/alg (s)",
                               "Kelp forest inverts and algae (upc)"="Kelp inv/alg (U)",
                               "Rocky reef fish"="Shallow reef",
                               "Deep reef fish"="Deep reef")) %>% 
  # Standardize beta
  group_by(habitat) %>% 
  mutate(beta_sd=scale(beta, center=F, scale=T)) %>% 
  ungroup()


# Plot four corner - rocky
g1 <- ggplot(coef_abs %>% filter(habitat=="Rocky intertidal"),
             aes(x=indicator, y=guild, fill=beta)) +
  facet_wrap(~habitat, ncol=1) +
  geom_raster() +
  # Labels
  labs(x="", y="", tag=" ") +
  # Legend
  scale_fill_gradient2(name="Beta",
                       midpoint=0,
                       low="darkred", high="navy", mid="white") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "right",
        legend.key.size = unit(0.3, "cm"),
        legend.margin = margin(0,0,0,0),
        # axis.title = element_blank(),
        #axis.text.x=element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1)
  )
g1

# Plot four corner - kelp fish
g2 <- ggplot(coef_abs %>% filter(habitat=="Kelp forest fish"),
             aes(x=indicator, y=guild, fill=beta)) +
  facet_wrap(~habitat, ncol=1) +
  geom_raster() +
  # Labels
  labs(x="", y="", tag=" ") +
  # Legend
  scale_fill_gradient2(name="Beta",
                       midpoint=0,
                       low="darkred", high="navy", mid="white") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "right",
        legend.key.size = unit(0.3, "cm"),
        legend.margin = margin(0,0,0,0),
        # axis.title = element_blank(),
        #axis.text.x=element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1)
  )
g2

# Plot four corner - kelp inv/alg
g3 <- ggplot(coef_abs %>% filter(habitat=="Kelp inv/alg (s)"),
             aes(x=indicator, y=guild, fill=beta)) +
  facet_wrap(~habitat, ncol=1) +
  geom_raster() +
  # Labels
  labs(x="", y="", tag=" ") +
  # Legend
  scale_fill_gradient2(name="Beta",
                       midpoint=0,
                       low="darkred", high="navy", mid="white") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "right",
        legend.key.size = unit(0.3, "cm"),
        legend.margin = margin(0,0,0,0),
        # axis.title = element_blank(),
        #axis.text.x=element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1)
  )
g3

# Plot four corner - kelp inv/alg (U)
g4 <- ggplot(coef_abs %>% filter(habitat=="Kelp inv/alg (U)"),
             aes(x=indicator, y=guild, fill=beta)) +
  facet_wrap(~habitat, ncol=1) +
  geom_raster() +
  # Labels
  labs(x="", y="", tag=" ") +
  # Legend
  scale_fill_gradient2(name="Beta",
                       midpoint=0,
                       low="darkred", high="navy", mid="white") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "right",
        legend.key.size = unit(0.3, "cm"),
        legend.margin = margin(0,0,0,0),
        # axis.title = element_blank(),
        #axis.text.x=element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1)
  )
g4

# Plot four corner - rocky reef
g5 <- ggplot(coef_abs %>% filter(habitat=="Shallow reef"),
             aes(x=indicator, y=guild, fill=beta)) +
  facet_wrap(~habitat, ncol=1) +
  geom_raster() +
  # Labels
  labs(x="", y="", tag=" ") +
  # Legend
  scale_fill_gradient2(name="Beta",
                       midpoint=0,
                       low="darkred", high="navy", mid="white") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "right",
        legend.key.size = unit(0.3, "cm"),
        legend.margin = margin(0,0,0,0),
        # axis.title = element_blank(),
        #axis.text.x=element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1)
  )
g5

# Plot four corner - deep reef
g6 <- ggplot(coef_abs %>% filter(habitat=="Deep reef"),
             aes(x=indicator, y=guild, fill=beta)) +
  facet_wrap(~habitat, ncol=1) +
  geom_raster() +
  # Labels
  labs(x="", y="", tag=" ") +
  # Legend
  scale_fill_gradient2(name="Beta",
                       midpoint=0,
                       low="darkred", high="navy", mid="white") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "right",
        legend.key.size = unit(0.3, "cm"),
        legend.margin = margin(0,0,0,0),
        # axis.title = element_blank(),
        #axis.text.x=element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1)
  )
g6

#merge

g <- gridExtra::grid.arrange(g1, g3, g4, g2, g5, g6, nrow = 3, ncol = 2)

g_final <- annotate_figure(g, left = textGrob("Thermal affinity", rot = 90, vjust = 1),
                bottom = textGrob("Indicator", hjust=0.25, vjust=-1))


ggsave(g_final, filename=file.path(plotdir, "FigSX_4corner_absolute.png"),
       width=5.5, height=6.5, units="in", dpi=600, bg="white")






