

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
gisdir <- file.path(basedir, "gis_data/processed")
plotdir <- "analyses/2performance_fisheries/figures"

# Get blocks
blocks <- wcfish::blocks %>% 
  filter(block_state=="California" & block_type=="Inshore")

# Read data
block_stats <- read.csv(file.path(gisdir, "CA_blocks_with_mpas_all_mpa_types.csv"), as.is=T) %>% 
  left_join(blocks %>% select(block_id, block_sqkm) %>% sf::st_drop_geometry(), by="block_id") %>% 
  rename(mpa_sqkm=mpa_km2) %>% 
  mutate(mpa_perc=mpa_sqkm/block_sqkm)


# Build data
################################################################################

# Number of counterfactual blocks 
# (though some might not be eligible for other reasons)
blocks_all <- blocks %>% 
  select(block_id) %>% 
  left_join(block_stats) %>% 
  # Default block type
  mutate(block_type1=ifelse(!is.na(mpa_perc), "MPA", "Control"),
         block_type1=factor(block_type1, levels=c("MPA", "Control")))  %>% 
  mutate(block_type2=ifelse(!is.na(mpa_perc) & mpa_perc>=0.025, "MPA", "Control"),
         block_type2=factor(block_type2, levels=c("MPA", "Control")))  


# 
percs <- seq(0,1, 0.01)
blocks_n <- purrr::map_df(percs, function(x){
  
  # Build data
  n_counter <- blocks_all %>% 
    filter(is.na(mpa_perc) | mpa_perc <= x) %>% 
    nrow()
  
  # Out
  out <- tibble(perc=x,
                n_counter=n_counter)
  
})


# Plot data
################################################################################

# Theme
theme1 <- theme(axis.text=element_text(size=5),
                axis.title=element_text(size=6),
                legend.text=element_text(size=5),
                legend.title=element_text(size=6),
                plot.title=element_text(size=7), 
                plot.tag=element_text(size=7),
                # Gridlines
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"),
                # Legend
                legend.position = c(0.25, 0.15),
                legend.key.size = unit(0.2, "cm"),
                legend.background = element_rect(fill=alpha('blue', 0)))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Plot map 1
g1 <- ggplot() +
  # Plot blocks
  geom_sf(data=blocks_all, mapping=aes(fill=block_type1), lwd=0.1) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3) +
  # Labels
  labs(title="Control = Blocks with 0% MPA coverage", tag="A") +
  # Legend
  scale_fill_ordinal(name="Block type") +
  # Crop
  coord_sf(xlim = c(-125, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + theme1 +
  theme(legend.position = c(0.2, 0.2),
        axis.title = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g1

# Plot map 1
g2 <- ggplot() +
  # Plot blocks
  geom_sf(data=blocks_all, mapping=aes(fill=block_type2), lwd=0.1) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3) +
  # Labels
  labs(tag="B", title="Control = Blocks with <2.5% MPA coverage") +
  # Legend
  scale_fill_ordinal(name="Block type") +
  # Crop
  coord_sf(xlim = c(-125, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + theme1 + 
  theme(legend.position = c(0.2, 0.2),
        axis.title.y = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g2

# Plot percentage
g3 <- ggplot(block_stats, aes(x=mpa_perc)) + 
  geom_histogram(breaks=seq(0, 1, 0.025), fill="grey70") +
  # Reference line
  geom_vline(xintercept = 0.025, linetype="dotted", color="grey30") +
  # Labels
  labs(x="% of block\ncovered by MPAs", y="# of blocks", title="   ", tag="C") +
  # Axes
  scale_x_continuous(labels=scales::percent) +
  # Thene
  theme_bw() + theme1 +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
g3

# Threshold vs. counterfactuals
g4 <- ggplot(blocks_n, aes(x=perc, y=n_counter)) +
  geom_line(lwd=0.5) +
  # Reference line
  geom_vline(xintercept = 0.025, linetype="dotted", color="grey30") +
  # Labels
  labs(x="% of block covered by MPAs\nthat can be considered as a control block", 
       y="# of eligible control blocks", tag='D') +
  scale_x_continuous(labels=scales::percent) +
  # Theme
  theme_bw() + theme1 +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
g4

# Merge plots
layout_matrix <- matrix(c(1,2,3, 
                          1,2,4), ncol=3, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, g3, g4, layout_matrix=layout_matrix)
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_counterfactuals.png"), 
       width=6.5, height=3.5, units="in", dpi=600)



# Counterfactual validation
################################################################################

# MPA blocks
mu_mpa <- 0.2
sd_mpa <- 0.05

# Control blocks
mu_cntrl <- 0.03
sd_cntrl <- 0.02

# Simulate
x <- seq(0, 0.5, 0.001)
y_mpa <- dnorm(x, mean=mu_mpa, sd=sd_mpa)
y_cntrl <- dnorm(x, mean=mu_cntrl, sd=sd_cntrl)
df <- tibble(type=c(rep("MPA", length(x)), rep("Control", length(x))),
             x=c(x, x),
             y=c(y_mpa, y_cntrl)) %>% 
  mutate(type=factor(type, levels=c("MPA", "Control")))

# Plot
g <- ggplot(df, aes(x=x, y=y, group=type, fill=type)) +
  geom_area(alpha=0.8, color="grey30", position = "dodge") +
  # Labels
  labs(x="MPA density\n(% MPA within 1 km of block)", y="Density") +
  scale_x_continuous(labels=scales::percent) +
  # Legend
  scale_fill_ordinal(name="Block type") +
  # Theme
  theme_bw() +
  theme(axis.text=element_text(size=7),
        axis.title=element_text(size=8),
        legend.text=element_text(size=7),
        legend.title=element_text(size=8),
        legend.position = c(0.8, 0.8),
        # Gridlines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_counterfactuals_validity.png"), 
       width=3.5, height=3.5, units="in", dpi=600)
