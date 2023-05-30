
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ggplot2)
library(tidyverse)

# Directories
plotdir <- "analyses/1performance_eco/figures"

# Read data
data_orig <- readxl::read_excel("~/Desktop/fake_data.xlsx") %>% 
  # Add error bars
  mutate(lrr_lo=lrr-0.2,
         lrr_hi=lrr+0.2) %>% 
  # Order status
  mutate(status=factor(status, levels=c("Targeted", "Non-targeted")))

# Format data
################################################################################

# Format data
data1 <- data_orig %>% 
  # Order regions
  mutate(region=recode(region, 
                       "Pooled"="", 
                       "N. Channel Islands"="N. Channel\nIslands"),
         region=factor(region,
                       levels=c("North", "Central", "N. Channel\nIslands", "South", ""))) %>% 
  # Order habitats
  mutate(habitat=factor(habitat, 
                        levels=c("Surf", "Kelp", "Rocky reef", "Deep reef", "Pooled") %>% rev())) %>% 
  # Remove one
  filter(!is.na(status))


# Plot data - original
################################################################################

# Theme
theme1 <- theme(axis.text=element_text(size=7),
                axis.title=element_text(size=8),
                legend.text=element_text(size=7),
                legend.title=element_text(size=8),
                strip.text=element_text(size=8),
                plot.title=element_text(size=10),
                # Gridlines
                panel.grid.major.x = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black"),
                # Legend
                legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data1, aes(x=lrr, y=habitat, shape=status, color=habitat)) +
  facet_grid(region~., space="free_y", scales="free_y") +
  # Points
  geom_point(position=position_dodge(width=1)) +
  # Error bars
  geom_errorbar(mapping=aes(xmin=lrr_lo, xmax=lrr_hi, color=habitat, linetype=status),
                position=position_dodge(width=1), width=0) +
  # Reference line
  geom_vline(xintercept=0, linetype="dashed") +
  # Labels
  labs(x="Log-response ratio", y="") +
  # Legend
  scale_linetype_manual(name="", values=c("solid", "dotted")) +
  scale_shape_manual(name="", values=c(16, 1)) +
  scale_color_manual(name="Habitat", guide="none",
                     values=c("orange", "darkgreen", "navy", "purple", "black") %>% rev()) +
  # Theme
  theme_bw() + theme1 +
  theme(legend.position = "top",
        legend.margin = margin(rep(-5, 4)))
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig2_biomass_lrr_region_centric.png"), 
       width=6.5, height=5.5, units="in", dpi=600)


# Plot data - habitat centric
################################################################################

# Reverese order
data2 <- data_orig %>% 
  # Order regions
  mutate(region=factor(region,
                       levels=c("North", "Central", "N. Channel Islands", "South", "Pooled") %>% rev())) %>% 
  # Order habitats
  mutate(habitat=recode(habitat, "Pooled"=""),
         habitat=factor(habitat, 
                        levels=c("Surf", "Kelp", "Rocky reef", "Deep reef", ""))) %>% 
  # Remove some
  filter(! (habitat=="" & region!="Pooled")) %>% 
  filter(!is.na(status))


# Plot data
g <- ggplot(data2, aes(x=lrr, y=region, color=habitat, shape=status)) +
  facet_grid(habitat~., space="free_y", scales="free_y") +
  # Estimates
  geom_point(position=position_dodge(width=0.5), size=2) +
  # Error bars
  geom_errorbar(mapping=aes(xmin=lrr_lo, xmax=lrr_hi, linetype=status),
                position=position_dodge(width=0.5), width=0, linewidth=1) +
  # Reference line
  geom_vline(xintercept=0, linetype="dashed") +
  # Labels
  labs(x="Log-response ratio", y="") +
  # Legend
  scale_linetype_manual(name="", values=c("solid", "dotted")) +
  scale_shape_manual(name="", values=c(16, 1)) +
  scale_color_manual(name="Habitat", guide="none",
                       values=c("orange", "darkgreen", "navy", "purple", "black")) +
  # Theme
  theme_bw() + theme1 +
  theme(legend.position = "top",
        legend.margin = margin(rep(-5, 4)))
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig2_biomass_lrr_habitat_centric.png"), 
       width=6.5, height=4.5, units="in", dpi=600)




