

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(patchwork)

# Directories
datadir <- "~/Desktop"
plotdir <- "analyses/5community_climate_ecology/figures"

# Read data
data_orig <- read.csv(file.path(datadir, "MPA_centroid_distances_with_traits.csv"), as.is=T)

# Read MPA metadata
mpas_data <- readRDS(file.path(datadir, "CA_mpa_metadata.Rds"))


# Format data
################################################################################

# Build data
data <- data_orig %>%
  # Rename
  janitor::clean_names() %>% 
  # Reduce
  select(mpa, habitat, process, distance) %>% 
  # Arrange process
  mutate(process=factor(process, levels=c("Resistance", "Recovery"))) %>% 
  # Arrange habitats
  mutate(habitat=recode_factor(habitat,
                               "Rocky intertidal"="Rocky\nintertidal",
                               "Kelp forest inverts and algae"="Kelp forest\ninverts/algae",
                               "Kelp forest fishes"="Kelp forest\nfishes")) %>% 
  # Format MPA names
  mutate(mpa=stringr::str_to_title(mpa),
         mpa=gsub("Smr", "SMR", mpa),
         mpa=gsub("Smca", "SMCA", mpa),
         mpa=recode(mpa, 
                    "Point Vicente SMCA"="Point Vicente SMCA (No-Take)",
                    "Campus Point SMCA"="Campus Point SMCA (No-Take)",
                    "Blue Cavern Onshore SMCA"="Blue Cavern Onshore SMCA (No-Take)")) %>% 
  # Add region
  left_join(mpas_data %>% select(mpa, region, lat_dd), by="mpa") %>% 
  # Arrange MPAs
  mutate(region=recode_factor(region,
                              "North Central Coast"="North",
                              "Central Coast"="Central",
                              "South Coast"="South")) %>% 
  arrange(desc(lat_dd)) %>% 
  mutate(mpa=factor(mpa, levels = unique(mpa) %>% rev())) %>% 
  # Add simulates perf shift
  mutate(dist_perc=runif(n=n(), -0.55, 0.55),
         dist_perc=ifelse(habitat=="Rocky\nintertidal", NA, dist_perc))


# Perform regression
################################################################################

# Resistance model
##########################################

# Resistance data
resist_all <- data_orig %>%
  filter(process == "Resistance")%>%
  filter(!(is.na(habitat_diversity)))

# Resistance model
resist_mod_all <- glm(test_stat ~ size + habitat_diversity + prop_rock + fishing_pressure +
                        habitat_richness,
                      data = resist_all, 
                      family=binomial(link="logit"), na.action = na.exclude)
summary(resist_mod_all)

# Build logs ratios
resis_lr <- exp(cbind(coef(resist_mod_all), confint(resist_mod_all)))  %>% 
  as.data.frame() %>% 
  setNames(c("odds", "odds_lo", "odds_hi")) %>% 
  rownames_to_column(var = "variable") %>% 
  mutate(process="Resistance",
         pvalue=coef(summary(resist_mod_all))[,4]) %>% 
  select(process, variable, everything())


# Recovery model
##########################################

# Recovery data
recov_all <- data_orig %>%
  filter(process == "Recovery")%>%
  filter(!(is.na(habitat_diversity)))

# Recovery mode;
recov_mod_all <- glm(test_stat ~ size + habitat_diversity + prop_rock + fishing_pressure +
                       habitat_richness,
                     data = recov_all, 
                     family=binomial(link="logit"), na.action = na.exclude)%>%
  MASS::stepAIC(trace=FALSE)
summary(recov_mod_all)


# Build logs ratios
recov_lr <- exp(cbind(coef(recov_mod_all), confint(recov_mod_all)))  %>% 
  as.data.frame() %>% 
  setNames(c("odds", "odds_lo", "odds_hi")) %>% 
  rownames_to_column(var = "variable") %>% 
  mutate(process="Recovery",
         pvalue=coef(summary(recov_mod_all))[,4]) %>% 
  select(process, variable, everything())

# Merge data
odds <- bind_rows(resis_lr, recov_lr) %>% 
  # Remove intercept
  filter(variable!="(Intercept)") %>% 
  # Format variable
  mutate(variable=recode(variable,
                         "size"="MPA size (sqkm)",
                         "prop_rock"="Proportion rock habitat",
                         "habitat_richness"="Habitat richness",
                         "habitat_diversity"="Habitat diversity",
                         "fishing_pressure"="Fishing pressure")) %>% 
  # Order process
  mutate(process=factor(process, levels=c("Resistance", "Recovery"))) %>% 
  # Mark sig
  mutate(sig=pvalue<=0.05) %>% 
  # Arrange
  arrange(process, desc(odds)) %>% 
  mutate(order=1:n()) %>% 
  mutate(variable= tidytext::reorder_within(variable, order, within = process))


# Plot data
################################################################################

# Base theme
base_theme <- theme(axis.text=element_text(size=7),
                    axis.title=element_text(size=8),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=7),
                    strip.text=element_text(size=7),
                    plot.tag=element_text(size=9),
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
                     plot.title = element_text(size=7),
                     axis.text=element_text(size=6),
                     plot.tag=element_text(size=9),
                     axis.title = element_blank(),
                     axis.text.x = element_blank(),
                     axis.text.y=element_text(color=c("#377EB8", "#E41A1C")))

# Colors
RColorBrewer::brewer.pal(2, "Set1")

# Plot schematic 1
toy1 <- tibble(site=factor(c("MPA", "Reference"), levels=c("Reference", "MPA")),
               distance=c(0.5, 0.7))
schem1 <- ggplot(toy1, aes(y=site, yend=site, xend=distance, color=site)) +
  geom_segment(x=0, arrow = arrow(length=unit(0.30, "cm"))) +
  # Labels
  labs(title="MPA prevents shifts", tag="A") +
  scale_color_manual(values=c("#377EB8", "#E41A1C")) +
  # Limits
  lims(x=c(0, 0.8)) +
  # Theme
  schem_theme
schem1

# Plot schematic 1
toy2 <- tibble(site=factor(c("MPA", "Reference"), levels=c("Reference", "MPA")),
               distance=c(0.7, 0.5))
schem2 <- ggplot(toy2, aes(y=site, yend=site, xend=distance, color=site)) +
  geom_segment(x=0, arrow = arrow(length=unit(0.30, "cm"))) +
  # Labels
  labs(title="MPA exacerbates shifts", tag=" ") +
  scale_color_manual(values=c( "#377EB8", "#E41A1C")) +
  # Limits
  lims(x=c(0, 0.8)) +
  # Theme
  schem_theme
schem2

# Plot data
g1 <- ggplot(data, aes(x=habitat, y=mpa, size=distance, fill=dist_perc)) +
  facet_grid(region~process, space="free_y", scale="free_y") +
  geom_point(pch=21) +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_size_continuous(name="Shift distance\n(smaller = more resilient)") +
  scale_fill_gradient2(name="% of shift\nprevented (red)\nor exacerbated (blue)",
                       midpoint=0, high="#E41A1C", low="#377EB8", mid="white") +
  guides(size = guide_legend(order = 1),
         fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title = element_blank(),
        axis.text = element_text(size=6),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g1

# Merge
layout_matrix <- matrix(c(1,2,
                          3,3), ncol=2, byrow=T)
g1_full <- gridExtra::grid.arrange(schem1, schem2, g1, 
                                   layout_matrix=layout_matrix,
                                   heights=c(0.1, 0.9))
g1_full

# Plot odds
g2 <- ggplot(odds, aes(x=odds, y=variable, color=sig)) +
  facet_grid(process~., scales="free_y", space="free_y") +
  # Ref line
  geom_vline(xintercept=1, color="grey70") +
  # Plot data
  geom_errorbar(mapping=aes(xmin=odds_lo, xmax=odds_hi), alpha=0.5, width=0) +
  geom_point() +
  # Labels
  labs(x="Odds ratio", y="", tag="B") +
  scale_x_continuous(trans="log10", 
                     breaks=c(0.1, 1, 10, 100, 1000, 10000),
                     labels=c("0.1", "1", "10", "100", "1,000", "10,000")) +
  tidytext::scale_y_reordered() +
  scale_color_manual(values=c("black", "#E41A1C")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="none",
        axis.text = element_text(size=5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g2

# Plot data
g3 <- ggeffects::ggpredict(recov_mod_all, terms = "size[all]") %>%
  ggplot(aes(x, predicted)) +
  # Plot fit
  geom_smooth(span=0.25, color='black') +
  # Plot rug
  geom_point(aes(x=size, y=test_stat), data=recov_all)+
  # Plot confidence interval
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  # Plot p-value
  annotate(geom="text", x=0, y=0.95, label="p=0.015", hjust=0, size=2.2) +
  # Labels
  labs(x="MPA size (sqkm)", y="Probability of recovery", tag="C") +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
g3

# Plot data
g4 <- ggpredict(recov_mod_all,terms = "prop_rock[all]") %>%
  ggplot(aes(x, predicted)) +
  # Plot fits
  geom_smooth(span=0.25, color='black') +
  # Plot rug
  geom_point(aes(x=prop_rock, y=test_stat), data=recov_all) +
  # Plot confidence interval
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  # Plot p-value
  annotate(geom="text", x=0, y=0.95, label="p=0.029", hjust=0, size=2.2) +
  # Labels
  labs(x="Proportion rock habitat", y="Probability of recovery", tag="D") +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
g4

# Merge plots
layout_matrix <- matrix(c(1,2,
                          1,3,
                          1,4), ncol=2, byrow=T)
g <- gridExtra::grid.arrange(g1_full, g2, g3, g4,
                             layout_matrix=layout_matrix, widths=c(0.7, 0.3))

# Export
ggsave(g, filename=file.path(plotdir, "Fig5_resilience_traits.png"), 
       width=6.5, height=6.5, units="in", dpi=600)



