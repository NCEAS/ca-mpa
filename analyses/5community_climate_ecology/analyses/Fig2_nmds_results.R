

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(stringr)
library(janitor)
library(vegan)
library(ggplot2)
library(parallel)
library(scales)
library(here)
library(ggpubr)
library(usedist)
library(here)

# Directories (Josh)
# datadir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/community_climate_derived_data" # Josh

# Directories (Chris)
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data/"
datadir <- file.path(basedir, "monitoring/processed_data/community_climate_derived_data/Chris_file_share/")

# Directories (both)
outdir <- "analyses/5community_climate_ecology/output"
plotdir <- "analyses/5community_climate_ecology/figures"

# Read data
comm_data <- load(file.path(datadir, "comm_data.rda"))
nmds_scores <- load(file.path(datadir, "bray_nmds_scores.rda"))
env_fit_scores <- load(file.path(datadir, "env_fit_scores.rda"))
group_vars <- load(file.path(datadir, "group_vars.rda"))
envr_vars <- load(file.path(datadir, "envr_vars.rda"))
eco_dist <- load(file.path(datadir, "distance_matrices_BC.rda"))

# Read simulated data
load(file.path(outdir, "simulated_ellipses.Rdata"))


# Perform NMDS analysis
################################################################################

# Ordination objects
habitats <- c("Shallow reef", "Kelp forest inverts and algae", "Kelp forest fishes", "Deep reef", "Rocky intertidal")
list1 <- list(CCFRP_ord, kelp_invalg_ord, kelp_fish_ord, deep_reef_ord, rocky_ord)
list2 <- list(CCFRP_en, kelp_invalg_en, kelp_fish_en, deep_reef_en, rocky_en)
list3 <- list(CCFRP_group_vars, kelp_invalg_group_vars, kelp_fish_group_vars, deep_reef_group_vars, rocky_group_vars)

# Loop through objects
i <- 1
for(i in 1:length(list1)){
  
  # Data to work on
  hab_do <- habitats[i]
  print(paste(i, hab_do))
  ord_do <- list1[[i]]
  arrows_do <- list2[[i]]
  vars_do <- list3[[i]]
  
  # Extract arrows
  en_coord_cont <- as.data.frame(vegan::scores(arrows_do, "vectors")) * vegan::ordiArrowMul(arrows_do, fill=0.05)
  
  # Format arrows
  # Relabel the "BT" row name as "SBT"
  rownames(en_coord_cont)[rownames(en_coord_cont) == "BT"] <- "SBT"
  
  # Extract data
  data_orig <- as.data.frame(vegan::scores(ord_do, display="sites"))
  
  # Format vars
  if("mpa_designation" %in% names(vars_do)){
    vars_do <- vars_do %>% 
      rename(mpa_defacto_designation=mpa_designation)
  }
  
  # Format data
  data <- data_orig %>% 
    # Add variables
    mutate(desig_state=vars_do$desig_state, 
           year=vars_do$year, 
           MHW=vars_do$MHW, 
           region4=vars_do$region4, 
           mpa_designation=vars_do$mpa_defacto_designation) %>% 
    # Format variables
    mutate(region4 = factor(region4, levels = c("north", "central", "north islands", "south")),
           MHW = factor(MHW, levels = c("before","during","after")), 
           mpa_designation = factor(mpa_designation, levels = c("smr", "ref")))
  
  # Compute data centroids
  centroids_orig <- aggregate(cbind(NMDS1, NMDS2, NMDS3) ~ MHW+region4+mpa_designation, data = data, FUN = mean) #computes centroids by MHW and region
  
  # Merge data and centroids
  segs <- merge(data, setNames(centroids_orig, c('year','oNMDS1','oNMDS2')), by = 'year', sort=FALSE) 
  
  # To draw the spider, we need to use geom_segment() which required coordinates to draw the segment from and to. Our 'to'
  # coordinates, the xend and yend aesthetics will be the centroids. So we need to replicate the group centroid for each
  # observation in the group. T
  
  # Build a thing
  NMDS <- tibble(MDS1 = ord_do$points[,1], 
                 MDS2 = ord_do$points[,2],
                 group = vars_do$desig_state,
                 MHW = vars_do$MHW, 
                 mpa_desig = vars_do$mpa_defacto_designation) %>% 
    # Fractor group
    mutate(group=as.factor(group))
  
  # Plot the ordinated data
  plot(ord_do)
  ord <- vegan::ordiellipse(ord_do, vars_do$desig_state, kind = "se", conf=0.95, label=T)
  
  # Build ellipses
  ellipse_df_orig <- data.frame()
  for(g in levels(NMDS$group)){
    ellipse_df_orig <- rbind(ellipse_df_orig, cbind(as.data.frame(with(NMDS[NMDS$group==g,],
                                                     vegan:::veganCovEllipse(ord[[g]]$cov, ord[[g]]$center, ord[[g]]$scale)))
                                  , group=g))
  }
  
  # Format ellipse data
  ellipse_df <- ellipse_df_orig %>% 
    # Add habitat
    mutate(habitat=hab_do) %>% 
    # Add site type
    mutate(site_type = stringr::word(group, 1)) %>% 
    # Format site type
    mutate(site_type=recode_factor(site_type,
                                   "smr"="Inside",
                                   "ref"="Outside")) %>% 
    # Add period
    mutate(period = word(group, start = -1)) %>% 
    # Format period
    mutate(period=recode_factor(period, 
                                "before"="Before",
                                "during"="During", 
                                "after"="After")) %>% 
    # Arrange
    select(-group) %>% 
    select(habitat, period, site_type, NMDS1, NMDS2, everything())
  
  # Format data centroids
  centroids <- centroids_orig %>% 
    # Add habitat
    mutate(habitat=hab_do) %>% 
    # Rename
    rename(site_type=mpa_designation, period=MHW) %>% 
    # Format site type
    mutate(site_type=recode_factor(site_type, 
                                   "smr"="Inside",
                                   "ref"="Outside")) %>% 
    # Format period
    mutate(period=recode_factor(period, 
                                "before"="Before",
                                "during"="During", 
                                "after"="After")) %>% 
    # Arrange
    select(habitat, period, site_type, region4, everything())

  # Compute stress
  stress <- as.data.frame(ord_do[["stress"]]) %>% 
    dplyr::rename("stress" = 1) %>%
    mutate_at(1, round, 3) %>% 
    # Add habitat
    mutate(habitat=hab_do)
  
  # Plot data
  g <- ggplot(data=centroids, aes(x=NMDS1, y=NMDS2, color=period)) +
    # Ellipses
    geom_path(data=ellipse_df, mapping=aes(linetype=site_type)) +
    # Centroids
    geom_point(mapping=aes(shape=site_type)) +
    # Arrows
    geom_segment(data = en_coord_cont, 
                 x = 0, y = 0, 
                 mapping=aes(xend = NMDS1, yend = NMDS2), 
                 size=0.5, alpha = 0.5,
                 arrow = arrow(length = unit(0.25, "cm")), colour = "grey30", lwd=0.5) +
    # Labels
    labs(x="NMDS1", y="NMDS2", title=hab_do) +
    # Legend
    scale_color_manual(name="Period", values=c('1B9E77','#FF7F00','#984EA3')) +
    scale_linetype_discrete(name="Site type") +
    scale_shape_discrete(name="Site type") +
    # Theme
    theme_bw()
  print(g)
  
  # Merge data
  if(i == 1){
    ellipses_all <- ellipse_df
    centroids_all <- centroids
    stress_all <- stress
  }else{
    ellipses_all <- rbind(ellipses_all, ellipse_df)
    centroids_all <- rbind(centroids_all, centroids)
    stress_all <- rbind(stress_all, stress)
  }
  
}

# Format data
################################################################################

# Order habitats
habitat_order <- c("Rocky intertidal",  "Kelp forest inverts and algae", "Kelp forest fishes", "Shallow reef", "Deep reef")

# Format ellipses
ellipses_all <- ellipses_all %>% 
  mutate(habitat=factor(habitat, levels=habitat_order))

# Format centroids
centroids_all <- centroids_all %>% 
  mutate(habitat=factor(habitat, levels=habitat_order))

# Stress coordinates
label_xy <- ellipses_all %>% 
  group_by(habitat) %>% 
  summarize(NMDS1=max(NMDS1),
            NMDS2=max(NMDS2)) %>% 
  ungroup()

# Format stress
stress_all <- stress_all %>% 
  # Order habiats
  mutate(habitat=factor(habitat, levels=habitat_order)) %>% 
  # Add coordinates
  left_join(label_xy)



# Plot data
################################################################################

# Add stress tag
# Fix arrow problem?
# Add arrow labels

# Plotting par
colors <- c("darkgreen", "orange", "purple")
pt_size <- 1.2
line_size <- 0.3

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   plot.tag = element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.5, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot(data_sim, aes(x=x, y=y, color=period, linetype=site_type)) +
  facet_wrap(~scenario, nrow=1) +
  # Ellipses
  geom_path(linewidth=line_size) +
  geom_point(data_sim_pts, mapping=aes(x=x1, y=y1, color=period, shape=site_type), size=pt_size) +
  # Labels
  labs(x="nMDS1", y="nMDS2", tag="A", title="Potential MPA outcomes") +
  # Legends
  scale_linetype_manual(name="Site type", values=c("solid", "dotted"), drop=F) +
  scale_shape_manual(name="Site type", values=c(16, 17), drop=F) +
  scale_color_manual(name="Period", values=colors) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        axis.text=element_blank())
g1

# Plot data
g2 <- ggplot(data=centroids_all, aes(x=NMDS1, y=NMDS2, color=period)) +
  # Facet
  facet_wrap(~habitat, ncol=3, scales="free") +
  # Ellipses
  geom_path(data=ellipses_all, mapping=aes(linetype=site_type), linewidth=line_size) +
  # Centroids
  geom_point(mapping=aes(shape=site_type), size=pt_size) +
  # Arrows
  geom_segment(data = en_coord_cont,
               x = 0, y = 0,
               mapping=aes(xend = NMDS1, yend = NMDS2),
               size=0.5, alpha = 0.5,
               arrow = arrow(length = unit(0.25, "cm")), colour = "grey30", lwd=0.5) +
  # Add stress labels
  geom_text(data=stress_all, aes(x=NMDS1, y=NMDS2, label=stress), inherit.aes = F,
            hjust=1, size=2.2) +
  # Labels
  labs(x="nMDS1", y="nMDS2", title="Observed MPA outcomes by habitat", tag="B") +
  # Legend
  scale_color_manual(name="Heatwave period", values=colors) +
  scale_linetype_discrete(name="Site type") +
  scale_shape_discrete(name="Site type") +
  guides(color = guide_legend(order = 2), linetype = guide_legend(order = 1), shape=guide_legend(order=1)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.8, 0.2),
        legend.margin = margin(0, 0, 0, 0))
g2

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, nrow=2, heights=c(0.33, 0.67))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig2_nmds_results.png"), 
       width=6.5, height=6.5, units="in", dpi=600)








