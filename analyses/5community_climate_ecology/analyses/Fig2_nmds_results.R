

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
 datadir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/community_climate_derived_data/reprocessed_data_20230615" # Josh

# Directories (Chris)
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data/"
datadir <- file.path(basedir, "monitoring/processed_data/community_climate_derived_data/Chris_file_share/")

# Directories (both)
outdir <- "analyses/5community_climate_ecology/output"
plotdir <- "analyses/5community_climate_ecology/figures"

# Read data
comm_data <- load(file.path(datadir, "comm_data.rda"))
nmds_scores <- load(file.path(datadir, "bray_nmds_scores.rda"))
env_fit_scores <- load(file.path(datadir, "env_fit_scores_with_absolutes.rda"))
group_vars <- load(file.path(datadir, "group_vars.rda"))
envr_vars <- load(file.path(datadir, "envr_vars.rda"))
eco_dist <- load(file.path(datadir, "distance_matrices_BC.rda"))

# Read simulated data
load(file.path(outdir, "simulated_ellipses_new.Rdata"))

#Check number of MPAs

kelp_fish_MPAs <- kelp_fish_group_vars %>% filter(mpa_defacto_designation == "smr") %>% dplyr::select(group, affiliated_mpa) 
rocky_MPAs <-rocky_group_vars %>% filter(mpa_designation == "smr") %>% dplyr::select(group, affiliated_mpa) 
shallow_MPAs <- CCFRP_group_vars %>% filter(mpa_designation == "smr") %>% dplyr::select(group, affiliated_mpa) 
deep_MPAs <- deep_reef_group_vars %>% filter(mpa_defacto_designation == "smr") %>% dplyr::select(group, affiliated_mpa) 

distinct_MPAs <- rbind(kelp_fish_MPAs, rocky_MPAs, shallow_MPAs, deep_MPAs) %>% distinct(affiliated_mpa)

# Perform NMDS analysis
################################################################################

# Ordination objects
habitats <- c("Rocky intertidal", "Kelp forest inverts and algae", "Kelp forest fishes", "Shallow reef", "Deep reef")
arrow_scalars <- c(0.4, 0.25, 0.11, 0.4, 0.4)
list1 <- list(rocky_ord, kelp_invalg_ord, kelp_fish_ord, CCFRP_ord, deep_reef_ord) 
list2 <- list(rocky_en, kelp_invalg_en, kelp_fish_en, CCFRP_en, deep_reef_en)
list3 <- list(rocky_group_vars, kelp_invalg_group_vars, kelp_fish_group_vars, CCFRP_group_vars, deep_reef_group_vars)


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
  # The "fill" parameter dictates arrow length
  arrow_scalar <- arrow_scalars[i]
  arrows_orig <- as.data.frame(vegan::scores(arrows_do, "vectors")) * vegan::ordiArrowMul(arrows_do, fill=arrow_scalar)
  
  # Format arrows
  arrows <- arrows_orig %>% 
    # Add habitat
    mutate(habitat=hab_do) %>% 
    # Add variable
    rownames_to_column(var="variable") %>% 
    mutate(variable=recode(variable, "BT"="SBT")) %>% 
    # Arrange
    dplyr::select(habitat, variable, everything())
  
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
    dplyr::select(-group) %>% 
    dplyr::select(habitat, period, site_type, NMDS1, NMDS2, everything())
  
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
    dplyr::select(habitat, period, site_type, region4, everything())

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
    geom_segment(data = arrows, 
                 x = 0, y = 0, 
                 mapping=aes(xend = NMDS1, yend = NMDS2), 
                 size=0.5, alpha = 0.5,
                 arrow = arrow(length = unit(0.25, "cm")), colour = "grey30", lwd=0.5) +
    geom_point(x=0, y=0, color="black", shape=16) +
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
    arrows_all <- arrows
  }else{
    ellipses_all <- rbind(ellipses_all, ellipse_df)
    centroids_all <- rbind(centroids_all, centroids)
    stress_all <- rbind(stress_all, stress)
    arrows_all <- rbind(arrows_all, arrows)
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

# Format centroids
arrows_all <- arrows_all %>% 
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

# Format simulated data
data_sim <- data_sim %>% 
  mutate(scenario=recode(scenario,
                         "Heatwave impact"="i. Heatwave impact",
                         "No MPA benefit"="ii. No MPA benefit",
                         "MPA resistance"="iii. MPA resistance",
                         "MPA recovery"="iv. MPA recovery"))
data_sim_pts <- data_sim_pts %>% 
  mutate(scenario=recode(scenario,
                         "Heatwave impact"="i. Heatwave impact",
                         "No MPA benefit"="ii. No MPA benefit",
                         "MPA resistance"="iii. MPA resistance",
                         "MPA recovery"="iv. MPA recovery"))
#select vectors to plot
arrows_filtered <- arrows_all %>% 
                dplyr::filter(variable == "SST_anom" |
                                variable == "BEUTI_anom"|
                                variable == "MOCI"|
                                variable == "AT_anom" |
                                variable == "BT_anom")%>%
                mutate(variable = recode_factor(variable,
                                                "SST_anom" = "SST",
                                                "BEUTI_anom" = "BEUTI",
                                                "AT_anom" = "AT",
                                                "BT_anom" = "SBT"))


# Plot data
################################################################################


# Plotting par
colors <- c('#1B9E77','#FF7F00','#984EA3')
pt_size <- 1.3
line_size <- 0.3

# Theme
my_theme <-  theme(axis.text=element_text(size=7, color = "black"),
                   axis.title=element_text(size=8,color = "black"),
                   legend.text=element_text(size=7,color = "black"),
                   legend.title=element_text(size=8,color = "black"),
                   strip.text=element_text(size=8, hjust=0, face="bold",color = "black"),
                   strip.background = element_blank(),
                   plot.title=element_text(size=9,color = "black"),
                   plot.tag = element_text(size=9,color = "black", face = 'bold'),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.5, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
impact_labels <- data.frame(
  scenario = factor("i. Heatwave impact", levels = c("i. Heatwave impact", "ii. No MPA benefit", "iii. MPA resistance", "iv. MPA recovery")),
  site_type = factor("Inside", levels = c("Inside", "Outside")),
  period = factor("Before", levels = c("Before", "During", "After")),
  x = c(1.6, 0),
  y = c(0.8, 1.85),
  label = c("Impact","No\nimpact")
)

# Plot data
g1 <- ggplot(data_sim, aes(x = x, y = y, color = period, linetype = site_type)) +
  facet_wrap(~ scenario, nrow = 1) +
  # Ellipses
  geom_path(data = data_sim, mapping = aes(group = interaction(period, site_type)), linewidth = line_size) +
  geom_point(data = data_sim_pts, mapping = aes(x = x1, y = y1, color = period, shape = site_type), size = pt_size) +
  # Labels
  labs(x = "nMDS1", y = "nMDS2", tag = "(a)", title = "Potential shifts in community structure") +
  # Legends
  scale_linetype_discrete(name = "Site type", drop = FALSE) +
  scale_shape_manual(name = "Site type", values = c(16, 17), drop = FALSE) +
  scale_color_manual(name = "Period", values = colors) +
  # Theme
  theme_bw() +
  my_theme +
  theme(
    legend.position = "none",
    axis.ticks = element_blank(),
    axis.text = element_blank()
  )

# Add text on the first facet
g1 <- g1 +
  geom_text(data = impact_labels[1:2, ], aes(x = x, y = y, label = label), 
            color = "black", fontface="bold", size=2.4)

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
  geom_segment(data = arrows_filtered,
               x = 0, y = 0,
               mapping=aes(xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.25, "cm")), color = "grey60", linewidth=0.5) +
  geom_point(x=0, y=0, color="grey60", shape=16) +
  ggrepel::geom_text_repel(data=arrows_filtered, 
            mapping=aes(x=NMDS1, y=NMDS2, label=variable), size=2.2, fontface="bold", inherit.aes = F) +
  # Add stress labels
  geom_text(data=stress_all, aes(x=NMDS1, y=NMDS2, label=paste("Stress:",stress)), inherit.aes = F,
            hjust=1, size=2.2) +
  # Labels
  labs(x="nMDS1", y="nMDS2", title="Observed shifts in community structure", tag="(b)") +
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
       width=7.54, height=8, units="in", dpi=600)


