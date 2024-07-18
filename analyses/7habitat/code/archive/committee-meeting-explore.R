# Plot response ratios - Cori proposal work
# 



# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- here::here("analyses","1performance_eco","output")
aurora <- "/home/shares/ca-mpa/data/sync-data/"
gisdir <- "/home/shares/ca-mpa/data/sync-data/gis_data/processed"
plotdir <- here::here("analyses","1performance_eco","figures")

# Read data
mpa_dat <- readRDS(file.path(basedir, "biomass_richness_diversity2.Rds"))

unique(mpa_dat$state_name)

# Read data
state_waters_poly <- readRDS(file.path(gisdir, "CA_state_waters_polygons.Rds"))
state_waters_line <- readRDS(file.path(gisdir, "CA_state_waters_polyline.Rds"))
mpas_orig <- readRDS(file.path(aurora, "mpa_traits/processed", "CA_mpa_metadata.Rds"))


pmep_sub <- readRDS(file.path(aurora, "habitat_pmep/processed/substrate", "mpa_substrate_intersection_totals.Rds")) %>% 
  ungroup() %>% 
  mutate(affiliated_mpa = str_to_lower(name)) 
  

mpa_att <- readRDS(file.path(aurora, "mpa_traits", "processed", "mpa_attributes_habitat.Rds")) %>% 
  select(name, size_km2, hard_0_30 = hard_substrate_0_30m_km2_comb, kelp = max_kelp_canopy_cdfw_km2) %>% 
  mutate(affiliated_mpa = str_to_lower(name)) # shared


# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")


################################################################################


# Format sites
sites <- mpa_dat %>% 
  #match case with mpas_orig
  mutate(habitat = factor(habitat, levels = c("Surf zone","Kelp forest","Shallow reef","Deep reef")),
         state_region = factor(state_region, levels = c("North Coast","North Central Coast","Central Coast","South Coast")),
         state_name = factor(state_name),
         state_name = str_to_title(state_name),
         state_name = sub("([A-Za-z]+)$", "\\U\\1", state_name, perl = TRUE),
         #rename MPAs to match mpa_orig for join
         state_name = ifelse(state_name == "Campus Point SMCA","Campus Point SMCA (No-Take)",
                             ifelse(state_name == "Blue Cavern Onshore SMCA","Blue Cavern Onshore SMCA (No-Take)",
                                    ifelse(state_name == "Point Vicente SMCA","Point Vicente SMCA (No-Take)",state_name)))
         
  )%>%
  #join spatial data with each mpa
  left_join(mpas_orig, by = c("state_name" = "mpa")) %>%
  dplyr::select(habitat, year, state_region,state_name, 
                mpa_full, mpa_short, mlpa, authority, type, ccr, ccr_int, region_code, 
                block_id, area_sqkm, long_dd, lat_dd, everything())

# Check names
sites_in_data <- sort(unique(sites$state_name))
sites_in_data[!sites_in_data %in% mpas_orig$state_name]

# Format PMEP
pmep_build <- pmep_sub %>% 
  select(name, region, affiliated_mpa, PMEP_Zone_Desc, CMECS_SC_Category_Consolidated, area_km) %>% 
  pivot_wider(names_from = CMECS_SC_Category_Consolidated, 
              values_from = area_km)

# Summarize
################################################################################

# Build data
habitats_per_mpa <- sites %>%
  group_by(state_region, state_name) %>%
  summarise(unique_habitats = n_distinct(habitat))


# MPAs
mpas <- mpas_orig %>% 
  # Reduce to MPAs of interest
  filter(mlpa=="MLPA") %>% 
  rename(state_name = mpa)%>%
  # Add stats
  left_join(habitats_per_mpa)

# Subset
mpas_zero <- mpas %>% filter(is.na(unique_habitats))
mpas_data <- mpas %>% filter(!is.na(unique_habitats))


# Plot data
################################################################################

# MPA regions
# CA/OR, Alder Creek, Pigeon Point, Point Conception, CA/MEX 
region_lats <- c(39.0, 37.18, 34.5)

# Region labels
region_labels <- tibble(long_dd=c(-123.9, -122.9, -121, -118#, -119.5
),
lat_dd=c(40.5, 38.7, 36, 34.1#, 34.8
),
label=c("North\n(Dec 2012)", "North Central\n(May 2010)", "Central\n(Sep 2007)", "South\n(Jan 2012)"#, "N. Channel\nIslands (2003)"
))

# Theme
base_theme <-  theme(axis.text=element_text(size=7),
                     axis.title=element_text(size=8),
                     legend.text=element_text(size=7),
                     legend.title=element_text(size=8),
                     plot.tag=element_text(size=8),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.key = element_rect(fill=alpha('blue', 0)),
                     legend.background = element_rect(fill=alpha('blue', 0)),
                     #facets
                     strip.text = element_text(size=7, face = "bold")
)

# Plot data
g1 <- ggplot() +
  # Plot regions
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot state waters
  geom_sf(data=state_waters_line, color="grey40", lwd=0.1) +
  # Plot MPAs
  geom_point(data=mpas_data, mapping=aes(x=long_dd, y=lat_dd, size=unique_habitats), color="grey50") +
  geom_point(data=mpas_zero, mapping=aes(x=long_dd, y=lat_dd), shape="x", size=3) +
  # Plot region labels
  geom_text(data=region_labels, mapping=aes(x=long_dd, y=lat_dd, label=label, fontface="bold"), hjust=0, size=2.3) +
  # Labels
  labs(x="", y="", tag="A") +
  scale_size_continuous(name="No. ecosystems \nmonitored") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  # Crop
  coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title=element_blank(),
        legend.position = c(0.8, 0.7),
        legend.key.size = unit(0.4, "cm"))
g1


sites_plot <- sites %>% 
  #use age > 0 
  filter(age_at_survey > 0) %>%
  #note:: Big Creek SMCA and Southeast Farallon Island SMCA are two MPAs from deep reef included here,
  #but they are not in Fig2 because there was no variance associated with biomass for the meta analysis
  group_by(state_region, habitat, affiliated_mpa) %>%
  summarize(Biomass = mean(target_biomass_logRR),
            Richness = mean(richness_unweighted_logRR),
            Diversity = mean(shannon_unweighted_logRR)) %>%
  pivot_longer(cols = c("Biomass", "Richness", "Diversity"),
               names_to = "factor",
               values_to = "value") %>%
  mutate(state_region = case_when(state_region == "North Coast" ~ "North",
                                  state_region == "North Central Coast" ~ "N. Central",
                                  state_region == "Central Coast" ~ "Central",
                                  state_region == "South Coast" ~ "South",
                                  TRUE ~ state_region),
         state_region = factor(state_region, levels = c("North","N. Central","Central","South"))) 

older_sites <- sites %>% 
  filter(age_at_survey > 3) %>% 
  group_by(state_region, habitat, affiliated_mpa) %>%
  summarize(Biomass = mean(target_biomass_logRR),
            Richness = mean(richness_unweighted_logRR),
            Diversity = mean(shannon_unweighted_logRR)) %>%
  pivot_longer(cols = c("Biomass", "Richness", "Diversity"),
               names_to = "factor",
               values_to = "value") %>%
  mutate(state_region = case_when(state_region == "North Coast" ~ "North",
                                  state_region == "North Central Coast" ~ "N. Central",
                                  state_region == "Central Coast" ~ "Central",
                                  state_region == "South Coast" ~ "South",
                                  TRUE ~ state_region),
         state_region = factor(state_region, levels = c("North","N. Central","Central","South"))) 
  


biomass_only <- sites_plot %>% 
  ungroup() %>% 
  filter(factor == "Biomass") %>% 
  left_join(., pmep_sub, relationship = "many-to-many") %>% 
  left_join(., mpa_att %>% select(affiliated_mpa, size_km2)) %>% 
  clean_names() %>% 
  filter(cmecs_sc_category_consolidated %in% c("Rock Substrate", "Unconsolidated Substrate")) %>% 
  mutate(pmep_zone_desc = if_else(pmep_zone_desc == "Landward", "Intertidal", pmep_zone_desc)) %>% 
  mutate(pmep_zone_desc = factor(pmep_zone_desc,, levels = c("Landward", "0 to 30m", "30 to 100m", "100 to 200m", ">200m")))

old_biomass_only <- older_sites %>% 
  ungroup() %>% 
  filter(factor == "Biomass") %>% 
  left_join(., pmep_sub, relationship = "many-to-many") %>% 
  left_join(., mpa_att %>% select(affiliated_mpa, size_km2)) %>% 
  clean_names() %>% 
  filter(cmecs_sc_category_consolidated %in% c("Rock Substrate", "Unconsolidated Substrate")) %>% 
  mutate(pmep_zone_desc = case_when(pmep_zone_desc == "Landward" ~ "Intertidal", 
                                    TRUE~pmep_zone_desc)) %>% 
  mutate(pmep_zone_desc = factor(pmep_zone_desc,, levels = c("Intertidal", "0 to 30m", "30 to 100m", "100 to 200m", ">200m")))
  

ggplot(data = old_biomass_only %>% filter(habitat == "Surf zone")) + 
  geom_point(aes(x = area_km, y = value)) +
  geom_smooth(aes(x = area_km, y = value), method = "lm") +
  geom_hline(aes(yintercept = 0)) +
  labs(x = "Area of habitat within MPA (km^2)",
       y = "Log response ratio of targeted fish biomass") +
  theme_minimal()+
  facet_wrap(~cmecs_sc_category_consolidated+pmep_zone_desc, scales = "free_x", nrow = 2)

ggplot(data = old_biomass_only %>% filter(habitat == "Kelp forest")) + 
  geom_point(aes(x = area_km, y = value)) +
  geom_smooth(aes(x = area_km, y = value), method = "lm") +
  geom_hline(aes(yintercept = 0)) +
  labs(x = "Area of habitat within MPA (km^2)",
       y = "Log response ratio of targeted fish biomass") +
  theme_minimal()+
  facet_wrap(~cmecs_sc_category_consolidated+pmep_zone_desc, scales = "free_x", nrow = 2)

ggplot(data = old_biomass_only %>% filter(habitat == "Shallow reef")) + 
  geom_point(aes(x = area_km, y = value)) +
  geom_smooth(aes(x = area_km, y = value), method = "lm") +
  geom_hline(aes(yintercept = 0)) +
  labs(x = "Area of habitat within MPA (km^2)",
       y = "Log response ratio of targeted fish biomass") +
  theme_minimal()+
  facet_wrap(~cmecs_sc_category_consolidated + pmep_zone_desc, scales = "free_x", nrow = 2)

ggplot(data = old_biomass_only %>% filter(habitat == "Deep reef")) + 
  geom_point(aes(x = area_km, y = value)) +
  geom_smooth(aes(x = area_km, y = value), method = "lm") +
  geom_hline(aes(yintercept = 0)) +
  labs(x = "Area of habitat within MPA (km^2)",
       y = "Log response ratio of targeted fish biomass") +
  theme_minimal()+
  facet_wrap(~cmecs_sc_category_consolidated +pmep_zone_desc, scales = "free_x", nrow = 2)

ggplot(data = old_biomass_only) +
  geom_point(aes(x = size_km2, y = value)) +
  geom_smooth(aes(x = size_km2, y = value), method = "lm") +
  geom_hline(aes(yintercept = 0)) +
  labs(x = "Total MPA size (km2)",
       y = "Log response ratio of targeted fish biomass") +
  facet_wrap(~habitat, scales = "free_x", nrow = 1)


# Plot response ratios
g2 <-  ggplot(biomass_only %>% filter(habitat == "Kelp forest"), 
              aes(x=habitat, y=reorder(affiliated_mpa, value), fill=value))+
  facet_grid(state_region~factor, space="free_y", scales="free_y") +
  geom_raster() +
  #habitat labels
  # scale_x_discrete(labels = c("SZ", "KF", "SR", "DR")) +
  # Labels
  labs(y="MPA", x="Habitat", 
       #tag="B",
       fill = "LogRR") +
  # Legend
  scale_fill_gradient2(low = "navyblue",mid="gray80",high = "indianred") +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y=element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x=element_blank(),
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) 
g2


# Bring in the habitat attributes
mpa_att <- 


# Merge figures
#g <- gridExtra::grid.arrange(g1, g2, ncol=2, widths=c(0.55, 0.45))
g <- gridExtra::grid.arrange(g1, g2, ncol=2, widths=c(0.5, 0.5))
g

# Export figure
#ggsave(g, filename=file.path(plotdir, "Fig1_map_figure5.png"), 
#       width=7.5, height=6, units="in", dpi=600)


# Kelp Sensitivity Analysis
kelp_sens <- readRDS(file.path(aurora, "habitat_pmep/processed/biotic", "resolution-sensitivity-results-kelp.Rds"))

# Average difference for each resolution (across different bboxes)
average <- kelp_sens %>% 
  group_by(res) %>% 
  summarize(res_mean = mean(diff_abs),
            res_sd = sd(diff_abs))


## d. Plot differences --------------------------------------------------
# Plot of differences
ggplot(data = average %>% filter(res < 300), aes(x = res, y = res_mean)) + 
  geom_col() +
  geom_errorbar(aes(ymin = res_mean, ymax = res_mean + res_sd)) +
  theme_minimal() +
  labs(x = "Resolution", y = "Average proportional difference from polygon area")




