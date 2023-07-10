

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(patchwork)

# Chris Directories
#basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data" #Chris
#datadir <- file.path(basedir, "monitoring/processed_data/community_climate_derived_data/statewide_data")
#plotdir <- "analyses/5community_climate_ecology/figures"

# Josh Directories
basedir <- "/home/shares/ca-mpa/data/sync-data"
 datadir <- file.path(basedir, "monitoring/processed_data/community_climate_derived_data/statewide_data")
 plotdir <- "analyses/5community_climate_ecology/figures"

# Read data
data_orig2 <- read.csv(file.path(datadir, "mpa_betadisp_mod_run2.csv"), as.is=T)


# Read MPA metadata
mpas_data <- readRDS(file.path(basedir, "mpa_traits/processed/CA_mpa_metadata.Rds"))

#check sample size

mpa_samp <- data_orig2 %>% filter(MPA_type =="smr") %>% distinct(MPA)

# Processing script
################################################################################
#This is the processing script used to calculate data_orig2 (betadisp model runs)
#this only needs to be performed once, and can be ignored if the mpa_betadisp_mod_run2.csv
#file is loaded above

#load data 
data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/community_climate_derived_data/statewide_data"

#nmds_scores <- load(file.path(data_path, "bray_nmds_scores.rda"))
group_vars <- load(file.path(data_path, "group_vars_statewide.rda"))
envr_vars <- load(file.path(data_path, "envr_vars_statewide.rda"))
eco_dist <- load(file.path(data_path, "distance_matrices_BC_statewide.rda"))


# define test centroids  

rocky_group_vars1 <- rocky_group_vars %>%
  mutate(affiliated_mpa2 = ifelse(affiliated_mpa == "none",
                                  site, affiliated_mpa),
         site_yr_ID = paste(affiliated_mpa2, desig_state))

kelp_fish_group_vars1 <- kelp_fish_group_vars %>%
  mutate(site_yr_ID = paste(affiliated_mpa, desig_state))

kelp_invalg_group_vars1 <- kelp_invalg_group_vars %>%
  mutate(site_yr_ID = paste(affiliated_mpa, desig_state))


# Step 1 - examine hull shape and dispersion 

rocky_disper <- betadisper(rocky_distmat, type = "centroid",
                           group = rocky_group_vars1$site_yr_ID)

kelp_fish_disper <- betadisper(kelp_fish_distmat, type="centroid",
                               group=kelp_fish_group_vars1$site_yr_ID)

kelp_invalg_disper <- betadisper(kelp_invalg_distmat, type = "centroid",
                                 group = kelp_invalg_group_vars1$site_yr_ID)

# Step 2 - calculate distances and pooled s.d.

#create helper function

eig_fun <- function(disper_mat) {
  
  x = melt(as.matrix(sqrt(dist(disper_mat$centroids[,disper_mat$eig>0]^2)-
                            dist(disper_mat$centroids[,disper_mat$eig<0]^2))))
  tibble::rownames_to_column(x, "distance")
  
  x2 <- x %>% mutate(
    MPA1 = word(Var1 , 1  , -3),
    MPA_type1 = word(Var1 , -2),
    MPA_period1 = word(Var1, -1),
    MPA2 = word(Var2 , 1  , -3),
    MPA_type2 = word(Var2 , -2),
    MPA_period2 = word(Var2, -1))
  
  x3 <- x2 %>% filter(MPA1 == MPA2 &
                        MPA_type1 == MPA_type2,
                      MPA_period1 == "before" & MPA_period2 == "after" |
                        MPA_period1 == "before" & MPA_period2 == "during") %>%
    mutate(join_ID = paste(MPA1, MPA_type1, MPA_period1, MPA_period2))
  
  sd = melt(as.matrix(tapply(disper_mat$distances, disper_mat$group, sd)))%>%
    tibble::rownames_to_column() %>% mutate(s_d = value)%>%
    dplyr::select(Var1, s_d)
  
  n = melt(table(disper_mat$group))%>%
    tibble::rownames_to_column() %>% mutate(n = value)%>%
    dplyr::select(Var1, n) 
  
  e_hat <- left_join(sd, n, by='Var1') %>%
    #select(!(c(value, rowname)))%>%
    mutate(MPA_type = factor(word(Var1, -2)),
           MPA_period = factor(word(Var1, -1)),
           MPA = factor(word(Var1 , 1  , -3)))%>%
    dplyr::select(!(Var1))%>%
    pivot_wider(names_from='MPA_period', values_from = c('s_d','n'))%>%
    mutate(sd_pooled_before_after = sqrt(
      ((`n_before`-1)*`s_d_before`^2 + (`n_after`-1)*`s_d_after`^2)/
        (`n_before`+`n_after`-2)
    ),
    sd_pooled_before_during = sqrt(
      ((`n_before`-1)*`s_d_before`^2 + (`n_during`-1)*`s_d_during`^2)/
        (`n_before`+`n_during`-2)
    )
    ) %>%
    pivot_longer(cols=c(`sd_pooled_before_after`,`sd_pooled_before_during`),
                 values_to ="sd_pooled")%>%
    mutate(sub = gsub("_", " ", name),
           MPA_period1 =factor(word(sub, -2, sep = ' ')),
           MPA_period2 = factor(word(sub, -1)),
           join_ID = paste(MPA, MPA_type, MPA_period1, MPA_period2))%>%
    dplyr::select(join_ID, sd_pooled)
  
  left_join(x3, e_hat, by='join_ID')
}


rocky <- eig_fun(rocky_disper) %>% 
  mutate(group = "Rocky intertidal")
kelp_fish <- eig_fun(kelp_fish_disper) %>% 
  mutate(group = "Kelp forest fishes")
kelp_invalg <- eig_fun(kelp_invalg_disper) %>% 
  mutate(group = "Kelp forest inverts and algae")



#combine into df
travel_distance <- as.data.frame(rbind(rocky, kelp_fish,
                                       kelp_invalg))



#Step 3 -- use permanova to test significance

#pariwise permanova

rocky_pair_perm <- pairwise.adonis2(rocky_distmat ~ site_yr_ID, 
                                    data = rocky_group_vars1, 
                                    permutations = 999,
                                    num_cores = 20)

kelp_fish_pair_perm <- pairwise.adonis2(kelp_fish_distmat ~ site_yr_ID, 
                                        data = kelp_fish_group_vars1,
                                        permutations = 999,
                                        num_cores = 20)


kelp_invalg_pair_perm <- pairwise.adonis2(kelp_invalg_distmat ~ site_yr_ID, 
                                          data = kelp_invalg_group_vars1, 
                                          permutations = 999,
                                          num_cores = 20)


##### EXTRACT OUTPUT
# Remove the different list item
rocky_pair_perm$parent_call <- NULL
kelp_fish_pair_perm$parent_call <- NULL
kelp_invalg_pair_perm$parent_call <- NULL

# make it a data frame

extract_fun <- function(mod, habitat) {
  mod$parent_call <- NULL
  mod %>% map_dfr(~tidy(.x), .id="name")%>%
    filter(term == "site_yr_ID")%>%
    mutate(group_1 = str_extract(name, "[^_]+"),
           group_2 = gsub(".*\\_", "", name),
           period_1 = gsub(".*\\ ", "", group_1),
           period_2 = gsub(".*\\ ", "", group_2),
           MPA_int_1 = word(group_1 , 1  , -2),
           MPA_1 = word(MPA_int_1, 1  , -2),
           MPA_int_2 = word(group_2 , 1  , -2),
           MPA_2 = word(MPA_int_2, 1  , -2),
           MPA_type_1 = word(MPA_int_1,-1),
           MPA_type_2 = word(MPA_int_1,-1),
           habitat = habitat
    )%>%
    filter(MPA_int_1 == MPA_int_2,
           period_1 == "before") %>%
    dplyr::select(habitat, "MPA" = MPA_1, "MPA_type" = MPA_type_1,
                  period_1, period_2,
                  SumOfSqs, R2, "F.stat"=statistic,
                  p.value)
}


rocky_fun <- extract_fun(rocky_pair_perm, "Rocky intertidal")
kelp_fish_fun <- extract_fun(kelp_fish_pair_perm, "Kelp forest fishes")
kelp_invalg_fun <- extract_fun(kelp_invalg_pair_perm, "Kelp forest inverts and algae")

mod_out <- rbind(rocky_fun, kelp_fish_fun, kelp_invalg_fun)


##join distance with significance level

#prep distance
b_join <- travel_distance %>%
  select(join_ID, group, "distance"=value, sd_pooled)

#prep model
a_join <- mod_out %>%
  mutate(join_ID = paste(MPA, MPA_type, period_1, period_2))

#join
mpa_output <- left_join(a_join, b_join, by=c("habitat"="group","join_ID"))%>%
  select(!(join_ID))


#write.csv(mpa_output, 
# "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/community_climate_derived_data/statewide_data/mpa_betadisp_mod_run2.csv",
# row.names = FALSE)



# Format data
#This is the processing script used to calculate data_orig2 (betadisp model runs)
#this only needs to be performed once, and can be ignored if the mpa_betadisp_mod_run2.csv
#file is loaded above
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
  left_join(mpas_data %>% dplyr::select(mpa, region, lat_dd), by="mpa") %>% 
  # Create type
  mutate(period=paste(period_1, period_2, sep="-"),
         process=recode_factor(period,
                               "before-during"="Resistance",
                               "before-after"="Recovery")) %>% 
  # Arrange
  dplyr::select(habitat, region, lat_dd, mpa, site_type, process, period, distance) %>% 
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
base_theme <- theme(axis.text=element_text(size=7, color = "black"),
                    axis.title=element_blank(),
                    legend.text=element_text(size=6,color = "black"),
                    legend.title=element_text(size=7,color = "black"),
                    strip.text=element_text(size=7, face = "bold",color = "black"),
                    strip.background = element_blank(),
                    plot.tag=element_text(size=8,color = "black", face = "bold"),
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
                     plot.tag=element_text(size=8,color = "black", face = "bold"),
                     plot.title = element_text(size=7,color = "black"),
                     axis.text=element_text(size=6,color = "black"),
                     axis.title = element_blank(),
                     axis.text.x = element_blank(),
                     axis.text.y=element_text(color=c("navy", "#E41A1C")))

# Colors
RColorBrewer::brewer.pal(3, "Set1")

# Plot schematic 1
toy1 <- tibble(site=factor(c("Inside", "Outside"), levels=c("Outside", "Inside")),
               distance=c(0.5, 0.7))
schem1 <- ggplot(toy1, aes(y=site, yend=site, xend=distance, color=site)) +
  geom_segment(x=0, arrow = arrow(length=unit(0.30, "cm"))) +
  # Labels
  #labs(title = "MPA prevents shifts")+
  labs(title="Shift distance less in MPA", tag="(a)") +
  scale_color_manual(values=c("navy", "#E41A1C")) +
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
  labs(title="Shift distance greater in MPA", tag="(b)") +
  scale_color_manual(values=c( "navy", "#E41A1C")) +
  # Limits
  lims(x=c(0, 0.8)) +
  # Theme
  schem_theme
schem2

# Plot data
g1 <- ggplot(data2 %>%
             mutate(process = ifelse(process == "Resistance","Resistance \n(Before-to-during)","Recovery \n(Before-to-after)"),
                    process = factor(process, levels = c("Resistance","Resistance \n(Before-to-during)", "Recovery \n(Before-to-after)"))
                    )
             , aes(x=habitat, y=mpa, size=distance, fill=dist_perc, color="")) +
  facet_grid(region~process, space="free", scale="free") +
  geom_point(pch=21, color="black") +
  # Labels
  labs(x="", y="", tag="(c)") +
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
cowplot::save_plot(g1_full, filename=file.path(plotdir, "Fig5_resistance_recovery_statewide.png"), 
     base_width=6.5, base_height=6.5, units="in", dpi=600, bg="white", base_asp=0.8)



