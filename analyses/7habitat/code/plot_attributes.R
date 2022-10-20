# Habitat Coverage Figure
# 25 Aug 2022

# Figure Styling Adapted from C. Free

# Setup ------------------------------------------------------------------------
# Packages
library(tidyverse)

# Clear workspace
rm(list = ls())

# Directories
data.dir <- file.path(getwd(), "analyses", "7habitat", "intermediate_data")
plot.dir <- file.path(getwd(), "analyses", "7habitat", "figures")

# Read Attribute (Habitat) Data
att <- readRDS(file.path(data.dir, "mpa_attributes_processed.Rds"))

# Build ------------------------------------------------------------------------
## Specify habitat lists ----
linear_habitats <- c("sandy_beach_km",
                     "rocky_inter_km",
                     "coastal_marsh_km",
                     "tidal_flats_km",
                     "hardened_armored_shore_km")

area_habitats <- c("hard_substrate_0_30m_km2_comb", 
                   "hard_substrate_30_100m_km2", 
                   "hard_substrate_100_200m_km2",
                   "hard_substrate_200_3000m_km2",
                   "soft_substrate_0_30m_km2_comb", 
                   "soft_substrate_30_100m_km2", 
                   "soft_substrate_100_200m_km2", 
                   "soft_substrate_200_3000m_km2",
                   "max_kelp_canopy_cdfw_km2")

## Select variables and lengthen ----
data <- att %>% 
  pivot_longer(cols = hard_substrate_0_30m_km2_comb:hardened_armored_shore_km,
               names_to = "habitat",
               values_to = "habitat_amount") %>% 
  mutate(habitat_type = if_else(habitat %in% linear_habitats, 'linear', 'area')) 

## Create list of MPAs with incomplete data ----
incomplete <- data %>%
  filter(is.na(habitat_amount)) %>%
  group_by(name, habitat_type) %>%
  summarize(n = n()) %>%
  pivot_wider(names_from = "habitat_type", values_from = "n")

incomplete_linear <- incomplete %>% 
  filter(is.na(linear))

incomplete_area <- incomplete %>% 
  filter(is.na(area))


## Summarize total habitat area for each habitat type ----
type_totals <- data %>% 
  group_by(name, habitat_type) %>% 
  summarize(total_area_calc = case_when(habitat_type == "area" ~ sum(habitat_amount, na.rm = TRUE)),
            total_linear_calc = case_when(habitat_type == "linear" ~ sum(habitat_amount, na.rm = TRUE))) %>% 
  distinct()

## Join total habitat area to dataframe ----
data2 <- data %>% 
  left_join(., type_totals)

## Calculate proportion and percentage from given and calculated values ----
data3 <- data2 %>% 
  mutate(#habitat_prop_given = case_when(habitat_type == "linear" ~ habitat_amount/shore_span_km,
         #                               habitat_type == "area" ~   habitat_amount/size_km2),
         habitat_prop_calc =  case_when(habitat_type == "linear" ~ habitat_amount/total_linear_calc,
                                        habitat_type == "area" ~   habitat_amount/total_area_calc)) %>% 
  # Identify incomplete mapping as incomplete
  mutate(habitat = if_else((name %in% incomplete_linear$name) &
                             habitat_type == "linear", "Incomplete", habitat)) %>% 
  mutate(habitat = if_else((name %in% incomplete_area$name) &
                             habitat_type == "area", "Incomplete", habitat)) %>% 
  mutate(habitat_prop_calc = if_else(habitat == "Incomplete", 1, habitat_prop_calc)) %>% 
  # Recode habitats as factors and give names for legend
  mutate(habitat = recode_factor(habitat,
                                 "soft_substrate_0_30m_km2_comb" = "Soft substrate (0-30m)",
                                 "soft_substrate_30_100m_km2" = "Soft substrate (30-100m)",
                                 "soft_substrate_100_200m_km2" = "Soft substrate (100-200m)",
                                 "soft_substrate_200_3000m_km2" = "Soft substrate (200-3000m)",
                                 "hard_substrate_0_30m_km2_comb" = "Hard substrate (0-30m)",
                                 "hard_substrate_30_100m_km2" = "Hard substrate (30-100m)",
                                 "hard_substrate_100_200m_km2" = "Hard substrate (100-200m)",
                                 "hard_substrate_200_3000m_km2" = "Hard substrate (200-3000m)",
                                 "max_kelp_canopy_cdfw_km2" = "Kelp canopy",
                                 "sandy_beach_km" = "Sandy beach",
                                 "rocky_inter_km" = "Rocky intertidal",
                                 "coastal_marsh_km" = "Coastal marsh",
                                 "tidal_flats_km" = "Tidal flats",
                                 "hardened_armored_shore_km" = "Armored shore",
                                 "Incomplete" = "Incomplete")) %>% 
  mutate(habitat_prop_calc = if_else(is.na(habitat_prop_calc), 0, habitat_prop_calc)) %>% 
  mutate(name = reorder(name, size_km2))
  
test <- data3 %>% 
  group_by(name, habitat_type) %>% 
  summarize(total_prop = sum(habitat_prop_calc)) %>% 
  pivot_wider(names_from = "habitat_type", values_from = "total_prop")

## Save as df ----
plot_df <- data3


# Theme ------------------------------------------------------------------------

## Habitat colors ----
hab_area_colors <- c("wheat", "wheat1", "wheat2", "wheat3", # soft
                     "tan1", "tan2", "tan3", "tan4", # hard
                     "seagreen4", # kelp
                     "grey50") # incomplete

hard_soft_colors <- c("wheat", "wheat1", "wheat2", "wheat3", # soft
                      "tan1", "tan2", "tan3", "tan4") # hard

hab_linear_colors <- c("peachpuff2", # sandy
                       "steelblue3", # intertidal
                       "palegreen3", # coastal marsh
                       "burlywood4", # tidal flats,
                       "snow4") # armored

hab_colors <- c("wheat", "wheat1", "wheat2", "wheat3", # soft
                "tan1", "tan2", "tan3", "tan4", # hard
                "seagreen4", # kelp
                "peachpuff2", # sandy
                "steelblue3", # intertidal
                "palegreen3", # coastal marsh
                "burlywood4", # tidal flats,
                "snow4") # armored

hab_theme_wide <- theme(axis.text=element_text(size=6),
                        axis.title=element_text(size=8),
                        axis.title.x=element_blank(),
                        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=5),
                        axis.text.y = element_text(angle = 90, hjust = 0.5),
                        legend.text=element_text(size=6),
                        legend.title=element_text(size=7),
                        strip.text=element_text(size=6),
                        plot.title=element_blank(),
                        panel.spacing=unit(0.1,"lines"),
                        # Gridlines
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.background = element_blank(),
                        axis.line = element_line(colour = "black"),
                        # Legend
                        legend.key.size = unit(0.3, "cm"),
                        legend.background = element_rect(fill=alpha('blue', 0)),
                        legend.position = "bottom")

                       
# Plot ------------------------------------------------------------------------
## Hard/Soft Only ----
### 1. Prop from Given Size ----
# hs1 <- plot_df %>% 
#   filter(habitat_type == "area") %>% 
#   filter(!(habitat == "Kelp canopy")) %>% 
#   ggplot(aes(x = name, y = habitat_prop_given*100, fill = habitat)) +
#   # Facet
#   ggh4x::facet_nested(.~bioregion+mpa_habitat_type, space="free_x", scales="free_x") +
#   # Bar Plot
#   geom_col(color="grey30", lwd=0.1) +
#   geom_hline(yintercept = 100) +
#   scale_fill_manual(name = "Habitat Type", 
#                     values = hard_soft_colors) +
#   scale_y_continuous(limits = c(0, 107), expand = c(0,0)) +
#   labs(y="Percentage of MPA area", x="") +
#   theme_bw() +
#   hab_theme_wide
# hs1

### 2. Prop from Calc ----
hs2 <- plot_df  %>% 
  filter(habitat_type == "area") %>% 
  filter(!(habitat == "Kelp canopy")) %>% 
  ggplot(aes(x = name, y = habitat_prop_calc*100, fill = habitat)) +
  # Facet
  ggh4x::facet_nested(.~bioregion+mpa_habitat_type, space="free_x", scales="free_x") +
  # Bar Plot
  geom_col(color="grey30", lwd=0.1) +
  geom_hline(yintercept = 100) +
  # Legend colors
  scale_fill_manual(name = "Habitat Type", 
                    values = hard_soft_colors) +
  scale_y_continuous(limits = c(0, 105), expand = c(0,0)) +
  labs(y="Percentage of total habitat area", x="") +
  theme_bw() +
  hab_theme_wide
hs2

### 3. Actual Amount ----
hs3 <- plot_df %>% 
  filter(habitat_type == "area") %>% 
  filter(!(habitat == "Kelp canopy")) %>% 
  ggplot(aes(x = name, y = habitat_amount, fill = habitat)) +
  # Facet
  ggh4x::facet_nested(.~bioregion+mpa_habitat_type, space="free_x", scales="free_x") +
  # Bar Plot
  geom_col(color="grey30", lwd=0.1) +
  # Legend colors
  scale_fill_manual(name = "Habitat Type", 
                    values = hard_soft_colors) +
  scale_y_continuous(expand = c(0,0)) +
  labs(y="Habitat area (km2)", x="") +
  theme_bw() +
  hab_theme_wide
hs3

## Hard/Soft/Kelp---------------------------------------------------------------
### 1. Prop from Given Size ----
# hsk1 <- plot_df %>% 
#   filter(habitat_type == "area") %>% 
#   ggplot(aes(x = name, y = habitat_prop_given*100, fill = habitat)) +
#   # Facet
#   ggh4x::facet_nested(.~bioregion+mpa_habitat_type, space="free_x", scales="free_x") +
#   # Bar Plot
#   geom_col(color="grey30", lwd=0.1) +
#   geom_hline(yintercept = 100) +
#   # Legend colors
#   scale_fill_manual(name = "Habitat Type", 
#                     values = hab_area_colors) +
#   scale_y_continuous(limits = c(0, 130), expand = c(0,0)) +
#   labs(y="Percentage of MPA area", x="") +
#   theme_bw() +
#   hab_theme_wide
# hsk1

### 2. Prop from Calc ----
hsk2 <- plot_df %>% 
  filter(habitat_type == "area") %>% 
  #filter(!(mpa_habitat_type == "Estuary")) %>% 
  ggplot(aes(x = name, y = habitat_prop_calc, fill = habitat)) +
  # Facet
  ggh4x::facet_nested(.~bioregion+mpa_habitat_type, 
                      space="free_x", scales="free_x") +
  # Bar Plot
  geom_col(color="grey30", lwd=0.1, na.rm = F) +
  #geom_hline(yintercept = 100) +
  # Legend colors
  scale_fill_manual(name = "Habitat Type", 
                    values = hab_area_colors) +
  scale_y_continuous(limits = c(0, 1.1), expand = c(0,0)) +
  labs(y="Percentage of total habitat area", x="") +
  theme_bw() +
  hab_theme_wide
hsk2 

### 3. Actual Amount ----
hsk3 <- plot_df %>% 
  filter(habitat_type == "area") %>% 
  ggplot(aes(x = name, y = habitat_amount, fill = habitat)) +
  # Facet
  ggh4x::facet_nested(.~bioregion+mpa_habitat_type, space="free_x", scales="free_x") +
  # Bar Plot
  geom_col(color="grey30", lwd=0.1) +
  # Legend colors
  scale_fill_manual(name = "Habitat Type", 
                    values = hab_area_colors) +
  scale_y_continuous(expand = c(0,0)) +
  labs(y="Habitat area (km2)", x="") +
  theme_bw() +
  hab_theme_wide
hsk3

## Shoreline Only --------------------------------------------------------------

### 1. Prop from Size ----
# Insight is that this is not derived usefully.
# s1 <- plot_df %>% 
#   filter(habitat_type == "linear") %>% 
#   mutate(habitat_prop_given = if_else(is.na(habitat_prop_given), 0, habitat_prop_given)) %>% 
#   ggplot(aes(x = name, y = habitat_prop_given*100, fill = habitat)) +
#   # Facet
#   ggh4x::facet_nested(.~bioregion+mpa_habitat_type, space="free_x", scales="free_x") +
#   # Bar Plot
#   geom_col(color="grey30", lwd=0.1) +
#   geom_hline(yintercept = 100) +
#   # Legend colors
#   scale_fill_manual(name = "Habitat Type", 
#                     values = hab_linear_colors) +
#   scale_y_continuous(limits = c(0, 130), expand = c(0,0)) +
#   labs(y="Percentage of MPA area", x="") +
#   theme_bw() +
#   hab_theme_wide
# s1

### 2. Prop from Calc ----
s2 <- plot_df %>% 
  filter(habitat_type == "linear") %>% 
  ggplot(aes(x = name, y = habitat_prop_calc*100, fill = habitat)) +
  # Facet
  ggh4x::facet_nested(.~bioregion+mpa_habitat_type, space="free_x", scales="free_x") +
  # Bar Plot
  geom_col(color="grey30", lwd=0.1, na.rm = F) +
  geom_hline(yintercept = 100) +
  # Legend colors
  scale_fill_manual(name = "Habitat Type", 
                    values = hab_linear_colors) +
  scale_y_continuous(limits = c(0, 105), expand = c(0,0)) +
  labs(y="Percentage of total MPA shoreline habitat", x="") +
  theme_bw() +
  hab_theme_wide
s2

### 3. Actual Amount ----
s3 <- plot_df %>% 
  filter(habitat_type == "linear") %>% 
  ggplot(aes(x = name, y = habitat_amount, fill = habitat)) +
  # Facet
  ggh4x::facet_nested(.~bioregion+mpa_habitat_type, space="free_x", scales="free_x") +
  # Bar Plot
  geom_col(color="grey30", lwd=0.1) +
  # Legend colors
  scale_fill_manual(name = "Habitat Type", 
                    values = hab_linear_colors) +
  labs(y="Amount of Shoreline (km)", x="") +
  theme_bw() +
  hab_theme_wide
s3

## Export All ------------------------------------------------------------------

#gridExtra::grid.arrange(hsk2, s2, nrow = 2)

ggsave(hs1, filename=file.path(plot.dir, "hard_soft_size.png"),
       width=8, height=4, units="in", dpi=600)
ggsave(hs2, filename=file.path(plot.dir, "hard_soft_calc.png"),
       width=8, height=4, units="in", dpi=600)
ggsave(hs3, filename=file.path(plot.dir, "hard_soft_amount.png"),
       width=8, height=4, units="in", dpi=600)

ggsave(hsk1, filename=file.path(plot.dir, "hard_soft_kelp_size.png"),
       width=8, height=4, units="in", dpi=600)
ggsave(hsk2, filename=file.path(plot.dir, "hard_soft_kelp_calc.png"),
       width=8, height=4, units="in", dpi=600)
ggsave(hsk3, filename=file.path(plot.dir, "hard_soft_kelp_amount.png"),
       width=8, height=4, units="in", dpi=600)

ggsave(s2, filename=file.path(plot.dir, "shoreline_calc.png"),
       width=8, height=4, units="in", dpi=600)
ggsave(s3, filename=file.path(plot.dir, "shoreline_amount.png"),
       width=8, height=4, units="in", dpi=600)

# Generate Statewide/Regional Totals -------------------------------------------
# Try again
att_state <- data3 %>% 
  select(all_of(area_habitats), all_of(linear_habitats)) %>% 
  pivot_longer(everything(), 
               names_to = "habitat",
               values_to = "habitat_amount") %>% 
  group_by(habitat) %>% 
  summarize(att_total = sum(habitat_amount, na.rm = TRUE))



att_state <- data3 %>% 
  group_by(habitat) %>% 
  summarize(att_total = sum(habitat_amount, na.rm = T))

att_region <- data3 %>% 
  group_by(four_region, habitat, habitat_type) %>% 
  summarize(att_total = sum(habitat_amount, na.rm = T))

