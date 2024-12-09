
# Setup --------------------------------------------------------------------------
rm(list=ls())

# Load required packages
library(tidyverse)

# Set directories 
dat_path <- here::here("analyses","1performance_eco","output")
tabdir <- here::here("analyses","1performance_eco","tables")
figdir <- here::here("analyses","1performance_eco","figures")


# Load data
biomass_mod <- readRDS(file.path(dat_path, "biomass_with_moderators.Rds")) 

################################################################################
#get all unique MPAs and types

mpa_effort <- biomass_mod %>% select(habitat, state_region, year, affiliated_mpa) %>% unique() %>%
              rename(Ecosystem = habitat)
             
# Summarize the years into a single string for each affiliated_mpa and Ecosystem combination
mpa_type_summary <- mpa_effort %>%
  group_by(affiliated_mpa, Ecosystem) %>%
  summarise(years = toString(unique(year)), .groups = 'drop')

# Pivot the data wider
mpa_type_wide <- mpa_type_summary %>%
  pivot_wider(names_from = Ecosystem, values_from = years)


write.csv(mpa_type_wide, file.path(tabdir, "AppendixS8_survey_effort.csv"),row.names = FALSE)


################################################################################
#plot

# Theme
base_theme <- theme(axis.text=element_text(size=6, color = "black"),
                    axis.title=element_blank(),
                    strip.text=element_text(size=6.5, color = "black", face = "bold"),
                    strip.background = element_blank(),
                    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,color = "black",),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.background = element_rect(fill=alpha('blue', 0)))


g <- ggplot(mpa_effort %>%
              mutate(state_region = recode(state_region,
                                           "North Coast" = "North \nCoast",
                                           "North Central Coast" = "North Central \nCoast",
                                           "Central Coast" = "Central \nCoast",
                                           "South Coast" = "South \nCoast"),
                     Ecosystem = factor(Ecosystem, levels = c("Surf zone", "Kelp forest", "Shallow reef", "Deep reef"))),
            aes(x = year, y = affiliated_mpa)) +
  facet_grid(state_region ~ Ecosystem, scales = "free_y", space = "free_y") +
  geom_tile(fill = "SteelBlue", width = 0.9, height = 0.9) +
  # Labels
  labs(x = "", y = "") +
  scale_x_continuous(breaks = seq(2000, 2020, 5)) +
  # Theme
  theme_bw() + base_theme
g

# Export
ggsave(g, filename=file.path(figdir, "AppendixS15_data_availability.png"), 
       width=6.5, height=5.5, units="in", dpi=600)




