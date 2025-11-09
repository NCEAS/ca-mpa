# Create supplemental data availability figure
# Cori Lopazanski
# November 2025

library(tidyverse)
library(forcats)
library(ggh4x) 

rm(list = ls())
gc()

source("analyses/7habitat/code/Step0_helper_functions.R")  # Load the function from the file
fig.dir <- "~/ca-mpa/analyses/7habitat/figures/3way-figures"


my_theme <- theme_minimal(base_family = "Arial") + 
  theme(plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 8),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.margin = margin(t = 0, unit='cm'),
        plot.caption = element_text(size = 8),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 8, face = "bold"),
        legend.background = element_rect(fill = "white", color = NA),  
        panel.background = element_rect(fill = "white", color = NA),  
        plot.background = element_rect(fill = "white", color = NA))


habitats <- c(rock = "rock_filtered",
            kelp = "kelp_filtered",
            surf = "surf_filtered")

re_strings <- c(rock = "rmy",
                kelp = "my",
                surf = "m")

focal_group <- "targeted"
data_dir <- "~/ca-mpa/analyses/7habitat/output/data/3way"

data_list <- mapply(function(hab, re) {
  fname <- paste(hab, focal_group, re, "data.rds", sep = "_")
  readRDS(file.path(data_dir, fname))
}, hab = habitats, re = re_strings, SIMPLIFY = FALSE)

list2env(data_list, envir = .GlobalEnv)

test <- kelp %>%
  distinct(year, region4, affiliated_mpa) %>%
  mutate(affiliated_mpa = str_to_title(affiliated_mpa) %>% 
           str_replace_all(., "Smr", "SMR") %>% 
           str_replace_all(., "Smca", "SMCA")) %>% 
  group_by(region4, affiliated_mpa) %>%
  summarise(years = list(sort(unique(year))),
            n_years = length(unique(year)), .groups = "drop") %>%
  mutate(region4 = factor(region4, levels = c("North", "Central", "N. Channel Islands", "South"))) %>% 
  mutate(affiliated_mpa = fct_reorder(affiliated_mpa, n_years)) %>%
  arrange(region4, n_years) %>%
  unnest(years, keep_empty = TRUE) %>% 
  mutate(visits = if_else(n_years > 0, 1, 0))


ggplot(test, aes(x = years, y = affiliated_mpa, fill = visits)) +
  geom_tile(show.legend = F) +
  ggh4x::facet_nested(region4~., scales = "free", space = "free") +
  labs(x = "Year", y = "MPA") +
  theme_minimal()


# prepare and plot heatmap for all habitats with region-grouped y-axis and region-specific vertical lines
library(dplyr)
library(tidyr)
library(forcats)
library(stringr)
library(ggplot2)
library(ggh4x)

habitats <- c(rock = "rock_filtered",
              kelp = "kelp_filtered",
              surf = "surf_filtered")

re_strings <- c(rock = "rmy",
                kelp = "my",
                surf = "m")

focal_group <- "targeted"
data_dir <- "~/ca-mpa/analyses/7habitat/output/data/3way"

# read into named list and bind into one tibble with habitat label
data_list <- mapply(function(hab, re, lab) {
  fname <- paste(hab, focal_group, re, "data.rds", sep = "_")
  df <- readRDS(file.path(data_dir, fname))
  df$habitat <- lab
  df
}, hab = habitats, re = re_strings, lab = names(habitats), SIMPLIFY = FALSE)


# get canonical year levels from kelp (as characters, sorted)
kelp_levels <- sort(unique(as.character(data_list$kelp$year)))

# enforce those factor levels across all datasets in data_list, then bind
data_list_fixed <- map(data_list, ~ .x %>%
                         mutate(year = factor(as.character(year), levels = kelp_levels)))

all_dat <- bind_rows(data_list_fixed, .id = "habitat")


# effect years by region
effect_years <- tibble(
  region4 = c("North", "South", "N. Channel Islands", "Central"),
  effect_year = c(2012, 2012, 2003, 2007)
)

# prepare summary keeping years as list, count, then unnest; reorder affiliated_mpa within each region
plot_df <- all_dat %>%
  distinct(habitat, region4, affiliated_mpa, year) %>%
  mutate(affiliated_mpa = str_to_title(affiliated_mpa) %>%
           str_replace_all("Smr", "SMR") %>%
           str_replace_all("Smca", "SMCA")) %>%
  group_by(habitat, region4, affiliated_mpa) %>%
  summarise(years = list(sort(unique(year))),
            n_years = length(unique(year)), .groups = "drop") %>% 
  mutate(affiliated_mpa = fct_reorder(affiliated_mpa, n_years)) %>%
  unnest(years) %>%
  mutate(visits = if_else(n_years > 0, 1L, 0L)) %>%
  mutate(region4 = factor(region4, levels = c("North", "Central", "N. Channel Islands", "South")),
         habitat = factor(habitat, levels = names(habitats))) %>% 
  mutate(habitat = recode(habitat,
                          "kelp" = "Kelp forest",
                          "rock" = "Shallow reef",
                          "surf" = "Surf zone"))

# expand effect_years to include habitat so geom_vline applies per panel
vlines <- tidyr::crossing(effect_years, habitat = levels(plot_df$habitat)) %>% 
  mutate(effect_year = factor(effect_year, levels = kelp_levels)) %>% 
  mutate(region4 = factor(region4, levels = levels(plot_df$region4))) %>% 
  mutate(habitat = factor(habitat, levels = levels(plot_df$habitat)))

# plot
ggplot(plot_df, aes(x = years, y = affiliated_mpa, fill = visits)) +
  geom_tile(width = 1, height = 0.9, show.legend = FALSE) +
  geom_vline(data = vlines, aes(xintercept = effect_year), linetype = "dashed") +
  ggh4x::facet_nested(region4 ~ habitat, scales = "free_y", space = "free_y") +
  labs(x = "Year", y = "MPA") +
  my_theme + 
  theme(axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8, angle = 50, hjust = 1),
        strip.text.y = element_text(angle = -90),
        strip.text.x = element_text(size = 10))

ggsave(file.path(fig.dir, "si-figX-availability.png"),
       width = 10.5, height = 7, units = "in", dpi = 600)
