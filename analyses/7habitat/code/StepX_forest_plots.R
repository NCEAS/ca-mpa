# Build forest plots with full models
# Cori Lopazanski
# lopazanski@bren.ucsb.edu
# Dec 2024

# Setup ------------------------------------------------------------------------
rm(list = ls())
gc()


# Begin ------------------------------------------------------------------------

# Set example species and path
species <- "OELO"
species <- "SCAR"
species <- "ELAT"
species <- "SMIN"
species <- "OPIC"
species <- "OYT"
species <- "SMEL"
species <- "SMYS"
species <- "SCAR"

path <- "analyses/7habitat/output/kelp/all_regions/interaction"
path <- "analyses/7habitat/output/kelp/all_regions/w_depth"


species <- "BLU"
species <- "CPR"
species <- "GPR"
species <- "VER"

path <- "analyses/7habitat/output/refine_pref_habitat/rock/all_regions/interaction"

# Read the data
data <- readRDS(file.path(path, paste0(species, "_results.rds")))

data2 <- data %>% 
  mutate(term_revised = str_remove_all(term, "MPA") %>% 
           str_remove_all("_annual_250|_annual_500|_annual_100|_annual_50|_annual_25") %>% 
           str_remove_all("_250|_500|_100|_25|_50") %>% 
           if_else(str_detect(., ":site_type"), str_replace(., "^(.*):site_type$", "site_type:\\1"), .) %>% 
           factor(levels = c("(Intercept)",
                             "site_type:depth_mean",
                             "site_type:depth_sd",
                             "site_type:soft_bottom",
                             "site_type:hard_bottom",
                             "site_type:kelp",
                             "site_type:age_at_survey",
                             "site_type",
                             "age_at_survey",
                             "depth_mean",
                             "depth_sd",
                             "soft_bottom",
                             "hard_bottom",
                             "kelp")))

data_sd <- data2 %>% filter(str_detect(model_id, "DM") | !key == "Full Model")

ggplot(data_sd %>% filter(!term == "(Intercept)"), 
       aes(x = estimate, y = term_revised, color = scale, pch = significance)) +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  geom_errorbarh(data = . %>% filter(is.na(importance) | importance > 0.5),
                 aes(xmin = conf_low, xmax = conf_high), linetype = "solid",
                 height = 0.2, position = position_dodge(width = 0.8)) +
  geom_errorbarh(data = . %>% filter(!is.na(importance) & importance < 0.5),
                 aes(xmin = conf_low, xmax = conf_high), linetype = "dashed",
                 height = 0.2, position = position_dodge(width = 0.8)) +
  geom_point(position = position_dodge(width = 0.8), size = 2) +  
  scale_shape_manual(values = c("***" = 16, "**" = 17, "*" = 15, "NA" = NA, "NS" = 3)) +
  scale_color_manual(
    values = c("#440154", "#3b528b", "#21908d", "#5dc863", "#D7C51B"), # Adjusted manually
    guide = guide_legend(order = 1)
  ) +
  facet_wrap(~key) +
  theme_minimal() +
  labs(x = "Estimate (Scaled)", 
       y = NULL, 
       color = "Scale", 
       pch = "Significance", 
       title = paste(species)) +
  theme(axis.text.y = element_text(size = 10))


# Make one combined species plot?

# List all files with '_results' in their name
result_files <- list.files(path, pattern = "_results\\.rds$", full.names = TRUE)

# Read and combine all the data
all_data <- result_files %>%
  map_dfr(readRDS)

species_plot <- all_data %>% 
  filter(!term == "(Intercept)") %>% 
  filter(!key == "Full Model") %>% 
  filter(is.na(model_id) | model_id == "Model Average") %>% 
  filter(is.na(importance) | importance > 0.5)

ggplot(species_plot, 
       aes(x = estimate, y = term_revised, color = species_code, pch = significance)) +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  geom_errorbarh(aes(xmin = conf_low, xmax = conf_high), linetype = "solid",
                 height = 0.2, position = position_dodge(width = 0.8)) +
  geom_point(position = position_dodge(width = 0.8), size = 2) +  
  scale_shape_manual(values = c("***" = 16, "**" = 17, "*" = 15, "NA" = NA, "NS" = 3)) +
 # scale_color_manual(
#    values = c("#440154", "#3b528b", "#21908d", "#5dc863", "#D7C51B"), # Adjusted manually
#    guide = guide_legend(order = 1)
#  ) +
  theme_minimal() +
  labs(x = "Estimate (Scaled)", 
       y = NULL, 
       color = "Scale", 
       pch = "Significance") +
  theme(axis.text.y = element_text(size = 10))
