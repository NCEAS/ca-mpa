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

path <- "analyses/7habitat/output/refine_pref_habitat/kelp/all_regions/interaction"

species <- "BLU"
species <- "CPR"
species <- "GPR"
species <- "VER"

path <- "analyses/7habitat/output/refine_pref_habitat/rock/all_regions/interaction"

# Read the data
data <- readRDS(file.path(path, paste0(species, "_results.rds")))


ggplot(data %>% filter(!term == "(Intercept)"), 
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
