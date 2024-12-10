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


path <- "analyses/7habitat/output/refine_pref_habitat/kelp/all_regions/interaction"

# Read the data
data <- readRDS(file.path(path, paste0(species, "_subset.rds")))

# Preview the data structure  
#map(data, ~ names(.))

# Define helper functions
clean_terms <- function(df) {df %>%
    mutate(term_revised = str_remove_all(term, "MPA"),
           term_revised = str_remove_all(term_revised, "m_250|m_500|m_100|m_25|m_50"),
           term_revised = str_remove_all(term_revised, "_annual_250|_annual_500|_annual_100|_annual_50|_annual_25"),
           term_revised = if_else(str_detect(term_revised, ":site_type"),
                                  str_replace(term_revised, "^(.*):site_type$", "site_type:\\1"),
             term_revised),
           term_revised = factor(term_revised, levels = c(
             "(Intercept)",
             "site_type:soft_bottom_30_100",
             "site_type:hard_bottom_30_100",
             "site_type:soft_bottom_0_30",
             "site_type:hard_bottom_0_30",  
             "site_type:kelp",
             "site_type:age_at_survey",
             "site_type",
             "age_at_survey",
             "soft_bottom_30_100",
             "hard_bottom_30_100",
             "soft_bottom_0_30",
             "hard_bottom_0_30",
             "kelp")))}

add_significance <- function(df) {df %>%
    mutate(significance = factor(case_when(p.value < 0.001 ~ "***",
                                    p.value < 0.01 ~ "**",
                                    p.value < 0.05 ~ "*",
                                    is.na(p.value) ~ "NA",
                                    TRUE ~ "NS"), levels = c("***", "**", "*", "NS", "NA")))
           }

# Process top models
if (length(data$top_models) > 1) {
  model_avg <- model.avg(data$top_models, fit = TRUE)
  coef_table <- coefTable(model_avg) %>%
    as.data.frame() %>%
    rownames_to_column("term") %>%
    mutate(
      term = str_replace(term, "typeMPA", "type"),
      conf.low = Estimate - 1.96 * `Std. Error`, # 95% CI lower
      conf.high = Estimate + 1.96 * `Std. Error`, # 95% CI upper
      importance = sw(model_avg)[term],
      p.value = NA
    ) %>% 
    rename(estimate = Estimate)
} else {
  coef_table <- tidy(data$top_models[[1]], conf.int = TRUE, effect = "fixed") %>%
    mutate(
      term = str_replace(term, "typeMPA", "type"),
      importance = 1
    )
}


# Create summary_df
summary_df <- coef_table %>%
  clean_terms() %>%
  add_significance() %>%
  mutate(
    species_code = species,
    sign = sign(estimate),
    num_models = length(data$top_models),
    scale = unique(case_when(
      str_detect(names(data$top_models), "m_500") ~ 500,
      str_detect(names(data$top_models), "m_250") ~ 250,
      str_detect(names(data$top_models), "m_100") ~ 100,
      str_detect(names(data$top_models), "m_50") ~ 50,
      str_detect(names(data$top_models), "m_25") ~ 25)),
    key = if_else(length(data$top_models) == 1, "Top Model v. Base Model", "Top Models (Average) v. Base Model")) %>%
  filter(!term %in% c("(Intercept)"))

# Process core models
model_results <- lapply(names(data$core_models), function(model_name) {
  tidy(data$core_models[[model_name]], conf.int = TRUE, effect = "fixed") %>%
    clean_terms() %>%
    add_significance() %>%
    mutate(model = model_name)
})

# Combine core model results
model_data <- do.call(rbind, model_results) %>% 
  mutate(scale = case_when(
      str_detect(model, "kelp_annual_25 ") ~ 25,
      str_detect(model, "kelp_annual_50 ") ~ 50,
      str_detect(model, "kelp_annual_100") ~ 100,
      str_detect(model, "kelp_annual_250") ~ 250,
      str_detect(model, "kelp_annual_500") ~ 500),
    key = "Full Model") 

# Combine all results
combined_data <- bind_rows(model_data, summary_df) %>%
  mutate(scale = factor(scale, levels = c(25, 50, 100, 250, 500))) %>% 
  mutate(key = case_when(model == "site_type * age_at_survey" & !length(data$top_models) == 1 ~ "Top Models (Average) v. Base Model",
                         model == "site_type * age_at_survey" & length(data$top_models) == 1 ~ "Top Model v. Base Model",
                         T~key))

ggplot(combined_data %>% filter(!term == "(Intercept)"), 
       aes(x = estimate, y = term_revised, color = scale, pch = significance)) +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  geom_errorbarh(data = . %>% filter(is.na(importance) | importance > 0.5),
                 aes(xmin = conf.low, xmax = conf.high), linetype = "solid",
                 height = 0.2, position = position_dodge(width = 0.8)) +
  geom_errorbarh(data = . %>% filter(!is.na(importance) & importance < 0.5),
                 aes(xmin = conf.low, xmax = conf.high), linetype = "dashed",
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
       title = paste(unique(summary_df$species_code))) +
  theme(axis.text.y = element_text(size = 10))



library(performance)

data$core_models$`hard_bottom_0_30m_25 * site_type, kelp_annual_25 * site_type, soft_bottom_0_30m_25 * site_type, site_type * age_at_survey`

