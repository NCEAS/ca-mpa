

library(tidyverse)
library(gt)
library(broom)

kelp <- readRDS("/Users/lopazanski/Desktop/ltm/update_2024/combine_tables/kelp_full.Rds")


rock_list <- list.files(path = "/Users/lopazanski/Desktop/output/rock/all_regions/log_c_scaled") %>%
  str_remove_all(., "_models.rds|_results.rds") %>% unique()




species <- "BLU"
path <- "/Users/lopazanski/Desktop/output/rock/all_regions/log_c_scaled"
habitat <- "rock"

data <- readRDS(file.path(path, paste0(species, "_results.rds"))) %>% 
  mutate(scale = factor(case_when(is.na(scale) & model_id == "ST*A" ~ NA,
                         is.na(scale) ~ as.factor(str_extract(term, "\\d+")),
                         T~scale),
                        levels = c(25, 50, 100, 250, 500, NA))) %>% 
  mutate(importance_type = case_when((is.na(importance) | importance > 0.5) ~ "Greater than 0.5",
                                     (!is.na(importance) & importance < 0.5) ~ "Less than 0.5")) %>% 
  left_join(pred_kelp_int) %>%  
  filter(!(key == "Full Model" & is.na(type) & is.na(type3))) %>% # not actually full
  mutate(term_revised = str_remove_all(term, "MPA") %>% 
           str_remove_all("_annual_250|_annual_500|_annual_100|_annual_50|_annual_25") %>% 
           str_remove_all("_250|_500|_100|_25|_50") %>% 
           if_else(str_detect(., ":age_at_survey$"), str_replace(., "^(.*):age_at_survey$", "age_at_survey:\\1"), .) %>% 
           if_else(str_detect(., ":site_type$"), str_replace(., "^(.*):site_type$", "site_type:\\1"), .) %>% 
           if_else(str_detect(., ":site_type:"), str_replace(., "^(.*?):(site_type):(.*?)$", "\\2:\\3:\\1"), .)  %>% 
           factor(levels = c("(Intercept)",
                             "site_type:age_at_survey:depth_mean",
                             "site_type:age_at_survey:depth_sd",
                             "site_type:age_at_survey:kelp",
                             "site_type:age_at_survey:hard_bottom",
                             "age_at_survey:depth_mean",
                             "age_at_survey:depth_sd",
                             "age_at_survey:hard_bottom",
                             "age_at_survey:kelp",
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


data_sd <- data %>%
  filter(!is.na(type3) | !key == "Full Model") %>%
  filter(str_detect(model_id, "DSD") | !key == "Full Model") %>%
  filter(!term == "(Intercept)") %>%
  filter(!key == "Top Model")

data_mn <- data %>% 
  filter(!is.na(type3) | !key == "Full Model") %>%
  filter(str_detect(model_id, "DM") | !key == "Full Model") %>%
  filter(!term == "(Intercept)")

ggplot(data_sd,
       aes(x = estimate, y = term_revised, color = scale, pch = significance)) +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  geom_errorbarh(aes(xmin = conf_low, xmax = conf_high, linetype = importance_type, color = scale),
                 height = 0.4, position = position_dodge(width = 0.8))+
  geom_point(position = position_dodge(width = 0.8), size = 2) +  
  scale_shape_manual(values = c("***" = 16, "**" = 17, "*" = 15, "NA" = NA, "NS" = 3)) +
  scale_color_manual(
    values = c("#440154", "#3b528b", "#21908d", "#5dc863", "#D7C51B"), 
    guide = guide_legend(order = 1)
  ) +
  facet_wrap(~key) +
  theme_minimal() +
  labs(x = "Estimate (Scaled)", 
       y = NULL, 
       color = "Scale", 
       pch = "Significance", 
       linetype = "Importance",
       title = paste(species)) +
  my_theme


data_sd %>% 
  filter((key == "Top Model v. Base Model" & is.na(model_id)) | model_id == "Model Average") %>% 
  dplyr::select(species_code, term_revised, scale, estimate, p_value, importance) %>% 
  mutate(p_value = case_when(p_value < 0.001 ~ "< 0.001", T~as.character(round(p_value, 3)))) %>% 
  gt() %>% 
  fmt_scientific(columns = c(estimate), decimals = 2, exp_style = "e")







