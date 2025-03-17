# Add in cross-validation for the top models to see if each term and interaction is important
# Cori Lopazanski
# March 2025

library(tidymodels)
library(lme4)
library(multilevelmod)
library(rsample)
library(stringr)
library(multilevelmod)

set.seed(123) 

# Define the full model
focal_model <- top_models[[3]]
full_formula <- formula(focal_model)

# Define the dataset to use
data_sp <- data$data_sp

# Extract fixed effects terms from formula
fixed_terms <- data.frame(term = attr(terms(full_formula), "term.labels")) %>% filter(!str_detect(term, "affiliated_mpa|year")) %>% pull(term)

# Identify interaction terms
interaction_terms <- fixed_terms[str_detect(fixed_terms, ":") & !fixed_terms == "site_type:age_at_survey"] 

# Generate models by systematically removing each interaction
model_formulas <- list(full_model = full_formula)

for (interaction in interaction_terms) {
  new_terms <- setdiff(fixed_terms, interaction)  # Remove one interaction
  model_formulas[[paste0("drop_", str_replace_all(interaction, ":", "_"))]] <- 
    as.formula(paste("log_c_biomass ~", paste(new_terms, collapse = " + "), "+ (1 | affiliated_mpa) + (1 | year)"))
}


# Random kfold cv
cv_df <- crossv_mc(data_sp, 15) %>%
  mutate(train = map(train, as_tibble),
         test = map(test, as_tibble))

cv_results <- map_dfr(names(model_formulas), function(model_name) {
  model_formula <- model_formulas[[model_name]]
  
  model_cv <- cv_df %>%
    mutate(lmer_mod = map(train, ~ lmer(model_formula, data = .x, REML = TRUE,
                                        control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e8))))) %>%
    mutate(rmse   = map2_dbl(lmer_mod, test, \(mod, df) rmse(model = mod, data = df)),
           mae    = map2_dbl(lmer_mod, test, \(mod, df) mae(model = mod, data = df)),
           loglik = map(lmer_mod, \(mod) as.numeric(logLik(mod)))) %>%
    mutate(model = model_name) 
  
  return(model_cv)
}) 


# Calculate the overall mean for each metric
mean_values <- cv_results %>% 
  filter(model == "full_model") %>% 
  select(rmse, loglik, mae) %>% 
  unnest(cols = c(loglik, rmse, mae)) %>% 
  pivot_longer(cols = c(rmse, loglik, mae), names_to = "metric", values_to = "value") %>%
  group_by(metric) %>% 
  summarise(mean_value = mean(value, na.rm = TRUE))

# Create the plot
cv_results %>% 
  select(rmse, loglik, mae, model) %>% 
  unnest(cols = c(loglik, rmse, mae)) %>% 
  mutate(model = fct_inorder(model)) %>% 
  pivot_longer(cols = c(rmse, loglik, mae), names_to = "metric", values_to = "value") %>% 
  ggplot(aes(x = model, y = value)) + 
  geom_violin() +
  geom_hline(data = mean_values, aes(yintercept = mean_value), color = "red", linetype = "dashed") + 
  stat_summary(fun = "mean", geom = "point", size = 2) +  
  coord_flip() + 
  labs(title = names(focal_model),
       x = NULL, y = "Metric value") + 
  theme_minimal() +
  facet_wrap(~metric, scales = "free_x") 



# Group kfold cv
# Seems like another option is to define the partitions based on groups, like MPAs
# This is a bit unstable and because not all MPAs are in a set, there's an error
# in calculating the rmse "new levels" - so can do it manually like this but not sure
# if that's the right move here:

# cv_df <- group_vfold_cv(data_sp, v = 15, group = site) %>%
#   mutate(train = map(splits, analysis),
#          test  = map(splits, assessment))
# 
# 
# evaluate_model <- function(model, test_data) {
#   # Make predictions on the test set
#   predictions <- predict(model, newdata = test_data, allow.new.levels = TRUE)
#   
#   rmse_value <- sqrt(mean((test_data$log_c_biomass - predictions)^2))
#   mae_value <- mean(abs(test_data$log_c_biomass- predictions))
#   loglik_value <- as.numeric(logLik(model))
#   
#   # Return metrics as a tibble
#   tibble(rmse = rmse_value, mae = mae_value, loglik = loglik_value)
# }
# 
# cv_results <- map_dfr(names(model_formulas), function(model_name) {
#   model_formula <- model_formulas[[model_name]]
#   
#   model_cv <- cv_df %>%
#     mutate(lmer_mod = map(train, ~ lmer(model_formula, data = .x, REML = TRUE,
#                                         control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e8))))) %>%
#     mutate( rmse = map2_dbl(lmer_mod, test, ~ evaluate_model(.x, .y)$rmse),
#             mae  = map2_dbl(lmer_mod, test, ~ evaluate_model(.x, .y)$mae),
#             loglik = map_dbl(lmer_mod, ~ as.numeric(logLik(.x)))) %>%
#     mutate(model = model_name) 
#   
#   return(model_cv)
# })
# 
# 
# cv_results %>% 
#   select(rmse, loglik, mae, model) %>% 
#   unnest(cols = c(loglik, rmse, mae)) %>% 
#   mutate(model = fct_inorder(model)) %>% 
#   pivot_longer(cols = c(rmse, loglik, mae), names_to = "metric", values_to = "value") %>% 
#   ggplot(aes(x = model, y = value)) + 
#   geom_violin() +
#   stat_summary(fun = "median", geom = "point", size = 2) +  
#   coord_flip() + 
#   theme_minimal() +
#   labs(x = NULL, y = "Metric value") + 
#   facet_wrap(~metric, scales = "free_x")  
# 
#   ]

