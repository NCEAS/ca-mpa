# Make AICc, R2, and Nested Tables
# Cori Lopazanski
# Feb 2025


# Setup ------------------------------------------------------------------------

library(gt)
library(performance)
library(tidyverse)

rm(list = ls())
gc()

source("analyses/7habitat/code/helper_functions.R")  

fig.dir <- "~/ca-mpa/analyses/7habitat/figures"


# Load Results & Tables --------------------------------------------------------

get_results <- function(habitat, re_string){
  
  results_file <- paste(habitat, re_string, "selection_results.rds", sep = "_")
  results <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/results", results_file)) 
  
  return(results)
}


rock <- get_results("rock", "rmsy")
kelp <- get_results("kelp", "rmsy")
surf <- get_results("surf", "rm")

# Build Table 1 ----------------------------------------------------------------

# Combined version that has AICc and R2 for the models from each ecosystem
rock_aicc <- rock$aicc_table %>% mutate(Model = str_remove_all(Model, "\\+ST\\*A"))
kelp_aicc <- kelp$aicc_table %>% mutate(Model = str_remove_all(Model, "\\+ST\\*A"))
surf_aicc <- surf$aicc_table


# Combine the different AICc tables into one dataframe
combined_aicc <- bind_rows(`Shallow reef` = rock_aicc,
                           `Kelp forest` = kelp_aicc,
                           `Surf zone` = surf_aicc, .id = "Ecosystem") %>% 
  mutate(Model = str_replace(Model, "AV", "MB") %>% 
           str_replace_all("ST", "P") %>% 
           str_replace_all("\\+", " + ")) %>% 
  mutate(Type = case_when(top == TRUE ~ "top",
                          Model == "P*A" ~ "base",
                          TRUE ~ NA))

# Get models for R2 comparison
model_lists <- list(`Shallow reef` = rock$models, `Kelp forest` = kelp$models, `Surf zone` = surf$models)

model_long <- map2_dfr(
  model_lists,
  names(model_lists),
  ~ tibble(
    Ecosystem = .y,
    Type = c("top", "base"),
    Marginal_R2 = map_dbl(c(.x$top, .x$base), ~ r2_nakagawa(.x)$R2_marginal),
    Conditional_R2 = map_dbl(c(.x$top, .x$base), ~ r2_nakagawa(.x)$R2_conditional)
  )
)

model_long

# Build Combined AICc and R2 Table (EcoApps) ----

# tag combined_aicc rows that correspond to the 'top' and 'base' models, join the R2 values, and drop the helper
aicc_with_r2 <- combined_aicc %>%
  left_join(model_long, by = c("Ecosystem", "Type")) %>%
  select(-Type) %>%
  gt(groupname_col = "Ecosystem") %>%
  cols_label(delta = "ΔAICc",
             weight = "AICc weight",
             Marginal_R2 = html("Marginal R&sup2;"),
             Conditional_R2 = html("Conditional R&sup2;")) %>%
  fmt_number(columns = c(delta, weight, Marginal_R2, Conditional_R2), decimals = 3) %>% 
  sub_missing(columns = everything(), missing_text = "") %>% 
  tab_options(table.font.names = "Times New Roman",
              table.font.size = px(12),
              data_row.padding = px(6),
              row_group.padding = px(6),
              heading.align = "left",
              table.width = pct(100)) %>% 
  tab_style(style = cell_text(font = "Times New Roman", size = px(12)),
             locations = cells_body(columns = everything())) %>% 
  tab_style(style = cell_text(font = "Times New Roman", size = px(12), weight = "bold"),
            locations = cells_column_labels(columns = everything())) %>% 
  tab_style(style = cell_text(font = "Times New Roman", size = px(12), weight = "bold", decorate = "underline"),
            locations = cells_row_groups()) %>% 
  tab_style(style = cell_text(font = "Times New Roman", size = px(12), weight = "bold"),
            locations = cells_body(rows = top == TRUE)) %>%   
  cols_hide("top") %>% 
  tab_source_note(source_note = paste0(
    "Notes: Models shown are within ΔAICc ≤ 2 of the top-ranked model or are the base model without any habitat variables (P*A). The final competitive model for each ecosystem after applying the nesting rule is bolded. ",
    "Abbreviations: H = hard bottom; S = soft bottom; P = protected status; DM = depth mean; DCV = depth coefficient of variation; SSD = standard deviation in slope; K = annual kelp canopy cover; MB = maximum biotic extent; A = MPA age.")) 

aicc_with_r2



# Build Nested Table ------------------------------------------------------------


r2_difference <- model_long %>%
 dplyr::select(-c(Conditional_R2)) %>% 
  pivot_wider(names_from = Type, values_from = Marginal_R2) %>%
  mutate(Marginal_R2_diff = (top - base)*100) %>%
  select(Ecosystem, Marginal_R2_diff)
r2_difference


# Function to extract variance components
extract_re_variance <- function(model, ecosystem, model_type) {
  vc <- as.data.frame(VarCorr(model))
  total_var <- sum(vc$vcov)
  
  vc %>%
    mutate(
      Ecosystem = ecosystem,
      Model = model_type,
      Group = grp,
      Variance = vcov,
      Percent = 100 * vcov / total_var
    ) %>%
    select(Ecosystem, Model, Group, Variance, Percent)
}


# Extract variance contributions for top and base models
re_variance <- map2_dfr(model_lists, names(model_lists), function(models, eco) {
  bind_rows(
    extract_re_variance(models$top,  ecosystem = eco, model_type = "top"),
    extract_re_variance(models$base, ecosystem = eco, model_type = "base")
  )
})

re_variance %>% 
  gt(groupname_col = "Ecosystem") %>% 
  fmt_number(columns = c("Variance", "Percent"), decimals = 3)


# Get percent comparisons
# Extracting the interaction effect between protected status and MPA age from a designated “base model” for each ecosystem,
# then converting that coefficient from log scale to an interpretable percent change per year.

# Pulls the first instance of the target term from each results table.
#Computes a Wald 95% CI on the log scale.
# Exponentiates the coefficient and CI bounds, subtracts 1, and scales by 100 to express the effect as percent change per year (implicitly treating the model as log-link / multiplicative).
# Returns a tidy summary per ecosystem.

# Standardizing and reporting the annual percent effect of protection (via the interaction term) across ecosystems, based on the base model.

get_base_beta <- function(results, ecosystem_name){
  row <- results %>%
    filter(key == "Base Model", term_revised == "Protected Status x MPA Age") %>%
    slice(1)
  
  beta <- row$estimate
  se <- row$std_error
  ci_low <- beta - 1.96 * se
  ci_high <- beta + 1.96 * se
  
  pct <- (exp(beta) - 1) * 100
  pct_low <- (exp(ci_low) - 1) * 100
  pct_high <- (exp(ci_high) - 1) * 100
  
  tibble(
    ecosystem = ecosystem_name,
    beta = beta,
    se = se,
    pct_per_year = round(pct, 2),
    pct_low = round(pct_low, 2),
    pct_high = round(pct_high, 2)
  )
}

ecosystem_list <- list(rock$all_results, kelp$all_results, surf$all_results)
ecosystem_names <- c("shallow_reef", "kelp_forest", "surf_zone")

pct_base <- map2_dfr(ecosystem_list, ecosystem_names, get_base_beta)

# formatted table with CI column
pct_base_table <- pct_base %>%
  mutate(pct_CI = sprintf("%0.2f%% (%0.2f to %0.2f%%)", pct_per_year, pct_low, pct_high)) %>%
  select(ecosystem, beta, se, pct_CI) %>% 
  gt() %>% 
  fmt_number(decimals = 2)

pct_base_table
