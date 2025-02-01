# Cori Loapzanski
# August 2024

# About ------------------------------------------------------------------------

# Explore biomass (kg_per_m2) of fish species using mixed-effects models 
# Evaluate the influence of habitat characteristics on biomass
# Compare multiple models to identify the best-fitting model

# Setup ------------------------------------------------------------------------
library(lme4)  # For fitting mixed-effects models
library(MuMIn)  # For calculating R² and AICc
library(car)  # For VIF
library(performance)  # For model diagnostics
library(patchwork)
library(lmerTest)

# Read Data --------------------------------------------------------------------
# Proceed here directly from Step2_combine_tables.R
rm(list = setdiff(ls(), "data"))
gc()


# Subset data to species of interest
data_subset <- data %>%
  mutate(pref_habitat = rowSums(across(c(hard_bottom_biotic_0_30m_100, soft_bottom_biotic_0_30m_100)), na.rm = TRUE),
         pref_habitat_scaled = scale(pref_habitat),
         age_scaled = scale(age_at_survey),
         age_group = cut(age_at_survey, 
                         breaks = c(0, 5, 10, 15, 20, Inf), 
                         labels = c("0-5 years", "6-10 years", "11-15 years", "16-20 years", ">20 years"))) %>% 
  filter(mpa_defacto_class == "smr") %>%          # Filter for MPA class "smr"
  filter(species_code == "OYT") %>%               # Filter for species "OYT"
  filter(age_at_survey >= 0)                      # Keep only non-negative age_at_survey

test <- data_subset %>% 
  group_by(affiliated_mpa, region, site, site_type, pref_habitat) %>% 
  summarize(total = sum(kg_per_m2))

ggplot(data = test) +
  geom_point(aes(x = pref_habitat, y = total, color = site_type)) +
  geom_smooth(aes(x = pref_habitat, y = total, color = site_type)) 

# Create a filtered subset where age_at_survey > 5 for comparison
data_subset2 <- data_subset %>% 
  filter(age_at_survey > 8)

m1 <- lmer(log(kg_per_m2 +1)~ site_type * age_group + (1 | region) + (1|year),
           data = data_subset, REML = FALSE)

m2 <- lmer(log(kg_per_m2 +1) ~ site_type * age_group + pref_habitat + (1 | region) + (1|year),
           data = data_subset, REML = FALSE)

m3 <- lmer(log(kg_per_m2 +1) ~ site_type * age_group + site_type * pref_habitat + (1 | region)+  (1|year),
           data = data_subset, REML = FALSE)

m4 <- lmer(log(kg_per_m2 +1) ~ pref_habitat + (1 | region)+  (1|year),
           data = data_subset, REML = FALSE)


# Compare models using AICc
model_comparison <- model.sel(m1, m2, m3)
print(model_comparison)

summary(m1)
summary(m2)
summary(m3)
summary(m4)

# Extract fixed effects for the effect of protection (site_type) from each model
coef_m1 <- tidy(m1) %>% filter(term == "site_typeReference:age_at_survey")
coef_m2 <- tidy(m2) %>% filter(term == "site_typeReference:age_at_survey")

# Get confidence intervals for site_typeMPA from each model
conf_m1 <- confint(m1, parm = "site_typeReference:age_at_survey", level = 0.95)
conf_m2 <- confint(m2, parm = "site_typeReference:age_at_survey", level = 0.95)

# Combine the coefficients and confidence intervals into a single data frame
effects_df <- rbind(
  data.frame(model = "Protection Only", estimate = coef_m1$estimate, conf.low = conf_m1[1], conf.high = conf_m1[2]),
  data.frame(model = "Protection with Habitat Control", estimate = coef_m2$estimate, conf.low = conf_m2[1], conf.high = conf_m2[2])
)

ggplot(effects_df, aes(x = model, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  labs(title = "Effect Sizes for Protection (MPA vs. Reference)",
       x = "Model",
       y = "Effect Size (Estimate ± 95% CI)") +
  theme_minimal()

a <- ggplot(data = data_subset) +
  geom_point(aes(x = pref_habitat, y = kg_per_m2, color = site_type)) +
  geom_smooth(aes(x = pref_habitat, y = kg_per_m2, color = site_type), method = "gam") +
  labs(x = "Area of biotic habitat in depths 0-30m within 250m of site (m2)",
       y = "Biomass (kg per m2)",
       color = NULL,
       title = "Olive and yellowtail rockfish \nTargeted \nHard Bottom Biotic") +
  scale_color_manual(values = c("#ff7eb6", "#d4bbff")) +
  theme_minimal() +
  theme(axis.title = element_text(size = 12))
a

b <- ggplot(data = data_subset) +
  geom_point(aes(x = year, y = kg_per_m2, color = site_type), show.legend = F) +
  geom_smooth(aes(x = year, y = kg_per_m2, color = site_type), method = "glm", show.legend = F) +
  theme_minimal() +
  scale_color_manual(values = c("#ff7eb6", "#d4bbff")) +
  labs(x = "Year",  y = "Biomass (kg per m2)", color = NULL) +
  theme(axis.title = element_text(size = 12))

c <- ggplot(data = data_subset) +
  geom_point(aes(x = age_at_survey, y = kg_per_m2, color = site_type), show.legend = F) +
  geom_smooth(aes(x = age_at_survey, y = kg_per_m2, color = site_type), method = "glm", show.legend = F) +
  theme_minimal() +
  labs(x = "Years since implementation",  y = "Biomass (kg per m2)", color = NULL) +
  scale_color_manual(values = c("#ff7eb6", "#d4bbff")) +
  theme(axis.title = element_text(size = 12))


layout <- (a + ggtitle("A")) / ((b + ggtitle("B")) | (c + ggtitle("C")))
layout


ggplot(data = data_subset) +
  geom_point(aes(x = pref_habitat, y = kg_per_m2, color = site_type)) +
  geom_smooth(aes(x = pref_habitat, y = kg_per_m2, color = site_type), method = "gam") +
  labs(x = "Area of biotic habitat in depths 0-30m within 250m of site (m2)",
       y = "Biomass (kg per m2)",
       color = NULL) +
  scale_color_manual(values = c("#ff7eb6", "#d4bbff")) +
  theme_minimal() +
  theme(axis.title = element_text(size = 12)) 

length(unique(data_subset$site))
length(unique(data_subset$affiliated_mpa))
length(unique(data_subset$site[data_subset$site_type == "Reference"]))
length(unique(data_subset$site[data_subset$site_type == "MPA"]))
