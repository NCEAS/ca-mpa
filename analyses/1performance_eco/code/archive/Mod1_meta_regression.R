#title: "CA MPA Performance biomass meta analyses"
#author: "Joshua G. Smith"
#date: "8/3/2023"

rm(list=ls())

#required packages
librarian::shelf(ggplot2, tidyverse, here, metafor)

#set directories
data_path <- "/home/shares/ca-mpa/data/sync-data/"
biomass_dat <-  paste0(data_path,"monitoring/processed_data/biomass_processed")
fig_dir <- here::here("analyses","1performance_eco","figures")
tab_dir <- here::here("analyses","1performance_eco","tables")
dat_path <- here::here("analyses","1performance_eco","output")

#read data
biomass_mod <- readRDS(file.path(dat_path, "biomass_with_moderators.Rds")) %>% 
  mutate(habitat = ifelse(habitat == "Rocky reef","Shallow reef",habitat))

################################################################################
#calculate the effect size for each MPA

#calcualte effect size for each year and MPA
dat <- escalc(measure="ROM", m1i=biomass_smr + scalar_smr, m2i=biomass_ref + scalar_ref, sd1i=sd_smr, 
              sd2i=sd_ref, n1i=n_rep_smr, n2i=n_rep_ref, data = biomass_mod)

dat <- dat %>%
  filter(!is.na(vi), vi > 0)


################################################################################
#build meta regression

# Set reference levels
dat$state_region <- factor(dat$state_region, levels = unique(dat$state_region))
dat$state_region <- relevel(dat$state_region, ref = "North Coast")
dat$habitat <- factor(dat$habitat, levels = unique(dat$habitat))
dat$habitat <- relevel(dat$habitat, ref = "Surf zone")


#random effects
#`random = ~ 1 | habitat` specifies a random effect for habitat, allowing the effect
#sizes from affiliated_mpa within the same habitat to be correlated. This allows the model
#to account for within-study correlation due to repeated measures over time within 
#the same habitat. 
res <- rma.mv(yi,vi, method="REML", data=dat, mod =  ~age_at_survey + 
                state_region + habitat, random = ~1 | habitat) 

res <- rma.mv(yi,vi, method="REML", data=dat, 
              mod =  ~age_at_survey + 
                      state_region + 
                      size +
                      habitat_richness +
                      habitat_diversity +
                      log(fishing_pressure)+
                      target_status,
                      random = ~1 | habitat) 

# Tidy the results of the meta-analysis
res.out <- tidy(res)

# Create additional columns for the lower and upper bounds of the confidence intervals
res.out <- res.out %>%
  mutate(ci.lb = estimate - 1.96 * std.error,
         ci.ub = estimate + 1.96 * std.error)

# Save the output to a .csv file
write.csv(res.out, file.path(tab_dir, "Table1_meta_analysis_results.csv"), row.names = FALSE)


################################################################################
#Now lets look at individual MPAs

res <- rma.mv(yi,vi, method="REML", data=dat, mod =  ~affiliated_mpa*target_status, random = ~1 | habitat) 


################################################################################
#plot individual MPAs

# Tidy the results of the meta-analysis
res.out <- tidy(res)

# Drop "affiliated_mpa" part of the string in 'term'
res.out$term <- sub("^affiliated_mpa", "", res.out$term)

# Create additional columns for the lower and upper bounds of the confidence intervals
res.out <- res.out %>%
  mutate(ci.lb = estimate - 1.96 * std.error,
         ci.ub = estimate + 1.96 * std.error,
         target_status = case_when(
           grepl("Targeted", term) ~ "Targeted",
           grepl("Nontargeted", term) ~ "Nontargeted",
           TRUE ~ NA_character_
         )
         )


res.out <- res.out %>%
  mutate(target_status = case_when(
    grepl("Targeted", term) ~ "Targeted",
    grepl("Nontargeted", term) ~ "Nontargeted",
    TRUE ~ NA_character_
  ))







