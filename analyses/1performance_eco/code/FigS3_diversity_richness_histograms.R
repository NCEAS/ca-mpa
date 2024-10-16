

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- here::here("analyses","1performance_eco","output")
aurora <- "/home/shares/ca-mpa/data/sync-data/"
gisdir <- "/home/shares/ca-mpa/data/sync-data/gis_data/processed"
plotdir <- here::here("analyses","1performance_eco","figures")

# Read data
rd_dat <- readRDS(file.path(basedir, "biomass_richness_diversity2.Rds"))
biomass_dat <- readRDS(file.path(basedir, "biomass_with_moderators_new2.Rds")) 


################################################################################
#prep data - aggregate data for all years > 0

# Format richness and diversity
rd_build1 <- rd_dat %>% 
  group_by(habitat, affiliated_mpa) %>%
  filter(year == max(year)) %>%
  select(habitat, year, state_region, affiliated_mpa, state_name, mpa_defacto_class, 
         shannon_unweighted_logRR, richness_unweighted_logRR, age_at_survey)%>%
  #match case with mpas_orig
  mutate(habitat = factor(habitat, levels = c("Surf zone","Kelp forest","Shallow reef","Deep reef")),
         state_region = factor(state_region, levels = c("North Coast","North Central Coast","Central Coast","South Coast")),
         state_name = factor(state_name),
         state_name = str_to_title(state_name),
         state_name = sub("([A-Za-z]+)$", "\\U\\1", state_name, perl = TRUE),
         #rename MPAs to match mpa_orig for join
         state_name = ifelse(state_name == "Campus Point SMCA","Campus Point SMCA (No-Take)",
                             ifelse(state_name == "Blue Cavern Onshore SMCA","Blue Cavern Onshore SMCA (No-Take)",
                                    ifelse(state_name == "Point Vicente SMCA","Point Vicente SMCA (No-Take)",state_name)))) 



# Summarize
################################################################################


# Theme
base_theme <-  theme(axis.text=element_text(size=7),
                     axis.title=element_text(size=8),
                     legend.text=element_text(size=7),
                     legend.title=element_text(size=8),
                     plot.tag=element_text(size=8),
                     plot.title = element_text(size = 8),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.key = element_rect(fill=alpha('blue', 0)),
                     legend.background = element_rect(fill=alpha('blue', 0)),
                     #facets
                     strip.text = element_text(size=7, face = "bold",hjust=0),
                     strip.background = element_blank()
)


# Function to calculate mean and confidence interval for t-test
calculate_ttest_ci <- function(x) {
  mean_value <- mean(x)
  se <- sd(x) / sqrt(length(x))
  margin_error <- qt(0.975, df = length(x) - 1) * se
  ci <- c(mean_value - margin_error, mean_value + margin_error)
  return(ci)
}

# Calculate mean and t-test confidence interval
ci_values <- rd_build1 %>%
  group_by(habitat) %>%
  summarize(ci_s = list(calculate_ttest_ci(shannon_unweighted_logRR)),
            mean_s = mean(shannon_unweighted_logRR),
            ci_r = list(calculate_ttest_ci(richness_unweighted_logRR)),
            mean_r = mean(richness_unweighted_logRR))

# Unnest the data frame to make it usable in ggplot
ci_values <- unnest(ci_values)

# Plot histogram
A <- ggplot(rd_build1, aes(x = shannon_unweighted_logRR)) +
  geom_histogram(binwidth = 0.1, color = "white", fill = "indianred", position = "identity", alpha = 0.7) +
  #add mean
  geom_vline(data = ci_values, aes(xintercept = mean_s), color = "black", linetype = "solid", size = 1) +
  #add confidence region
  geom_vline(data = ci_values, aes(xintercept = ci_s), color = "black", linetype = "dashed", size = 0.5) +
  geom_ribbon(data = ci_values, aes(ymin = -Inf, ymax = Inf, x = ci_s), fill = "gray", alpha = 0.4) +
  facet_wrap(~habitat, scales = "free", ncol=1) +
  labs(title = "Shannon diversity (unweighted)",
       x = "",
       y = "") +
  theme_bw() + base_theme

B <- ggplot(rd_build1, aes(x = richness_unweighted_logRR)) +
  geom_histogram(binwidth = 0.1, color = "white", fill = "indianred", position = "identity", alpha = 0.7) +
  #add mean
  geom_vline(data = ci_values, aes(xintercept = mean_r), color = "black", linetype = "solid", size = 1) +
  #add confidence region
  geom_vline(data = ci_values, aes(xintercept = ci_r), color = "black", linetype = "dashed", size = 0.5) +
  geom_ribbon(data = ci_values, aes(ymin = -Inf, ymax = Inf, x = ci_r), fill = "gray", alpha = 0.4) +
  facet_wrap(~habitat, scales = "free", ncol=1) +
  labs(title = "Richness (no. species)",
       x = "",
       y = "") +
  theme_bw() + base_theme

c <- ggpubr::ggarrange(A,B,nrow=1)

annotated_figure <- ggpubr::annotate_figure(c, 
                                            bottom = text_grob("Response ratio", size = 8, face = "plain",
                                                               hjust=0.3, vjust=-1),
                                            left = text_grob("Frequency", size = 8, face = "plain",
                                                             rot = 90,
                                                             vjust=1)
)

(t_test_result <- t.test(biomass_dat$yi, mu = 0))


ggsave(annotated_figure, filename=file.path(plotdir, "FigS1_histogram.png"), bg = "white",
       width=5, height=5, units="in", dpi=600) 


