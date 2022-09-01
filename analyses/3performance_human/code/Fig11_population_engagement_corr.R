

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
datadir <- "analyses/3performance_human/output"
plotdir <- "analyses/3performance_human/figures"

# Read data
data_orig <- readRDS(file=file.path(datadir, "CA_MPA_human_use_indicators.Rds"))


# Build data
################################################################################

# Build data
data <- data_orig %>% 
  # Reduce to MPAs of interest
  filter(mlpa=="MLPA") %>% 
  # Simplify
  select(mpa, mpa_short, npeople_50km, inat_observers_n) %>% 
  # NA omit
  na.omit()


# Identify charismatic MPAs
################################################################################

# Fit linear model
lmfit <- lm(inat_observers_n ~ npeople_50km, data)
summary(lmfit)

# Extract residuals and predictions
re <- lmfit$residuals
pred <- lmfit$fitted.values

# Calculate residual percent difference
re_perc <- re / pred * 100
hist(re_perc, breaks=seq(-100, 600, 10))

# Classify charismatic MPAs
# Unalluring
data1 <- data %>% 
  mutate(re=re,
         re_perc=re_perc,
         category=cut(re_perc, 
                      breaks=c(-Inf, -75, 75, Inf), 
                      labels=c("Inaccessible", "Typical", "Charismatic"))) 

# Export charisma key
write.csv(data1, file=file.path(datadir, "CA_MPA_charisma_key.csv"), row.names = F)

# Fit linear model to typical
lmfit2 <- lm(inat_observers_n ~ npeople_50km, data1 %>% filter(category=="Typical"))
summary(lmfit2)

# Plot data 
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = c(0.85, 0.88),
                   legend.key.size = unit(0.4, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot() +
  # Plot regression
  geom_smooth(data=data1, formula='y ~ x',
              aes(x=npeople_50km/1e6, y=inat_observers_n),
              method=lm, color="grey50", fill="grey80", alpha=0.5) +
  # Plot regression
  geom_smooth(data=data1 %>% filter(category=="Typical"), formula='y ~ x',
              aes(x=npeople_50km/1e6, y=inat_observers_n),
              method=lm, color="darkgreen", fill="green4", alpha=0.5) +
  # Plot points
  geom_point(data=data1, mapping=aes(x=npeople_50km/1e6, y=inat_observers_n, fill=category), 
             pch=21, size=1.9) +
  # Plot all charisma labels
  ggrepel::geom_text_repel(data1 %>% filter(category=="Charismatic"),
                           mapping=aes(x=npeople_50km/1e6, y=inat_observers_n, label=mpa_short), 
                           inherit.aes = F, size=2, max.overlaps = 1000, color="grey60") +
  # Plot select inaccessible labels
  ggrepel::geom_text_repel(data1 %>% filter(category=="Inaccessible" & npeople_50km/1e6>1),
                           mapping=aes(x=npeople_50km/1e6, y=inat_observers_n, label=mpa_short), 
                           inherit.aes = F, size=2, max.overlaps = 1000, color="grey60") +
  # Labels
  labs(x="Human population size\n(millions of people within 50 km)", 
       y="Human engagement\n(# of iNaturalist observers, 2012-2021)") +
  scale_fill_discrete(name="MPA type",
                    # values=c("orange", "grey70", "purple"),
                    guide = guide_legend(reverse = TRUE)) +
  # Theme
  theme_bw() + my_theme
g

# Export figure
ggsave(g, filename=file.path(plotdir, "Fig11_population_engagement_corr.png"), 
       width=4.5, height=4.5, units="in", dpi=600)



