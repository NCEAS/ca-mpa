

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

# MPAs of interest
types_use <- c("SMR", "SMRMA", "SMCA", "SMCA (No-Take)")

# Build data
data <- data_orig %>% 
  # Reduce to MPAs of interest
  filter(type %in% types_use) %>% 
  # Simplify
  select(mpa, mpa_short, npeople_50km, inat_observers_tot) %>% 
  na.omit()


# Identify charismatic MPAs
################################################################################

# Fit linear model
lmfit <- lm(inat_observers_tot ~ npeople_50km, data)
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
         charisma_yn=ifelse(re_perc>100, "Charismatic", "Typical") %>% 
           factor(., levels=c("Typical", "Charismatic")))

# Export charisma key
write.csv(data1, file=file.path(datadir, "CA_MPA_charisma_key.csv"), row.names = F)

# Fit linear model to typical
lmfit2 <- lm(inat_observers_tot ~ npeople_50km, data1 %>% filter(charisma_yn=="Typical"))
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
                   legend.position = c(0.8, 0.8),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot() +
  # Plot regression
  geom_smooth(data=data1, formula='y ~ x',
              aes(x=npeople_50km/1e6, y=inat_observers_tot),
              method=lm, color="grey50", fill="grey80", alpha=0.5) +
  # Plot regression
  geom_smooth(data=data1 %>% filter(charisma_yn=="Typical"), formula='y ~ x',
              aes(x=npeople_50km/1e6, y=inat_observers_tot),
              method=lm, color="darkred", fill="red", alpha=0.5) +
  # Plot points
  geom_point(data=data1, mapping=aes(x=npeople_50km/1e6, y=inat_observers_tot, fill=charisma_yn), 
             pch=21, size=1.9) +
  # Plot labels
  ggrepel::geom_text_repel(data1 %>% filter(charisma_yn=="Charismatic"),
                           mapping=aes(x=npeople_50km/1e6, y=inat_observers_tot, label=mpa_short), 
                           inherit.aes = F, size=2, max.overlaps = 1000, color="grey60") +
  # Labels
  labs(x="Millions of people\nwithin 50 km", y="Total iNaturalist observers\nfrom 2000 to 2018") +
  scale_fill_discrete(name="MPA type") +
  # Theme
  theme_bw() + my_theme
g

# Export figure
ggsave(g, filename=file.path(plotdir, "Fig8_population_engagement_corr.png"), 
       width=4.5, height=4.5, units="in", dpi=600)



