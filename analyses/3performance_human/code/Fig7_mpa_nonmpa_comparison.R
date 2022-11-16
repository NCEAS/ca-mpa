

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
data_orig <- readRDS(file.path(datadir, "counterfactual_results.Rds"))


# Build data
################################################################################

# Build stats
stats <- data_orig %>% 
  group_by(mpa, cell_type) %>% 
  summarize(inat_observers_n=sum(inat_observers_n, na.rm=T)+1,
            ebird_observers_n=sum(ebird_observers_n, na.rm=T)+1,
            reef_surveys_n=sum(reef_surveys_n, na.rm=T)+0.1)

# Build ratios
ratios <- stats %>% 
  # Gather
  gather(key="indicator", value="value", 3:ncol(.)) %>% 
  # Format indicator
  mutate(indicator=recode(indicator,
                          "reef_surveys_n"="REEF",
                          "ebird_observers_n"="eBird",
                          "inat_observers_n"="iNaturalist")) %>% 
  # Spread 
  rename(mpa_name=mpa) %>% 
  spread(key="cell_type", value="value") %>% 
  rename(mpa=MPA, counterfactual=Counterfactual) %>% 
  # Calculate ratio
  mutate(ratio=log(mpa / counterfactual))

# Perform t-tests
reef.ttest <- ratios %>% 
  filter(indicator=="REEF") %>% 
  pull(ratio) %>% 
  t.test()
inat.ttest <- ratios %>% 
  filter(indicator=="iNaturalist") %>% 
  pull(ratio) %>% 
  t.test()
ebird.ttest <- ratios %>% 
  filter(indicator=="eBird") %>% 
  pull(ratio) %>% 
  t.test()

# Recode ratios
ratios1 <- ratios %>% 
  mutate(indicator=recode_factor(indicator,
                                 "iNaturalist"="# of iNaturalist\nobservers\n(p=0.12)",
                                 "eBird"="# of eBird\n observers*\n(p=0.02)",
                                 "REEF"="# of REEF\nsurveys**\n(p<0.0001)"))


# Plot data 
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   axis.title.y=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = c(0.75, 0.25),
                   legend.key.size = unit(0.3, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Plot data
g <- ggplot(ratios1, aes(y=indicator, x=ratio)) +
  # geom_violin(fill="grey90", draw_quantiles=0.5) +
  geom_boxplot(fill="grey80", color="grey30", outlier.color = NA) +
  geom_jitter(height = 0.2, alpha=0.3, fill="grey60", color="black") +
  # Reference line
  geom_vline(xintercept = 0, linetype="solid", color="black") +
  # Text reference
  annotate(geom="text", x=min(ratios$ratio, na.rm=T), y=3.8, 
           label="More engagement\nin non-MPAs", vjust=1, hjust=0, size=2) +
  annotate(geom="text", x=max(ratios$ratio, na.rm=T), y=3.8, 
           label="More engagement\nin MPAs", vjust=1, hjust=1, size=2) +
  # Labels
  labs(x="Log-response ratio\nlog(MPA+1 / Non-MPA+1)", y="") +
  # Axis
  scale_x_continuous(breaks=seq(-6, 6, 2)) +
  scale_y_discrete(expand=c(-0.3, 1.3)) +
  # Theme
  theme_bw() + my_theme
g

# Export figure
ggsave(g, filename=file.path(plotdir, "Fig7_mpa_nonmpa_comparison.png"), 
       width=3.5, height=2, units="in", dpi=600)



