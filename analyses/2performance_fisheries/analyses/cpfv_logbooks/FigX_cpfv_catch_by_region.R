

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
blockdir <- "analyses/2performance_fisheries/analyses/blocks"
cpfvdir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/california/cdfw_data/data/confidential/cpfv_logbooks/processed"
plotdir <- "analyses/2performance_fisheries/analyses/cpfv_logbooks/figures" 

# Read block data
blocks <- readRDS(file=file.path(blockdir, "blocks_by_mlpa_region_w_mpa_stats.Rds"))

# Read CPFV logbook data
data_orig <- readRDS(file.path(cpfvdir, "CDWF_2000_2020_cpfv_logbook_data.Rds"))


# Build data
################################################################################

# Build data
data <- data_orig %>% 
  # Summarize by year and block
  group_by(year, block_id) %>% 
  summarize(n_kept=sum(n_kept, na.rm=T)) %>% 
  ungroup() %>% 
  # Add block info
  left_join(blocks %>% select(block_id, mlpa_region, mpa_yn)) %>% 
  # Summarize by year, region, block type
  group_by(year, mlpa_region, mpa_yn) %>% 
  summarise(n_kept=sum(n_kept, na.rm = T)) %>% 
  ungroup() %>% 
  # Regions of interest
  filter(grepl("Coast", mlpa_region)) %>% 
  # Add proportion by year
  group_by(year, mlpa_region) %>% 
  mutate(prop_kept=n_kept/sum(n_kept)) %>% 
  ungroup() %>% 
  # Order MPA
  mutate(mpa_yn=factor(mpa_yn, levels=c("MPA", "Non-MPA") %>% rev()))

# Add implementation year
mpa_yr <- tibble(mlpa_region=c("South Coast", "Central Coast", "North Central Coast", "North Coast"),
                 year=c(2012, 2007, 2010, 2012)-0.5) %>% 
  mutate(mlpa_region=factor(mlpa_region,
                            levels=levels(data$mlpa_region)))

# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.3, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Plot data
g1 <- ggplot(data, aes(x=year, y=n_kept/1e3, fill=mpa_yn)) +
  facet_wrap(~mlpa_region, scales="free_y", nrow=1) +
  geom_bar(stat="identity", color="grey30", lwd=0.1) +
  # Reference line
  geom_vline(data=mpa_yr, mapping=aes(xintercept=year)) +
  # Labels
  labs(x="Year", y="Thousands of fish kept") +
  # Legend
  scale_fill_discrete(name="Block type", guide = guide_legend(reverse = TRUE)) +
  # Themes
  theme_bw() + my_theme +
  theme(legend.position = "top")
g1

# Plot data
g2 <- ggplot(data, aes(x=year, y=prop_kept, fill=mpa_yn)) +
  facet_wrap(~mlpa_region, scales="free_y", nrow=1) +
  geom_bar(stat="identity", color="grey30", lwd=0.1) +
  # Reference line
  geom_vline(data=mpa_yr, mapping=aes(xintercept=year)) +
  # Labels
  labs(x="Year", y="Percentage of fish kept") +
  scale_y_continuous(labels=scales::percent) +
  # Legend
  scale_fill_discrete(name="Block type", guide = guide_legend(reverse = TRUE)) +
  # Themes
  theme_bw() + my_theme +
  theme(legend.position = "none")
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, heights=c(0.55, 0.45))
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigX_cpfv_catch_by_region.png"), 
       width=6.5, height=4.5, units="in", dpi=600)




