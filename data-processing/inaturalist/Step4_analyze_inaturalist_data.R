

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(rinat)
library(tidyverse)

# Directories
indir <- "data/inaturalist/raw"
outdir <- "data/inaturalist/processed"
plotdir <- "data/inaturalist/figures"

# Read data
data_orig <- readRDS(file.path(outdir, "2000_2020_inaturalist_data_inside_mpas.Rds"))


# Plot number of iNaturalist users by MPA
################################################################################

# Build data
stats1 <- data_orig %>%
  filter(!is.na(mpa)) %>%
  group_by(mpa) %>%
  summarize(n_users=n_distinct(user_name)) %>%
  ungroup() %>%
  arrange(desc(n_users))

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   plot.title=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.3, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))
# Plot data
g <- ggplot(stats1, aes(y=mpa %>% factor(., mpa), x=n_users)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of users", y="", title="iNaturalist users inside MPAs (2011-2022") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_inat_submissions_by_mpa.png"),
       width=5.5, height=7, units="in", dpi=600)





