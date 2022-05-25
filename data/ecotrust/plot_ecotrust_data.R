
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data/"
indir <- file.path(basedir, "ecotrust/raw")
outdir <- file.path(basedir, "ecotrust/processed")
plotdir <- "data/ecotrust/figures"

# Read data
data <- readRDS(file=file.path(outdir, "ecotrust_survey_data.Rds"))


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=7),
                   plot.title=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = "bottom",
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot(data %>% filter(category=="MPA"),
             aes(x=prop, y=port_complex, fill=response %>% as.character())) +
  facet_wrap(~question, nrow=1) +
  geom_bar(stat="identity", col="grey30", lwd=0.1) +
  # Labels
  labs(x="Percent of respondents", y="",
       title="EcoTrust MPA questions") +
  scale_x_continuous(labels=scales::percent) +
  # Legend
  scale_fill_manual(name="Score", values=RColorBrewer::brewer.pal(5, "RdBu")) +
  guides(fill = guide_legend(nrow = 1)) +
  # Theme
  theme_bw() + my_theme
g1

ggsave(g1, filename=file.path(plotdir, "FigX_ecotrust_mpa_qs.png"),
       width=6.5, height=3, units="in", dpi=600)

# Plot data
g2 <- ggplot(data %>% filter(category=="Economic"),
             aes(x=prop, y=port_complex, fill=response %>% as.character())) +
  facet_wrap(~question, nrow=1) +
  geom_bar(stat="identity", col="grey30", lwd=0.1) +
  # Labels
  labs(x="Percent of respondents", y="",
       title="EcoTrust economic questions") +
  scale_x_continuous(labels=scales::percent) +
  # Legend
  scale_fill_manual(name="Score", values=RColorBrewer::brewer.pal(6, "RdBu")) +
  guides(fill = guide_legend(nrow = 1)) +
  # Theme
  theme_bw() + my_theme
g2

ggsave(g2, filename=file.path(plotdir, "FigX_ecotrust_economic_qs.png"),
       width=6.5, height=3, units="in", dpi=600)

# Plot data
g3 <- ggplot(data %>% filter(category=="Environmental"),
             aes(x=prop, y=port_complex, fill=response %>% as.character())) +
  facet_wrap(~question, nrow=1) +
  geom_bar(stat="identity", col="grey30", lwd=0.1) +
  # Labels
  labs(x="Percent of respondents", y="",
       title="EcoTrust environment questions") +
  scale_x_continuous(labels=scales::percent) +
  # Legend
  scale_fill_manual(name="Score", values=RColorBrewer::brewer.pal(5, "RdBu")) +
  guides(fill = guide_legend(nrow = 1)) +
  # Theme
  theme_bw() + my_theme
g3

ggsave(g3, filename=file.path(plotdir, "FigX_ecotrust_environmental_qs.png"),
       width=6.5, height=3, units="in", dpi=600)

# Plot data
g4 <- ggplot(data %>% filter(category=="Social"),
             aes(x=prop, y=port_complex, fill=response %>% as.character())) +
  facet_wrap(~question, nrow=1) +
  geom_bar(stat="identity", col="grey30", lwd=0.1) +
  # Labels
  labs(x="Percent of respondents", y="",
       title="EcoTrust social questions") +
  scale_x_continuous(labels=scales::percent) +
  # Legend
  scale_fill_manual(name="Score", values=RColorBrewer::brewer.pal(5, "RdBu")) +
  guides(fill = guide_legend(nrow = 1)) +
  # Theme
  theme_bw() + my_theme
g4

ggsave(g4, filename=file.path(plotdir, "FigX_ecotrust_social_qs.png"),
       width=6.5, height=3, units="in", dpi=600)



