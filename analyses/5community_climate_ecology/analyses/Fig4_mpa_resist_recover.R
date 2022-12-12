

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(patchwork)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
plotdir <- "analyses/5community_climate_ecology/figures"

# Read data
data_orig <- read.csv("analyses/5community_climate_ecology/output/mpa_betadisp_mod.csv")

# Read MPA meta-data
mpas_orig <- readRDS(file.path(basedir, "mpa_traits/processed", "CA_mpa_metadata.Rds"))

# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>%
  rename(site_type=mpa_type) %>% 
  # Format MPA name
  mutate(mpa=stringr::str_to_title(mpa) %>% gsub("Smr", "SMR", .) %>% gsub("Smca", "SMCA", .)) %>% 
  mutate(mpa=recode(mpa, 
                    "Ano Nuevo SMR"="Año Nuevo SMR")) %>% 
  # Add region
  left_join(mpas_orig %>% select(mpa, region), by="mpa") %>% 
  # Create type
  mutate(period=paste(period_1, period_2, sep="-"),
         process=recode_factor(period,
                        "before-during"="Resistance",
                        "before-after"="Recovery")) %>% 
  # Arrange
  select(habitat, region, mpa, site_type, process, period, distance) %>% 
  # Spread
  spread(key="site_type", value="distance") %>% 
  rename(dist_ref=ref, dist_mpa=smr) %>% 
  # Calculate proportion prevented
  mutate(prop=(dist_ref-dist_mpa)/dist_ref) %>% 
  # Remove sites that aren't MPAs
  filter(!is.na(region) & !is.na(dist_mpa))

# Check MPA names
mpa_names <- sort(unique(data$mpa))
mpa_names[!mpa_names %in% mpas_orig$mpa]


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                   axis.title=element_blank(),
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
g <-ggplot(data, aes(x=habitat, y=mpa, size=dist_mpa, fill=prop)) +
  facet_grid(region~process, scales='free_y', space="free_y") +
  geom_point(pch=21) +
  # Labels
  labs(x="", y="", title="") +
  # Legend
  scale_size_continuous(name="Shift distance\n(smaller=greater resilience)") +
  scale_fill_gradient2(name="% of shift\nprevented (blue)\nor exacerbated (red)",
                       label=scales::percent,
                       midpoint=0, mid="white", low="darkred", high="navy",) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig4_mpa_resist_recover.png"), 
       width=4.75, height=3.5, units="in", dpi=600)

