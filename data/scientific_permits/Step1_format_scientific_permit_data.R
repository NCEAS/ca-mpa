

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data/"
indir <- file.path(basedir, "scientific_permits/raw")
outdir <- file.path(basedir, "scientific_permits/processed")
plotdir <- "data/scientific_permits/figures"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "MPA Permits_Total_Per_Year_220517 raw.xlsx"))

# Read MPA attributes
mpas <- readRDS(file.path(basedir, "mpa_traits/processed", "CA_mpa_metadata.Rds"))


# Format data
################################################################################

# Format data 
data <- data_orig %>%
  # Rename
  janitor::clean_names("snake") %>% 
  # Gather
  select(-total) %>% 
  gather(key="year", value="npermits", 2:ncol(.)) %>% 
  # Format year
  mutate(year=gsub("x", "", year) %>% as.numeric()) %>% 
  # Simplify
  filter(!is.na(npermits)) %>% 
  # Fix MPAs
  mutate(mpa=recode(mpa,
                    "Ano Nuevo SMR"="Año Nuevo SMR",               
                    "Drake's Estero SMCA"="Drakes Estero SMCA",
                    "Lovers Point-Julia Platt SMR"="Lovers Point - Julia Platt SMR")) %>% 
  # Add region
  left_join(mpas %>% select(mpa, region), by="mpa") %>% 
  # Arrange
  select(region, mpa, year, everything()) %>% 
  arrange(region, mpa, year)

# Check MPA names
data$mpa[!data$mpa %in% mpas$mpa] %>% sort()
range(data$year)

# Export data
saveRDS(data, file=file.path(outdir, "CA_2012_2021_mpa_scientific_permits.Rds"))


# Format data
################################################################################

# Theme
my_theme <-  theme(axis.text.y=element_text(size=5),
                   axis.text.x=element_text(size=6),
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
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot
g <- ggplot(data, aes(x=year, y=mpa, fill=npermits)) +
  facet_grid(region~., space="free_y", scales="free_y") +
  geom_tile() +
  # Labels
  labs(x="Year", y="") +
  scale_x_continuous(breaks=seq(2010, 2020, 2)) +
  # Legend
  scale_fill_gradientn(name="# of permits", colors=RColorBrewer::brewer.pal(9, "Blues")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
g

# Export data
ggsave(g, filename=file.path(plotdir, "FigSX_scientific_permits_over_time_by_mpa.png"), 
       width=6.5, height=7.5, units="in", dpi=600)

