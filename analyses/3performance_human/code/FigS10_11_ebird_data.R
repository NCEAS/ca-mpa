

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(lubridate)
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
mpadir <- file.path(basedir, "mpa_traits/processed")
datadir <- file.path(basedir, "ebird/processed")
plotdir <- "analyses/3performance_human/figures"
outputdir <- "analyses/3performance_human/output"

# Read data
data_orig <- readRDS(file.path(datadir, "CA_ebird_data_inside_mpas_100m_buffer.Rds"))

# Read MPA data
mpas_orig <- readRDS(file.path(mpadir, "CA_mpa_metadata.Rds"))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")


# Coverage
################################################################################

# Coverage
coverage <- data_orig %>% 
  # Add MPA metadata
  left_join(mpas_orig %>% select(region, type, mpa, mlpa)) %>% 
  # Reduce to MPAs of interest
  filter(mlpa == "MLPA") %>% 
  # Correct region
  # mutate(region=recode(region, "San Francisco Bay"="SF Bay")) %>% 
  # Add year, month, dummy date
  mutate(year=lubridate::year(survey_date),
         month=lubridate::month(survey_date),
         date_dummy=lubridate::ymd(paste(year, month, 1, sep="-"))) %>% 
  # Summarize
  group_by(region, mpa, date_dummy) %>% 
  summarize(nobservers=n_distinct(observer_id),
            nobservations=n(),
            nsurveys=n_distinct(survey_id)) %>% 
  ungroup()

# MPA order
mpa_order <- coverage %>% 
  # Order
  group_by(region, mpa) %>% 
  summarize(nobservations_tot=sum(nobservations)) %>% 
  ungroup() %>% 
  arrange(region, desc(nobservations_tot))

# Theme
theme1 <-  theme(axis.text=element_text(size=6),
                 axis.text.y=element_text(size=5),
                 axis.title=element_text(size=8),
                 axis.title.y=element_blank(),
                 legend.text=element_text(size=6),
                 legend.title=element_text(size=7),
                 strip.text=element_text(size=7),
                 # Gridlines
                 panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(), 
                 axis.line = element_line(colour = "black"),
                 # Legend
                 legend.background = element_rect(fill=alpha('blue', 0)))

# Plot  coverage
g <- ggplot(coverage, aes(x=date_dummy, 
                          y=mpa %>% factor(., levels=mpa_order$mpa), fill=nsurveys)) +
  facet_grid(region~., space="free_y", scale="free_y") +
  geom_tile(color="grey30", lwd=0.05) +
  # Axes
  scale_x_date(lim=c("1950-01-01", NA) %>% ymd(),
               breaks = seq(ymd("1950-01-01"), ymd("2020-01-01"), by="5 years"), 
               date_labels="%Y") +
  # Labels
  labs(x="Month", y="") +
  # Legend
  scale_fill_gradientn(name="# of surveys", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(), trans="log2") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + theme1
g  

# Export plot
ggsave(g, filename=file.path(plotdir, "FigS10_ebird_obs_coverage.png"), 
       width=6.5, height=7.5, units="in", dpi=600)



# Build data
################################################################################

# Estuary MPAs
estuary_mpas <- c("South Humboldt Bay SMRMA", "Ten Mile Estuary SMCA", "Big River Estuary SMCA",     
                  "Navarro River Estuary SMCA", "Estero Americano SMRMA", "Estero de San Antonio SMRMA",
                  "Drakes Estero SMCA", "Estero de Limantour SMR", "Elkhorn Slough SMR",        
                  "Elkhorn Slough SMCA", "Morro Bay SMRMA", "Morro Bay SMR",              
                  "Goleta Slough SMCA (No-Take)", "Bolsa Bay SMCA", "Bolsa Chica Basin SMCA (No-Take)",     
                  "Upper Newport Bay SMCA", "Batiquitos Lagoon SMCA (No-Take)", "San Elijo Lagoon SMCA (No-Take)",      
                  "San Dieguito Lagoon SMCA", "Famosa Slough SMCA (No-Take)", "Moro Cojo Slough SMR")
length(estuary_mpas)
estuary_mpas[!estuary_mpas %in% mpas_orig$mpa]

# Estuary states for manuscript
mpas_orig %>% 
  # MPAs of interest
  filter(mlpa=="MLPA") %>% 
  # Mark estuary
  mutate(estuary=ifelse(mpa %in% estuary_mpas, "estuary", "non-estuary")) %>% 
  # Summarize
  group_by(estuary) %>% 
  summarize(n=n(),
            area_sqkm=sum(area_sqkm)) %>% 
  ungroup() %>% 
  mutate(prop_n=n/sum(n)*100,
         prop_km2=area_sqkm/sum(area_sqkm)*100)


# Stats for manuscript
data_orig %>% 
  # Add MPA meta-data
  left_join(mpas_orig %>% select(mpa, type, region, mlpa), by="mpa") %>% 
  # Reduce to MPAs of interest
  filter(mlpa == "MLPA") %>% 
  # Group by
  group_by() %>% 
  summarize(n=n(),
            n_surveys=n_distinct(survey_id),
            n_people=n_distinct(observer_id),
            n_mpas=n_distinct(mpa))

# MPA stats
stats_full <- data_orig %>%
  # Filter 2012-2021
  filter(survey_date>=ymd("2012-01-01") & survey_date<=ymd("2021-12-31")) %>% 
  # Summarize
  group_by(mpa) %>%
  summarize(n_observers=n_distinct(observer_id),
            n_surveys=n_distinct(survey_id),
            n_observations=n(),
            n_species=n_distinct(taxon_concept_id)) %>%
  ungroup() %>% 
  # Add MPA meta-data
  left_join(mpas_orig %>% select(mpa, type, region, mlpa, lat_dd, long_dd), by="mpa")

# Export
saveRDS(stats_full, file=file.path(outputdir, "ebird_indicators.Rds"))
  
# Reduce
stats <- stats_full %>% 
  # Reduce to MPAs of interest
  filter(mlpa == "MLPA") %>% 
  arrange(desc(n_observers))

# Observer time series (by region)
observer_ts1 <- data_orig %>% 
  # Add MPA meta-data
  left_join(mpas_orig %>% select(mpa, type, region, mlpa), by="mpa") %>% 
  # Reduce to MPAs of interest
  filter(mlpa=="MLPA") %>% 
  # Add year
  mutate(year=lubridate::year(survey_date)) %>% 
  # Update region
  # mutate(region=recode(region, 
  #                      "San Francisco Bay"="North Central Coast")) %>% 
  # Summarize
  group_by(region, year) %>% 
  summarize(n_observers=n_distinct(observer_id)) %>% 
  ungroup()

# Observer time series (by region)
observer_ts2 <- data_orig %>% 
  # Add MPA meta-data
  left_join(mpas_orig %>% select(mpa, type, region, mlpa), by="mpa") %>% 
  # Reduce to MPAs of interest
  filter(mlpa == "MLPA") %>% 
  # Mark estuaries
  mutate(estuary=ifelse(mpa %in% estuary_mpas, "Estuarine", "Non-estuarine"),
         estuary=factor(estuary, levels=c("Non-estuarine", "Estuarine"))) %>% 
  # Add year
  mutate(year=lubridate::year(survey_date)) %>% 
  # Create visit id
  mutate(visit_id=paste(observer_id, mpa, survey_date, sep="-")) %>% 
  # Summarize
  group_by(estuary, year) %>% 
  summarize(n_visits=n_distinct(visit_id)) %>%
  # summarize(n_observers=n()) %>% 
  ungroup() %>% 
  # Proportion
  group_by(year) %>% 
  mutate(prop_visits=n_visits / sum(n_visits)) %>% 
  ungroup()

# MPA time series
mpa_ts <- data_orig %>% 
  # Add MPA meta-data
  left_join(mpas_orig %>% select(mpa, type, region, mlpa), by="mpa") %>% 
  # Reduce to MPAs of interest
  filter(mlpa=="MLPA") %>% 
  # Add year
  mutate(year=lubridate::year(survey_date)) %>% 
  # Update region
  # mutate(region=recode(region, 
  #                      "San Francisco Bay"="North Central Coast")) %>% 
  # Summarize
  group_by(region, year) %>% 
  summarize(n_mpas=n_distinct(mpa)) %>% 
  ungroup()

# Identify MPAs with zero engagement
mpas_zero <- mpas_orig %>% 
  # MLPA MPAs
  filter(mlpa=="MLPA") %>% 
  # MPAs without engagement
  filter(!mpa %in% stats$mpa)



# Plot data
################################################################################

# MPA regions
# CA/OR, Alder Creek, Pigeon Point, Point Conception, CA/MEX 
region_lats <- c(39.0, 37.18, 34.5)

# Theme
theme1 <-  theme(axis.text=element_text(size=7),
                 axis.text.y = element_text(angle = 90, hjust = 0.5),
                 axis.title=element_text(size=8),
                 plot.title=element_blank(),
                 legend.text=element_text(size=6),
                 legend.title=element_text(size=8),
                 plot.tag=element_text(size=9),
                 # Gridlines
                 panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(), 
                 axis.line = element_line(colour = "black"),
                 # Legend
                 legend.key = element_blank(),
                 legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot() +
  # Plot regions
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot MPAs
  geom_point(data=stats, 
             mapping=aes(x=long_dd, y=lat_dd, size=n_observers, fill=n_surveys), 
             pch=21, inherit.aes = F) +
  # Plot zero MPAs
  geom_point(data=mpas_zero, mapping=aes(x=long_dd, y=lat_dd), pch="x", size=2.3) +
  # Labels
  labs(x="", y="", tag="A") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  # Legend
  scale_size_continuous(name="# of eBirders") +
  scale_fill_gradientn(name="# of surveys", colors=RColorBrewer::brewer.pal(9, "Blues")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", order=2), size=guide_legend(order=1)) +
  # Crop
  coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + theme1 +
  theme(axis.title.y=element_blank(),
        legend.position = c(0.8, 0.7),
        legend.key.size = unit(0.4, "cm"))
g1

# Plot number of observers by region
g2 <- ggplot(observer_ts1, aes(x=year, y=n_observers, fill=region)) +
  geom_bar(stat="identity", lwd=0.1, color="grey30") +
  # Reference line
  geom_vline(xintercept = 2011.5, linetype="dotted", color="grey30") +
  # Labels
  labs(x="Year", y=" \n# of eBirders", tag="B") +
  # Axes
  scale_x_continuous(breaks=seq(1960, 2020, 10), limits = c(1960, 2022)) +
  # Legend
  scale_fill_ordinal(name="Region") +
  # Theme
  theme_bw() + theme1 +
  theme(legend.position = c(0.25, 0.75),
        legend.key.size = unit(0.25, "cm"))
g2

# Plot number of observers by estuary
g3 <- ggplot(observer_ts2, aes(x=year, y=prop_visits, fill=estuary)) +
  geom_bar(stat="identity", lwd=0.1, color="grey30") +
  # Reference line
  geom_hline(yintercept = 0.5, color="black", linetype="dotted", size=0.5) +
  # Labels
  labs(x="Year", y="Percent of visits\nlogged by eBirders", tag="B") +
  # Axes
  scale_x_continuous(breaks=seq(1960, 2020, 10), limits = c(1960, 2022)) +
  scale_y_continuous(labels=scales::percent) +
  # Legend
  scale_fill_manual(name="MPA type", values=c("lightblue1", "greenyellow"), guide = guide_legend(reverse = TRUE)) +
  # Theme
  theme_bw() + theme1 +
  theme(legend.position = "top",
        legend.margin = margin(-10,0,-5,0),
        legend.key.size = unit(0.25, "cm"))
g3

# # Plot number of MPAs by region
# g3 <- ggplot(mpa_ts, aes(x=year, y=n_mpas, fill=region)) +
#   geom_bar(stat="identity", lwd=0.1, color="grey30") +
#   # Reference line
#   geom_hline(yintercept=125, linetype="dotted", color="grey30") +
#   # Labels
#   labs(x="Year", y="# of MPAs surveyed", tag="C") +
#   # Axes
#   scale_x_continuous(breaks=seq(1960, 2020, 10), limits = c(1960, 2021)) +
#   scale_y_continuous(breaks=seq(0,120, 20)) +
#   # Legend
#   scale_fill_ordinal(name="") +
#   # Theme
#   theme_bw() + theme1 +
#   theme(legend.position = c(0.25, 0.75),
#         legend.key.size = unit(0.25, "cm"))
# g3

# Merge
layout_matrix <- matrix(c(1,2, 
                          1,3), ncol=2, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, g3, layout_matrix=layout_matrix, widths=c(0.52, 0.48))

# Export figure
ggsave(g, filename=file.path(plotdir, "FigS11_ebird_data.png"), 
       width=6.5, height=5.25, units="in", dpi=600)


