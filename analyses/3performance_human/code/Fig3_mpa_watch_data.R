

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
gisdir <- file.path(basedir, "gis_data/processed")
datadir <- file.path(basedir, "mpa_watch/processed")
plotdir <- "analyses/3performance_human/figures"

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Read MPA data
mpas_orig <- readRDS(file.path(basedir, "mpa_traits/processed", "CA_mpa_metadata.Rds"))
types_use <- c("SMR", "SMRMA", "SMCA", "SMCA (No-Take)")
mpas <- mpas_orig %>% 
  filter(type %in% types_use)

# To do list
# Cull based on start time and end time


# Build data
################################################################################

# Build data
data_orig <- readRDS(file.path(datadir, "MPA_Watch_2011_2022_surveys_ca_programs_wide.Rds"))
col_key <- readxl::read_excel(file.path(datadir, "column_key_ca_programs.xlsx"))

# Build data
data <- data_orig %>% 
  # Simplify
  select(survey_id, mpa, mpa_id, survey_type, date, duration_hr, total_activities:comments) %>% 
  # Remove invalid surveys
  filter(duration_hr > 0) %>% 
  # Gather
  gather(key="use", value="n_obs", 7:ncol(.)) %>% 
  # Left join
  left_join(col_key) %>% 
  # Reduce to data of interest
  filter(type!="Total" & consumptive %in% c("Consumptive", "Non-consumptive") & !grepl("estimate?", use)) %>% 
  # Convert number of observations to numeric
  mutate(n_obs=as.numeric(n_obs)) %>% 
  # Reduce
  filter(!is.na(n_obs) & n_obs>0)

# Examine survey coverage
################################################################################

# Examine survey coverage
survey_coverage <- data %>% 
  # Number of activies on each survey by location/type
  select(survey_id, mpa, mpa_id, survey_type, date) %>% 
  unique() %>% 
  # Add year, month, dummy date
  mutate(year=lubridate::year(date),
         month=lubridate::month(date),
         date_dummy=lubridate::ymd(paste(year, month, 1, sep="-"))) %>% 
  # Number of surveys
  group_by(year, month, date_dummy, mpa, mpa_id, survey_type) %>% 
  summarize(nsurveys=n_distinct(survey_id)) %>% 
  ungroup()

# Number of MPAs and control sites
n_distinct(data$survey_id)
n_distinct(survey_coverage$mpa[survey_coverage$survey_type=="MPA"])
n_distinct(survey_coverage$mpa[survey_coverage$survey_type=="Control"])

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

# Plot survey coverage
g <- ggplot(survey_coverage, aes(x=date_dummy, y=mpa, fill=nsurveys)) +
  facet_grid(survey_type~., space="free_y", scales="free_y") +
  geom_tile(color="grey30", lwd=0.05) +
  # Add vertical lines
  geom_vline(xintercept="")
  # Labels
  labs(x="Month", y="") +
  # Legend
  scale_fill_gradientn(name="# of surveys", colors=RColorBrewer::brewer.pal(9, "Blues"), trans="log2") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + theme1
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigSX_mpa_watch_survey_coverage.png"), 
       width=6.5, height=7, units="in", dpi=600)



# Examine survey coverage
################################################################################

# Summarize data
data1 <- data %>% 
  # Number of activies on each survey by location/type
  group_by(survey_id, mpa, mpa_id, survey_type, date, duration_hr, location, consumptive) %>% 
  summarize(n_activities=sum(n_obs)) %>% 
  ungroup() %>% 
  # Number per hour 
  mutate(activities_per_hour=n_activities/duration_hr)




