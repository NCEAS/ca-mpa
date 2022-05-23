

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(rinat)
library(tidyverse)

# Directories
indir <- "data/mpa_watch/raw"
outdir <- "data/mpa_watch/processed"
plotdir <- "data/mpa_watch/figures"

# Read data
data_orig <- read.csv(file.path(indir, "ActivityFrequencyData_AllPrograms.csv"), as.is=T)

# Read column key
col_key <- readxl::read_excel(file.path(outdir, "column_key.xlsx"))


# Format data
################################################################################

# Format data
data <- data_orig %>%
  # Rename columns
  rename(mpa_name=mpaname, site_name=sitename, site_type=sitetype,
         lat_dd=coordx, long_dd=coordy, yday=jdate, n_surveys=nsurvey) %>%
  # Gather
  gather(key="use_orig", value="n_obs", 14:ncol(.)) %>%
  # Format date
  mutate(date=lubridate::ymd(date)) %>%
  # Extract year and month
  mutate(year=lubridate::year(date),
         month=lubridate::year(date)) %>%
  # Add use info
  left_join(col_key, by="use_orig") %>%
  select(-use_orig) %>%
  # Arrange
  select(program, mpa_name, site_name, site_type,
         mpa, control, site, long_dd, lat_dd,
         year, month, date, yday, n_surveys,
         use_catg1, use_catg2, use,
         everything()) %>%
  arrange(program, mpa_name, site_name, date)

# Inspect
str(data)
range(data$date)


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "MPA_Watch_2011_2022_activity_freq.Rds"))



# Summarize and plot
################################################################################

# Summarize data
stats <- data %>%
  filter(control==0) %>%
  count(mpa_name, use_catg2)

# Stats order
mpa_order <- stats %>%
  group_by(mpa_name) %>%
  summarize(n=sum(n)) %>%
  ungroup() %>%
  arrange(desc(n))

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = c(0.8, 0.7),
                   legend.key.size = unit(0.3, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(stats, aes(x=n, y=mpa_name %>% factor(., levels=mpa_order$mpa_name), fill=use_catg2)) +
  geom_bar(stat="identity", col="grey30", lwd=0.1) +
  # Labels
  labs(x="Numer of observations\n(from 2011 to 2022)", y="") +
  # Legend
  scale_fill_discrete(name="Activity type") +
  # Theme
  theme_bw() + my_theme
g

# Export data
ggsave(g, filename=file.path(plotdir, "FigX_mpa_watch_activities_by_mpa.png"),
       width=6.5, height=4, units="in", dpi=600)



