

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

# Read charisma key
char_key_orig <- read.csv(file=file.path(datadir, "CA_MPA_charisma_key.csv"), as.is=T)


# Build data
################################################################################

# Indicators of interest
indicators_use <- c("npeople_50km", 
                    "nonconsump_hr", "consump_hr", 
                    "inat_observers_n", "ebird_observers_n", "reef_n",
                    "permits_n", "citations_n")

# Build data
data <- data_orig %>% 
  # Reduce to MPAs of interest
  filter(mlpa=="MLPA") %>% 
  # Recode region
  mutate(region=plyr::revalue(region, c("San Francisco Bay"="North Central Coast"))) %>%
  # Gather indicators
  gather(key="indicator", value="value", 9:ncol(.)) %>% 
  # Reduce to indicators of interest
  filter(indicator %in% indicators_use) %>% 
  # Format indicators
  mutate(indicator=recode_factor(indicator,
                                 "npeople_50km"="Population size",
                                 "inat_observers_n"="iNaturalist observers",
                                 "ebird_observers_n"='eBird observers',
                                 "reef_n"="REEF surveys",
                                 "permits_n"="Scientific permits",
                                 "citations_n"="Citations",
                                 "consump_hr"="MPA Watch (consumptive)",
                                 "nonconsump_hr"="MPA Watch (non-consumptive)")) %>% 
  # Add zeros for all but MPA Watch
  mutate(value=ifelse(is.na(value) & !grepl("MPA Watch", indicator), 0, value)) %>% 
  # Scale metrics
  group_by(indicator) %>% 
  mutate(value_scaled=scale(value, center=T, scale=T)) %>% 
  ungroup()

# Derive MPA order
mpa_order <- data %>%
  filter(indicator=="Population size") %>% 
  arrange(region, desc(value))

# Order data
data_ordered <- data %>%
  mutate(mpa=factor(mpa, levels=mpa_order$mpa))

# Extract zeros
data_zeroes <- data_ordered %>% 
  filter(value==0)

# Format char key for plotting
char_key <- char_key_orig %>% 
  filter(category!="Typical") %>% 
  mutate(indicator=factor("Population size", levels=levels(data$indicator))) %>% 
  left_join(mpa_order %>% select(mpa, region), by=c("mpa"))


# Plot data - long
################################################################################

# Theme
theme1 <- theme(axis.text=element_text(size=5),
                axis.text.y=element_text(size=4.5),
                axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                axis.title=element_blank(),
                legend.text=element_text(size=6),
                legend.title=element_text(size=8),
                strip.text=element_text(size=7),
                plot.title=element_blank(),
                # Gridlines
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black"),
                # Legend
                legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data_ordered, aes(x=indicator, y=mpa, fill=value_scaled)) +
  # Facet
  facet_grid(region~., scales="free_y", space="free_y") +
  # Raster
  geom_tile(lwd=0.1, color="grey60") +
  # Mark zeroes
  geom_point(data=data_zeroes, mapping=aes(x=indicator, y=mpa), pch="x", inherit.aes = F) +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_fill_gradient2("Engagement\n(scaled and centered)", 
                       midpoint = 0, low="darkred", high="navy", mid="white", na.value="grey50") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + theme1
g

# Export figure
ggsave(g, filename=file.path(plotdir, "Fig10_performance_scorecard_long.png"), 
       width=4.5, height=7.5, units="in", dpi=600)


# Plot data - wide
################################################################################

# Subset data
data1 <- data_ordered %>% 
  filter(region!="South Coast")
data2 <- data_ordered %>% 
  filter(region=="South Coast")
data_zeroes1 <- data_zeroes %>% 
  filter(region!="South Coast")
data_zeroes2 <- data_zeroes %>% 
  filter(region=="South Coast")
char_key1 <- char_key %>% 
  filter(region!="South Coast")
char_key2 <- char_key %>% 
  filter(region=="South Coast")

# Range
range(data$value_scaled, na.rm=T) # -1.2, 10.8

# Theme
theme1 <- theme(axis.text=element_text(size=6),
                axis.text.y=element_text(size=6),
                axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                axis.title=element_blank(),
                legend.text=element_text(size=6),
                legend.title=element_text(size=7),
                strip.text=element_text(size=7),
                plot.title=element_blank(),
                # Gridlines
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black"),
                # Legend
                legend.key.size = unit(0.35, "cm"),
                legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot(data1, aes(x=indicator, y=mpa, fill=value_scaled)) +
  # Facet
  facet_grid(region~., scales="free_y", space="free_y") +
  # Raster
  geom_tile(lwd=0.1, color="grey60") +
  # Mark charisma
  geom_point(char_key1, mapping=aes(x=indicator, y=mpa, color=category), size=0.9, inherit.aes=F) +
  # Mark zeroes
  geom_point(data=data_zeroes1, mapping=aes(x=indicator, y=mpa), pch="x", inherit.aes = F) +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_color_manual(name="MPA type", values=c("navy", "darkred")) +
  scale_fill_gradient2("Engagement\n(scaled and centered)", lim=c(-1.2, 10.8),
                       midpoint = 0, low="darkred", high="navy", mid="white", na.value="grey50") +  
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + theme1 +
  theme(legend.position = "none")
g1

# Plot data
g2 <- ggplot(data2, aes(x=indicator, y=mpa, fill=value_scaled)) +
  # Facet
  facet_grid(region~., scales="free_y", space="free_y") +
  # Raster
  geom_tile(lwd=0.1, color="grey60") +
  # Mark charisma
  geom_point(char_key2, mapping=aes(x=indicator, y=mpa, color=category), size=0.9, inherit.aes=F) +
  # Mark zeroes
  geom_point(data=data_zeroes2, mapping=aes(x=indicator, y=mpa), pch="x", inherit.aes = F) +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_color_manual(name="MPA type", values=c("navy", "darkred")) +
  scale_fill_gradient2(name="Engagement\n(scaled and centered)", lim=c(-1.2, 10.8),
                       midpoint = 0, low="darkred", high="navy", mid="white", na.value="grey50") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black",
                               title.position="top", title.hjust = 0),
         color = guide_legend(title.position="top", title.hjust = 0, ncol=1)) +
  # Theme
  theme_bw() + theme1 +
  theme(legend.position = "top")
g2

# Merge
g <-gridExtra::grid.arrange(g1, g2, nrow=1)
g

# Export figure
ggsave(g, filename=file.path(plotdir, "Fig10_performance_scorecard_wide.png"), 
       width=6.5, height=6.5, units="in", dpi=600)










