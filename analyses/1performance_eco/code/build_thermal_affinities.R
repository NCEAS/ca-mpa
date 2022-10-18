
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
plotdir <- "analyses/1performance_eco/figures"

# Read species key
spp_key_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/california/cdfw_data/data/public/cdfw_keys/processed/CDFW_species_key_taxa.Rds")


# Build data
################################################################################

# Reduce to species-specific
spp_key <- spp_key_orig %>% 
  filter(level=="species")

# Get FishLife values
spp_key_fl <- freeR::fishlife(species = spp_key$sci_name)

# Get FishBase values
spp_key_fb <- freeR::fishbase(dataset = "vonb", species = spp_key$sci_name, add_taxa=F)
colnames(spp_key_fb)

# Process FishBase values
spp_key_fb_avg <- spp_key_fb %>% 
  rename(sci_name=Species, temp_c_fb=Temperature) %>% 
  filter(!is.na(temp_c_fb)) %>% 
  group_by(sci_name) %>% 
  summarize(n=n(),
            temp_c_fb=median(temp_c_fb))

# Merge data
data <- spp_key %>% 
  select(comm_name, sci_name) %>% 
  unique() %>% 
  # Add FishLife temp
  left_join(spp_key_fl %>% select(species, temp_c), by=c("sci_name"="species")) %>% 
  rename(temp_c_fl=temp_c) %>% 
  # Add FishBase temp
  left_join(spp_key_fb_avg %>% select(sci_name, temp_c_fb), by=c("sci_name")) %>% 
  # Arrange
  rowwise() %>% 
  mutate(temp_c_avg=mean(c(temp_c_fb, temp_c_fl), na.rm=T)) %>% 
  ungroup() %>%
  arrange(temp_c_avg)

# Check common name
freeR::which_duplicated(data$comm_name)


# Plot data
################################################################################

# FB vs FL
ggplot(data, aes(x=temp_c_fb, y=temp_c_fl)) +
  geom_point() +
  # Labels
  labs(x="Temperature (°C) - FishBase", y="Temperature (°C) - FishLife") +
  # Theme
  theme_bw()

# Make long
data_wide <- data %>% 
  select(comm_name, sci_name, temp_c_fb, temp_c_fl) %>% 
  gather(key="source", value="temp_c", 3:4) %>% 
  mutate(comm_name=factor(comm_name, levels=data$comm_name))

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
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data_wide, aes(y=comm_name, x=temp_c, color=source)) +
  geom_point() +
  # Labels
  labs(x="Temperature (°C)", y="") +
  scale_x_continuous(breaks=seq(0,30,5)) +
  scale_color_discrete(name="Source") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.2, 0.8))

# Export
ggsave(g, filename=file.path(plotdir, "FigX_thermal_affinities.png"), 
       width=6.5, height=8.5, units="in", dpi=600)










