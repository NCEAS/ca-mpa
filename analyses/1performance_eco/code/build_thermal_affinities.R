
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Read species key
spp_key_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/california/cdfw_data/data/public/cdfw_keys/processed/CDFW_species_key.Rds")


#
################################################################################

# Reduce to species-specific
spp_key <- spp_key_orig %>% 
  filter(level=="species")

# Get taxa info
spp_key_taxa <- freeR::taxa(species = spp_key$sci_name)
spp_key_taxa1 <- spp_key_taxa %>% 
  filter(!is.na(class))
freeR::which_duplicated(spp_key_taxa1$sciname)

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
  # Add taxa info
  left_join(spp_key_taxa1, by=c("sci_name"="sciname")) %>% 
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


freeR::which_duplicated(data$comm_name)

# FB vs FL
ggplot(data, aes(x=temp_c_fb, y=temp_c_fl)) +
  geom_point() +
  # Labels
  labs(x="Temperature (°C) - FishBase", y="Temperature (°C) - FishLife") +
  # Theme
  theme_bw()

# Make long
data_wide <- data %>% 
  select(order, comm_name, sci_name, temp_c_fb, temp_c_fl) %>% 
  gather(key="source", value="temp_c", 4:5) %>% 
  mutate(comm_name=factor(comm_name, levels=data$comm_name))

# Plot data
ggplot(data_wide, aes(y=comm_name, x=temp_c, color=source)) +
  geom_point() +
  # Labels
  labs(x="Temperature (°C)") +
  # Theme
  theme_bw()



