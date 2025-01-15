
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Read data
data_orig <- readxl::read_excel("analyses/1performance_eco/output/species_key.xlsx")


# Format data
################################################################################

# Format data
data1 <- data_orig %>% 
  # Add sci name
  mutate(sciname_orig=paste(genus, species)) %>% 
  select(-c(genus, species)) %>% 
  # Correct a few names
  mutate(sciname=recode(sciname_orig,
                        "Clupea pallasii"="Clupea pallasii pallasii",     
                        "Kyphosus azureus"="Hermosilla azurea",           
                        "Pseudobatos productus"="Rhinobatos productus",    
                        "Sarda chiliensis chiliensis"="Sarda chiliensis",
                        "Tetronarce californica"="Torpedo californica",    
                        "Urobatis halleri"="Urolophus halleri"))

# Check names
freeR::check_names(data1$sciname)

# Get taxa
data_taxa <- freeR::taxa(data1$sciname)

# Get common names
data_comm <- freeR::fb_comm_name(data1$sciname)

# Merge data
data2 <- data1 %>% 
  # Add taxa
  left_join(data_taxa %>% select(class, order, family, sciname), by="sciname") %>% 
  # Add common name
  left_join(data_comm %>% select(species, comm_name), by=c("sciname"="species")) %>% 
  mutate(comm_name=case_when(sciname=="Hermosilla azurea" ~  "Zebra-perch sea chub",
                             sciname=="Urolophus halleri" ~ "Round stingray",  
                             sciname=="Raja binoculata" ~ "Big skate",
                             sciname=="Raja rhina" ~  "Longnose skate",
                             sciname=="Raja stellulata" ~ "Pacific starry skate",  
                             sciname=="Rhinobatos productus" ~"Shovelnose guitarfish", 
                             sciname=="Torpedo californica" ~ "Pacific electric ray",
                             T ~ comm_name)) %>% 
  # Add label
  mutate(label=paste0(comm_name, " (", sciname, ")")) %>% 
  # Arrange
  select(class, order, family, sciname, sciname_orig, comm_name, label, status, everything()) %>% 
  # Gather
  gather(key="ecosystem", value="value", 9:ncol(.)) %>% 
  mutate(ecosystem=recode(ecosystem,
                          "kelp_forest"="Kelp\nforest",
                          "deep_reef"="Deep\nreef",
                          "shallow_reef"="Shallow\nreef",
                          "surf_zone"="Surf/nzone")) %>% 
  # Reduce
  filter(!is.na(value))



# Plot data
################################################################################

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
                   legend.position = "none",
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Data
data2a <- data2 %>% 
  filter(order %in% c("Perciformes", "Scorpanoformes"))

# Plot data
g1 <- ggplot(data2a, mapping=aes(x=ecosystem, y=label, fill=ecosystem)) +
  facet_grid(family~., space="free_y", scales="free_y") +
  geom_tile() +
  # Label
  labs(x="Ecosystem", y="") +
  # Theme
  theme_bw() + my_theme 
g1



