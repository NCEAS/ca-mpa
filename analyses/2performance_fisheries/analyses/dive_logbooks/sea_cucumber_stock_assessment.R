
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
plotdir <- "analyses/2performance_fisheries/analyses/dive_logbooks/output/figures" 
outdir <- "analyses/2performance_fisheries/analyses/dive_logbooks/output"

# Read landings data
landings_orig <- wcfish::cdfw_waters


# Read landing receipts
receipts_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/california/cdfw_data/data/confidential/landings_receipts/processed/CDFW_2000_2020_landings_receipts.Rds")


# Analyze landings
################################################################################

# Landings
landings <- landings_orig %>% 
  # Reduce to sea cucumber
  filter(grepl("cucumber", comm_name)) %>% 
  # Recode sea cucumber
  mutate(comm_name=recode(comm_name,
                           "Sea cucumber"="Unspecified sea cucumber"))


# Stats
stats_spp <- landings %>% 
  group_by(year, comm_name) %>% 
  summarize(landings_lb=sum(landings_lb, na.rm=T)) %>% 
  ungroup()

# Plot data
ggplot(stats_spp, aes(x=year, y=landings_lb/1e3, fill=comm_name)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year", y="Landings (thousands of lbs)") +
  scale_fill_discrete(name="Species") +
  # Theme
  theme_bw() +
  theme(legend.position = c(0.8, 0.8),
        legend.key.size = unit(0.3, "cm"))



# Analyze receipts
################################################################################

# Build data
receipts <- receipts_orig %>% 
  # Reduce to sea cucumber
  filter(grepl("cucumber", tolower(species))) %>% 
  # Recode sea cucumber
  mutate(species=recode(species,
                        "Sea cucumber, warty"="Warty sea cucumber",
                        "Sea cucumber, giant red"="Giant red sea cucumber",
                        "Sea cucumber, unspecified"="Unspecified sea cucumber"))

# Stats
stats_spp <- landings %>% 
  group_by(year, species) %>% 
  summarize(landings_lb=sum(landings_lb, na.rm=T)) %>% 
  ungroup()

# Plot data
ggplot(stats_spp, aes(x=year, y=landings_lb/1e3, fill=species)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year", y="Landings (thousands of lbs)") +
  scale_fill_discrete(name="Species") +
  # Theme
  theme_bw() +
  theme(legend.position = c(0.8, 0.8),
        legend.key.size = unit(0.3, "cm"))



