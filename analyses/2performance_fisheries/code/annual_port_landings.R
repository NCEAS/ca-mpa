# Annual Port Landings 
# Cori Lopazanski


# Setup ------------------------------------------------------------------------

## Packages ----
library(tidyverse)

## Directories ----
datadir <- "/Volumes/GoogleDrive-105151121202188525604/My Drive/Research/NCEAS - California MPA Working Group/fisheries-data/raw"


## Read Data ----
landings <- readRDS(file.path(datadir, "CDFW_2000_2020_landings_receipts.Rds"))
port_key <- readRDS(file.path(datadir, "CDFW_port_key.Rds"))


# Build ------------------------------------------------------------------------

## Simplify Landings ----
data <- landings %>% 
  filter(block_state == "California") %>% 
  select(year, port_complex, port_id, port, block_id, 
         species_id, species, landings_lb, value_usd)

## Annual Landings per Port ----
annual_port <- data %>% 
  group_by(year, port_complex, port_id, port) %>% 
  summarize(total_lb = sum(landings_lb),
            total_usd = sum(value_usd)) %>% 
  arrange(-total_lb)


# Plot -------------------------------------------------------------------------

ggplot() +
  geom_point(data = annual_port %>% 
               filter(!(port_complex %in% c("Inland Waters", "Unknown"))), 
             aes(x = year, y = total_lb, color = port_complex))

            