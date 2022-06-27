# Annual Port Landings 
# Cori Lopazanski


# Setup ------------------------------------------------------------------------
# Clear workspace
rm(list = ls())

## Packages ----
library(tidyverse)

## Directories ----
datadir <- "/Volumes/GoogleDrive-105151121202188525604/My Drive/Research/NCEAS - California MPA Working Group/fisheries-data/raw"
plotdir <- "analyses/2performance_fisheries/figures"

## Read Data ----
landings <- readRDS(file.path(datadir, "CDFW_2000_2020_landings_receipts.Rds"))
port_key <- readRDS(file.path(datadir, "CDFW_port_key.Rds"))


# Build ------------------------------------------------------------------------

## Simplify Landings ----
data <- landings %>% 
  filter(block_state == "California") %>% 
  select(year, port_complex, port_id, port, block_id, vessel_id, date,
         species_id, species, landings_lb, value_usd)

## Annual Landings per Port ----
# Get 2000-2020
# Average annual landings on the x axis and port on the y axis barplot
# Keep track of the number of vessels contributing to each of those ports
# Consider what percent of the total catch is being contributed by each port 

# Calculate total landing per year for vessel at each port
annual_vessel <- data %>% 
  group_by(year, port_complex, port_id, port, vessel_id) %>% 
  dplyr::summarize(vessel_lb = sum(landings_lb, na.rm = TRUE),
                   vessel_usd = sum(value_usd, na.rm = TRUE),
                   n_receipts = n(),
                   n_days = length(unique(date)),
                   n_species = length(unique(species_id)))

# Calculate total landing per year for each port (all vessels)
annual_port <- annual_vessel %>% 
  group_by(year, port_complex, port_id, port) %>% 
  dplyr::summarize(annual_lb = sum(vessel_lb, na.rm = TRUE),
                   annual_usd = sum(vessel_usd, na.rm = TRUE),
                   n_receipts = sum(n_receipts),
                   n_vessels = length(unique(vessel_id)))

# Calculate average annual landing per port
avg_port <- annual_port %>% 
  group_by(port_complex, port_id, port) %>% 
  dplyr::summarize(avg_annual_lb = mean(annual_lb, na.rm = TRUE),
                   avg_annual_usd = mean(annual_usd, na.rm = TRUE),
                   total_receipts = sum(n_receipts, na.rm = TRUE),
                   total_vessels = sum(n_vessels, na.rm = TRUE)) %>% 
  filter(!(port_complex == "Unknown")) %>% 
  mutate(pct_total_lb = avg_annual_lb/sum(avg_port$avg_annual_lb)*100,
         pct_total_usd = avg_annual_usd/sum(avg_port$avg_annual_usd)*100) %>% 
  ungroup()

# Calculate total vessels per port
total_vessels <- data %>% 
  

# Order and Calculate Cumulative Totals
cumulative_usd <- avg_port %>% 
  arrange(-avg_annual_usd) %>% 
  mutate(total = cumsum(pct_total_usd)) %>% 
  mutate(port = fct_reorder(port, avg_annual_usd)) %>% 
  mutate(top_95 = case_when(total > 95 ~ "No",
                            total < 95 ~ "Yes"),
         top_98 = case_when(total > 98 ~ "No",
                            total < 98 ~ "Yes"))

cumulative_lb <- avg_port %>% 
  arrange(-avg_annual_lb) %>% 
  mutate(total = cumsum(pct_total_lb)) %>% 
  mutate(port = fct_reorder(port, avg_annual_lb)) %>% 
  mutate(top_95 = case_when(total > 95 ~ "No",
                            total < 95 ~ "Yes"),
         top_98 = case_when(total > 98 ~ "No",
                            total < 98 ~ "Yes"))

# Plot -------------------------------------------------------------------------
usd1 <- ggplot(data = cumulative_usd) +
  geom_col(aes(x = avg_annual_usd, y = port, fill = top_98)) +
  theme_minimal()

usd2 <- ggplot(data = cumulative_usd) +
  geom_col(aes(x = total, y = port, fill = top_98)) +
  theme_minimal()

gridExtra::grid.arrange(usd1, usd2, nrow = 1)

lb1 <- ggplot(data = cumulative_lb) +
  geom_col(aes(x = avg_annual_lb, y = port, fill = top_98),
           show.legend = F) +
  labs(x = "Average Annual Landings (lbs)")
  theme_minimal()

lb2 <- ggplot(data = cumulative_lb) +
  geom_col(aes(x = total, y = port, fill = top_98),
           show.legend = F) +
  theme_minimal() +
  labs(x = "Cumulative Percent of Total Annual Landing (lbs)")
  theme(axis.text.y=element_blank())

lb3 <- ggplot(data = cumulative_lb) +
  geom_col(aes(x = total_vessels, y = port, fill = top_98)) +
  theme_minimal() +
  labs(x = "Number of Vessels Contributing to Annual Landings",
       fill = "Does the port\ncontribute to\ntop 98%\ntotal annual landings")+
  theme(axis.text.y=element_blank())

gridExtra::grid.arrange(lb1, lb2, lb3, nrow = 1)

