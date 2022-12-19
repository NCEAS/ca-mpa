# Annual Port Landings 
# Cori Lopazanski


# Setup ------------------------------------------------------------------------
# Clear workspace
rm(list = ls())

## Packages ----
library(tidyverse)

## Directories ----
basedir <- "/Volumes/GoogleDrive-105151121202188525604/Shared drives/NCEAS MPA network assessment/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
datadir <- "/Volumes/GoogleDrive-105151121202188525604/My Drive/Research/NCEAS - California MPA Working Group/fisheries-data/raw"
plotdir <- "analyses/2performance_fisheries/figures"
outdir <- "/Volumes/GoogleDrive-105151121202188525604/My Drive/Research/NCEAS - California MPA Working Group/fisheries-data/processed"

## Read Data ----
landings <- readRDS(file.path(datadir, "CDFW_2000_2020_landings_receipts.Rds"))
port_key <- readRDS(file.path(datadir, "CDFW_port_key.Rds"))


# Build ------------------------------------------------------------------------

## Simplify Landings ----
data <- landings %>% 
  filter(block_state == "California") %>% 
  filter(!(port_complex == "Unknown")) %>% 
  select(year, port_complex, port_id, port, block_id, vessel_id, date,
         species_id, species, landings_lb, value_usd)

## Annual Landings per Port ----
# Get 2000-2020
# Average annual landings on the x axis and port on the y axis barplot
# Keep track of the number of vessels contributing to each of those ports
# Consider what percent of the total catch is being contributed by each port 

# Calculate total values per year for vessel at each port
annual_vessel <- data %>% 
  group_by(year, port_complex, port_id, port, vessel_id) %>% 
  dplyr::summarize(vessel_lb = sum(landings_lb, na.rm = T), 
                   vessel_usd = sum(value_usd, na.rm = T),
                   n_receipts = n(),
                   n_days = length(unique(date)),
                   n_species = length(unique(species_id)))

# Calculate total values per year for each port (all vessels)
annual_port <- annual_vessel %>% 
  group_by(year, port_complex, port_id, port) %>% 
  dplyr::summarize(annual_lb = sum(vessel_lb, na.rm = T), # total lb per year
                   annual_usd = sum(vessel_usd, na.rm = T), # total usd per year
                   n_receipts = sum(n_receipts), # total receipts per year
                   n_vessels = length(unique(vessel_id))) # total vessels per year

# Calculate average annual values per port (across all years)
avg_port <- annual_port %>% 
  group_by(port_complex, port_id, port) %>% 
  dplyr::summarize(avg_annual_lb = mean(annual_lb, na.rm = T), # avg lb per year
                   avg_annual_usd = mean(annual_usd, na.rm = T), # avg usd per year
                   avg_receipts = mean(n_receipts, na.rm = T),  # avg receipts per year
                   avg_vessels = mean(n_vessels, na.rm = T))  # avg vessels per year

all_lb = sum(avg_port$avg_annual_lb)
all_usd = sum(avg_port$avg_annual_usd)

 
# Calculate percent each contributes to total value
avg_port2 <- avg_port %>% 
  mutate(pct_total_lb = avg_annual_lb/all_lb*100,
         pct_total_usd = avg_annual_usd/all_usd*100) %>% 
  ungroup()


# Calculate vessel totals (receipts, days, species per vessel)
vessel_totals <- data %>% 
  group_by(port_complex, port_id, port, vessel_id) %>% 
  dplyr::summarize(vessel_lb = sum(landings_lb, na.rm = TRUE),
                   vessel_usd = sum(value_usd, na.rm = TRUE),
                   n_receipts = n(), # receipts per vessel
                   n_days = length(unique(date)), # vessel days
                   n_species = length(unique(species_id))) # vessel species

# Calculate total unique vessels per port (all time)
vessels_per_port <- data %>% 
  group_by(port) %>% 
  dplyr::summarize(num_vessels = length(unique(vessel_id))) 

# Calculate unique species per port (all time)
species_per_port <- data %>% 
  group_by(port) %>% 
  dplyr::summarize(num_species = length(unique(species_id)))

  
# Order and Calculate Cumulative Totals
cumulative_usd <- avg_port2 %>% 
  arrange(-avg_annual_usd) %>% 
  mutate(total = cumsum(pct_total_usd)) %>% 
  mutate(top_99 = case_when(total > 99 ~ "No",
                            total < 99 ~ "Yes"),
         top_985 = case_when(total > 98.5 ~ "No",
                             total < 98.5 ~ "Yes"),
         top_98 = case_when(total > 98 ~ "No",
                            total < 98 ~ "Yes"),
         top_95 = case_when(total > 95 ~ "No",
                            total < 95 ~ "Yes")) %>% 
  left_join(vessels_per_port, by = "port") %>% 
  mutate(port = fct_reorder(port, avg_annual_usd)) 

cumulative_lb <- avg_port2 %>% 
  arrange(-avg_annual_lb) %>% 
  mutate(total = cumsum(pct_total_lb)) %>% 
  mutate(port = fct_reorder(port, avg_annual_lb)) %>% 
  mutate(top_99 = case_when(total > 99 ~ "No",
                            total < 99 ~ "Yes"),
         top_985 = case_when(total > 98.5 ~ "No",
                            total < 98.5 ~ "Yes"),
         top_98 = case_when(total > 98 ~ "No",
                            total < 98 ~ "Yes"),
         top_95 = case_when(total > 95 ~ "No",
                            total < 95 ~ "Yes")) %>% 
  left_join(vessels_per_port, by = "port") %>% 
  mutate(port = fct_reorder(port, avg_annual_lb))

# Count number of ports in different percentages
top_numbers_usd <- cumulative_usd %>% 
  select(port, top_99, top_985, top_98, top_95) 
top_numbers_lb <- cumulative_lb %>% 
  select(port, top_99, top_985, top_98, top_95) 

# Which ports make both groups?
top_port_overlap <- top_numbers_usd %>% 
  full_join(top_numbers_lb, by = "port") %>% 
  filter(if_any(-c(port), ~ . == "Yes"))

overlap_long <- top_port_overlap %>% 
  pivot_longer(cols = top_99.x:top_95.y,
               names_to = "percentile",
               values_to = "status")

# Plot -------------------------------------------------------------------------
d1 <- ggplot(data = cumulative_usd) +
  geom_col(aes(x = avg_annual_usd, y = port, fill = top_99),
           show.legend = F) +
  labs(x = "Average Annual Revenues \n(USD)",
       y = "Port") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))

d2 <- ggplot(data = cumulative_usd) +
  geom_col(aes(x = total, y = port, fill = top_99),
           show.legend = F) +
  theme_minimal() +
  labs(x = "Cumulative Percent of\nTotal Annual Revenues (USD)",
       y = NULL) +
  theme_minimal() +
  theme(axis.text.y=element_blank())

d3 <- ggplot(data = cumulative_usd) +
  geom_col(aes(x = num_vessels, y = port, fill = top_99),
           show.legend = F) +
  theme_minimal() +
  labs(x = "Number of Vessels \n(all time)",
       y = NULL)+
  theme_minimal() +
  theme(axis.text.y=element_blank())

d4 <-  ggplot(data = cumulative_usd) +
  geom_col(aes(x = avg_vessels, y = port, fill = top_99)) +
  theme_minimal() +
  labs(x = "Number of Vessels \n(average per year)",
       y = NULL,
       fill = "Does the port\ncontribute to\ntop 99%total \nannual revenues?")+
  theme_minimal() +
  theme(axis.text.y=element_blank())

gridExtra::grid.arrange(d1, d2, d3, d4, nrow = 1)

## Overlap ----

ggplot(data = overlap_long) +
  geom_tile(aes(x = percentile, y = port, fill = status),
            color = "white",
            lwd = 1.5,
            linetype = 1) +
  theme_minimal()

## Landings ----

lb1 <- ggplot(data = cumulative_lb) +
  geom_col(aes(x = avg_annual_lb, y = port, fill = top_99),
           show.legend = F) +
  labs(x = "Average Annual Landings (lbs)") +
  theme_minimal()

lb2 <- ggplot(data = cumulative_lb) +
  geom_col(aes(x = total, y = port, fill = top_99),
           show.legend = F) +
  theme_minimal() +
  labs(x = "Cumulative Percent of Total Annual Landing (lbs)") +
  theme(axis.text.y=element_blank())

lb3 <- ggplot(data = cumulative_lb) +
  geom_col(aes(x = num_vessels, y = port, fill = top_99)) +
  theme_minimal() +
  labs(x = "Number of Vessels Contributing to Annual Landings",
       fill = "Does the port\ncontribute to\ntop 98%\ntotal annual landings")+
  theme(axis.text.y=element_blank())

gridExtra::grid.arrange(lb1, lb2, lb3, nrow = 1)

# Export ----
saveRDS(cumulative_lb, file.path(outdir, "annual_landings_per_port_lb.Rds"))
