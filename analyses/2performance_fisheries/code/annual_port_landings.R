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
  select(year, port_complex, port_id, port, block_id, 
         species_id, species, landings_lb, value_usd)

## Annual Landings per Port ----
annual_port <- data %>% 
  group_by(year, port_complex, port_id, port) %>% 
  dplyr::summarize(total_lb = sum(landings_lb, na.rm = TRUE),
                   total_usd = sum(value_usd, na.rm = TRUE)) %>% 
  arrange(-total_lb) %>% 
  filter(!(port_complex %in% c("Inland Waters", "Unknown"))) #these are zero
 
## Totals for All Years ----
all_port <- data %>% 
  group_by(port_complex, port_id, port) %>% 
  dplyr::summarize(total_lb = sum(landings_lb, na.rm = TRUE),
                   total_usd = sum(value_usd, na.rm = TRUE))

# Plot -------------------------------------------------------------------------

## Scatterplots ----

### Annual Totals ----
g1 <- ggplot() +
  geom_point(data = annual_port, 
             aes(x = year, y = total_lb, color = port_complex),
             show.legend = FALSE) +
  labs(title = "A. Total Annual Landings (lbs)") +
  theme_classic()

g2 <- ggplot() +
  geom_point(data = annual_port, 
             aes(x = year, y = total_usd, color = port_complex)) +
  labs(title = "B. Total Annual Revenue (USD)")+
  theme_classic()

g <- gridExtra::grid.arrange(g1, g2, nrow = 1)

### Facet By Port Complex ----
ggplot() +
  geom_point(data = annual_port, 
             aes(x = year, y = total_lb, color = port), 
             show.legend = FALSE) +
  facet_wrap(~port_complex)+
  labs(title = "Annual Total Landings (lbs) by Port Complex") +
  theme_minimal()

ggplot() +
  geom_point(data = annual_port, 
             aes(x = year, y = total_usd, color = port), 
             show.legend = FALSE) +
  facet_wrap(~port_complex)+
  labs(title = "Annual Total Revenues (USD) by Port Complex") +
  theme_minimal()

## Histograms ----

### Annual ----
h1 <- ggplot() + 
  geom_histogram(data = annual_port,
                 aes(x = total_lb)) +
  labs(title = "A. Total Annual Landings (lbs)",
       ylab = "number of ports") +
  theme_minimal()

h2 <- ggplot() + 
  geom_histogram(data = annual_port,
                 aes(x = total_usd)) +
  labs(title = "B. Total Annual Revenue (USD)",
       ylab = "number of ports") +
  theme_minimal()

h <- gridExtra::grid.arrange(h1, h2, nrow = 1)

### All Years ----
ggplot() + 
  geom_histogram(data = all_port,
                 aes(x = total_lb))
ggplot() + 
  geom_histogram(data = all_port,
                 aes(x = total_usd))

### Lower Tails ----

#### Landings
l1 <- ggplot() +
  geom_histogram(data = annual_port %>% 
                   filter(total_lb < 100000),
                 aes(x = total_lb))

l2 <- ggplot() +
  geom_histogram(data = annual_port %>% 
                   filter(total_lb < 25000),
                 aes(x = total_lb))

l3 <- ggplot() +
  geom_histogram(data = annual_port %>% 
                   filter(total_lb < 15000),
                 aes(x = total_lb))

l <- gridExtra::grid.arrange(l1, l2, l3, nrow = 3)

#### Revenues
r1 <- ggplot() +
  geom_histogram(data = annual_port %>% 
                   filter(total_usd < 100000),
                 aes(x = total_usd))

r2 <- ggplot() +
  geom_histogram(data = annual_port %>% 
                   filter(total_usd < 50000),
                 aes(x = total_usd))

r3 <- ggplot() +
  geom_histogram(data = annual_port %>% 
                   filter(total_usd < 20000),
                 aes(x = total_usd))

r <- gridExtra::grid.arrange(r1, r2, r3, nrow = 3)

