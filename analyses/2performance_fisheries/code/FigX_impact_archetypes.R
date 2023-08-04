
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
datadir <- "analyses/2performance_fisheries/data"
plotdir <- "analyses/2performance_fisheries/figures"

# Read data
data_orig <- readxl::read_excel(file.path(datadir, "archetype_data.xlsx"))


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Gather
  gather(key="scenario", value="mpa", 3:ncol(.)) %>%
  # Arrange
  select(scenario, year, control, mpa, everything()) %>% 
  # Gather again
  gather(key="block_type", value="landings", 3:ncol(.)) %>% 
  # Format block type
  mutate(block_type=recode(block_type,
                           "mpa"="MPA",
                           "control"="Control")) %>% 
  # Format scenario
  mutate(scenario=recode_factor(scenario,
                                "No impact"="No impact",
                                "Discplacement"="Displacement",
                                "Preemptive"="Preemptive fishing\nthen displacement",
                                "Spillover"="Preemptive fishing,\ndisplacement, then spillover"))

# Wrangle data
data1 <- data %>% 
  spread(key="block_type", value="landings") %>% 
  mutate(diff=MPA-Control)

  


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   plot.tag = element_text(size=7),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=7),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot(data, aes(x=year, y=landings, color=block_type)) +
  facet_wrap(~scenario, nrow=1) +
  # Reference polygon
  geom_rect(xmin=2008, xmax=2010, ymin=-Inf, ymax=Inf, fill="grey90", color=NA) +
  # Reference line
  geom_vline(xintercept=2010, linetype="dotted") +
  # Data
  geom_line() +
  # Labels
  labs(x="Year", y="Landings", tag="A") +
  scale_y_continuous(lim=c(0, NA)) +
  # Legend
  scale_color_discrete(name="Block type") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.18, 0.2),
        legend.key.size = unit(0.3, "cm"))
g1

# Labels
labels <- tibble(scenario=factor("No impact", levels=levels(data$scenario)), 
                 year=c(2020,2020),
                 diff=range(data1$diff),
                 vjust=c(0,1),
                 label=c("Lower\nin MPA block", "Higher\nin MPA block"))

# Plot data
g2 <- ggplot(data1, aes(x=year, y=diff)) +
  facet_wrap(~scenario, nrow=1) +
  # Reference polygon
  geom_rect(xmin=2008, xmax=2010, ymin=-Inf, ymax=Inf, fill="grey90", color=NA) +
  # Reference line
  geom_vline(xintercept=2010, linetype="dotted") +
  geom_hline(yintercept=0, linetype="dotted") +
  # Data
  geom_line() +
  # Plot labels
  geom_text(data=labels, 
            mapping=aes(x=year, y=diff, label=label, vjust=vjust), 
            col="grey30", size=2, hjust=1) +
  # Labels
  labs(x="Year", y="Difference in landings", tag="B") +
  # Theme
  theme_bw() + my_theme
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=2)
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_impact_archetypes.png"), 
       width=6.5, height=4, units="in", dpi=600)




