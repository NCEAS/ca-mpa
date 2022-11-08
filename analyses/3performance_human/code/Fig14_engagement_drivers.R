
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
load(file.path(datadir, "model_output.Rdata"))

# Format data
under_dat <- under_dat %>% mutate(logit_y=as.numeric(logit_y))
char_dat <- char_dat %>% mutate(logit_y=as.numeric(logit_y))



# Build data
################################################################################

# MPA age (yr)
x_age <- seq(0,20,0.5)
y_age <- predict(char_step_mod, 
                 newdata = tibble(distance_to_port=mean(char_dat$distance_to_port, na.rm=T),
                                  size_km2=mean(char_dat$size_km2, na.rm=T),
                                  take="yes",
                                  sandy_beach_km=mean(char_dat$sandy_beach_km, na.rm=T),
                                  mpa_age=x_age, # mean(char_dat$mpa_age, na.rm=T),
                                  nparks=mean(char_dat$nparks, na.rm=T)),
                 type="response",
                 se.fit = TRUE
)
df_age <- tibble(variable="MPA age (yr)",
                 x=x_age,
                 y=y_age$fit,
                 y_se=y_age$fit)  

# Sandy beach (km)
x_beach <- seq(0,20,0.5)
y_beach <- predict(char_step_mod, 
                   newdata = tibble(distance_to_port=mean(char_dat$distance_to_port, na.rm=T),
                                    size_km2=mean(char_dat$size_km2, na.rm=T),
                                    take="yes",
                                    sandy_beach_km=x_beach, # mean(char_dat$sandy_beach_km, na.rm=T),
                                    mpa_age=mean(char_dat$mpa_age, na.rm=T),
                                    nparks=mean(char_dat$nparks, na.rm=T)),
                   type="response"
)
df_beach <- tibble(variable="Sandy beach (km)",
                   x=x_beach,
                   y=y_beach) 

# Parks
x_parks <- seq(0,16,0.5)
y_parks <- predict(char_step_mod, 
                   newdata = tibble(distance_to_port=mean(char_dat$distance_to_port, na.rm=T),
                                    size_km2=mean(char_dat$size_km2, na.rm=T),
                                    take="yes",
                                    sandy_beach_km=mean(char_dat$sandy_beach_km, na.rm=T),
                                    mpa_age=mean(char_dat$mpa_age, na.rm=T),
                                    nparks=x_parks), #mean(char_dat$nparks, na.rm=T)),
                   type="response"
)
df_parks <- tibble(variable="Number of parks",
                   x=x_parks,
                   y=y_parks) 


# Underutilized
#############################

# Distance to port
x_ports <- seq(0, 85, 0.5) * 1000
y_ports <- predict(under_step_mod, 
                   newdata = tibble(distance_to_port=x_ports, #mean(under_dat$distance_to_port, na.rm=T),
                                    sandy_beach_km=mean(under_dat$sandy_beach_km, na.rm=T),
                                    rocky_inter_km=mean(under_dat$rocky_inter_km, na.rm=T),
                                    n_parking_lots=mean(under_dat$n_parking_lots, na.rm=T)),
                   type="response"
)
df_ports <- tibble(variable="Distance to port (km)",
                   x=x_ports,
                   y=y_ports)  

# Sandy beach (km)
x_beach <- seq(0,20,0.5)
y_beach2 <- predict(under_step_mod, 
                    newdata = tibble(distance_to_port=mean(under_dat$distance_to_port, na.rm=T),
                                     sandy_beach_km=x_beach, # mean(under_dat$sandy_beach_km, na.rm=T),
                                     rocky_inter_km=mean(under_dat$rocky_inter_km, na.rm=T),
                                     n_parking_lots=mean(under_dat$n_parking_lots, na.rm=T)),
                    type="response"
)
df_beach2 <- tibble(variable="Sandy beach (km)",
                    x=x_beach,
                    y=y_beach2) 

# Parking lots
x_lots <- seq(0,20,0.5)
y_lots <- predict(under_step_mod, 
                  newdata = tibble(distance_to_port=mean(under_dat$distance_to_port, na.rm=T),
                                   sandy_beach_km=mean(under_dat$sandy_beach_km, na.rm=T),
                                   rocky_inter_km=mean(under_dat$rocky_inter_km, na.rm=T),
                                   n_parking_lots=x_lots), # mean(under_dat$n_parking_lots, na.rm=T)),
                  type="response"
)
df_lots <- tibble(variable="Number of parking lots",
                  x=x_lots,
                  y=y_lots) 



# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_text(size=8),
                   plot.tag=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_blank(),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot
range(char_dat$mpa_age)
g1 <- ggplot(df_age, mapping=aes(x=x, y=y)) +
  # Median
  geom_line() +
  # Plot rug
  geom_jitter(data=char_dat, mapping=aes(x=mpa_age, y=logit_y), pch=1, width=0.2, height=0.05, size=1) +
  # Labels
  labs(x="MPA age (yr)\n", y="P(charismatic)", tag="A") +
  # Scale
  scale_y_continuous(lim=c(0,1)) +
  # Theme
  theme_bw() + my_theme
g1

# Plot
range(char_dat$sandy_beach_km)
g2 <- ggplot(df_beach, mapping=aes(x=x, y=y)) +
  # Median
  geom_line() +
  # Plot rug
  geom_jitter(data=char_dat, mapping=aes(x=sandy_beach_km, y=logit_y), pch=1, width=0.2, height=0.05, size=1) +
  # Labels
  labs(x="Sandy beach (km)\n", y="P(charismatic)", tag="B") +
  # Scale
  scale_y_continuous(lim=c(0,1)) +
  # Theme
  theme_bw() + my_theme
g2

# Plot
range(char_dat$nparks)
g3 <- ggplot(df_parks, mapping=aes(x=x, y=y)) +
  # Median
  geom_line() +
  # Plot rug
  geom_jitter(data=char_dat, mapping=aes(x=nparks, y=logit_y), pch=1, width=0.2, height=0.05, size=1) +
  # Labels
  labs(x="Number of parks\nwithin 1 km", y="P(charismatic)", tag="C") +
  # Scale
  scale_y_continuous(lim=c(0,1)) +
  # Theme
  theme_bw() + my_theme
g3

# Plot
range(under_dat$distance_to_port) /1000
g4 <- ggplot(df_ports, mapping=aes(x=x/1000, y=y)) +
  # Median
  geom_line() +
  # Plot rug
  geom_jitter(data=under_dat, mapping=aes(x=distance_to_port/1000, y=logit_y), pch=1, width=0.2, height=0.05, size=1) +
  # Labels
  labs(x="Distance to port (km)\n", 
       y="P(underutilized)", tag="D") +
  # Scale
  scale_y_continuous(lim=c(0,1)) +
  # Theme
  theme_bw() + my_theme
g4

# Plot
range(under_dat$sandy_beach_km)
g5 <- ggplot(df_beach2, mapping=aes(x=x, y=y)) +
  # Median
  geom_line() +
  # Plot rug
  geom_jitter(data=under_dat, mapping=aes(x=sandy_beach_km, y=logit_y), pch=1, width=0.2, height=0.05, size=1) +
  # Labels
  labs(x="Sandy beach (km)\n", 
       y="P(underutilized)", tag="E") +
  # Scale
  scale_y_continuous(lim=c(0,1)) +
  # Theme
  theme_bw() + my_theme
g5

# Plot
range(under_dat$n_parking_lots)
g6 <- ggplot(df_lots, mapping=aes(x=x, y=y)) +
  # Median
  geom_line() +
  # Plot rug
  geom_jitter(data=under_dat, mapping=aes(x=n_parking_lots, y=logit_y), pch=1, width=0.2, height=0.05, size=1) +
  # Labels
  labs(x="Number of parking lots\nwithin 1 km", 
       y="P(underutilized)", tag="F") +
  # Scale
  scale_y_continuous(lim=c(0,1)) +
  # Theme
  theme_bw() + my_theme
g6

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, g3, 
                             g4, g5, g6, nrow=2)
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_engagement_drivers.png"), 
       width=6.5, height=4, units="in", dpi=600)


