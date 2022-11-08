
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

# Link function
link <- family(under_step_mod)$linkinv

# MPA age (yr)
x_age <- seq(0,20,0.5)
y_age <- predict(char_step_mod, 
                 newdata = tibble(distance_to_port=mean(char_dat$distance_to_port, na.rm=T),
                                  size_km2=mean(char_dat$size_km2, na.rm=T),
                                  take="yes",
                                  sandy_beach_km=mean(char_dat$sandy_beach_km, na.rm=T),
                                  mpa_age=x_age, # mean(char_dat$mpa_age, na.rm=T),
                                  nparks=mean(char_dat$nparks, na.rm=T)),
                 type="link",
                 se.fit = TRUE
)
df_age <- tibble(variable="MPA age (yr)",
                 x=x_age,
                 fit_link=y_age$fit,
                 se_link=y_age$se.fit,
                 y=link(fit_link),
                 y_lo=link(fit_link - (1.96 * se_link)),
                 y_hi=link(fit_link + (1.96 * se_link)))  

# Sandy beach (km)
x_beach <- seq(0,20,0.5)
y_beach <- predict(char_step_mod, 
                   newdata = tibble(distance_to_port=mean(char_dat$distance_to_port, na.rm=T),
                                    size_km2=mean(char_dat$size_km2, na.rm=T),
                                    take="yes",
                                    sandy_beach_km=x_beach, # mean(char_dat$sandy_beach_km, na.rm=T),
                                    mpa_age=mean(char_dat$mpa_age, na.rm=T),
                                    nparks=mean(char_dat$nparks, na.rm=T)),
                   type="link",
                   se.fit = TRUE
)
df_beach <- tibble(variable="Sandy beach (km)",
                   x=x_beach,
                   fit_link=y_beach$fit,
                   se_link=y_beach$se.fit,
                   y=link(fit_link),
                   y_lo=link(fit_link - (1.96 * se_link)),
                   y_hi=link(fit_link + (1.96 * se_link))) 

# Parks
x_parks <- seq(0,16,0.5)
y_parks <- predict(char_step_mod, 
                   newdata = tibble(distance_to_port=mean(char_dat$distance_to_port, na.rm=T),
                                    size_km2=mean(char_dat$size_km2, na.rm=T),
                                    take="yes",
                                    sandy_beach_km=mean(char_dat$sandy_beach_km, na.rm=T),
                                    mpa_age=mean(char_dat$mpa_age, na.rm=T),
                                    nparks=x_parks), #mean(char_dat$nparks, na.rm=T)),
                   type="link",
                   se.fit = TRUE
)
df_parks <- tibble(variable="Number of parks",
                   x=x_parks,
                   fit_link=y_parks$fit,
                   se_link=y_parks$se.fit,
                   y=link(fit_link),
                   y_lo=link(fit_link - (1.96 * se_link)),
                   y_hi=link(fit_link + (1.96 * se_link))) 


# Underutilized
#############################

# Distance to port
x_ports <- seq(0, 85, 0.5) * 1000
y_ports <- predict(under_step_mod, 
                   newdata = tibble(distance_to_port=x_ports, #mean(under_dat$distance_to_port, na.rm=T),
                                    sandy_beach_km=mean(under_dat$sandy_beach_km, na.rm=T),
                                    rocky_inter_km=mean(under_dat$rocky_inter_km, na.rm=T),
                                    n_parking_lots=mean(under_dat$n_parking_lots, na.rm=T)),
                   type="link",
                   se.fit = TRUE
)
df_ports <- tibble(variable="Distance to port (km)",
                   x=x_ports,
                   fit_link=y_ports$fit,
                   se_link=y_ports$se.fit,
                   y=link(fit_link),
                   y_lo=link(fit_link - (1.96 * se_link)),
                   y_hi=link(fit_link + (1.96 * se_link)))  

# Sandy beach (km)
x_beach <- seq(0,20,0.5)
y_beach2 <- predict(under_step_mod, 
                    newdata = tibble(distance_to_port=mean(under_dat$distance_to_port, na.rm=T),
                                     sandy_beach_km=x_beach, # mean(under_dat$sandy_beach_km, na.rm=T),
                                     rocky_inter_km=mean(under_dat$rocky_inter_km, na.rm=T),
                                     n_parking_lots=mean(under_dat$n_parking_lots, na.rm=T)),
                    type="link",
                    se.fit = TRUE
)
df_beach2 <- tibble(variable="Sandy beach (km)",
                    x=x_beach,
                    fit_link=y_beach2$fit,
                    se_link=y_beach2$se.fit,
                    y=link(fit_link),
                    y_lo=link(fit_link - (1.96 * se_link)),
                    y_hi=link(fit_link + (1.96 * se_link))) 

# Parking lots
x_lots <- seq(0,20,0.5)
y_lots <- predict(under_step_mod, 
                  newdata = tibble(distance_to_port=mean(under_dat$distance_to_port, na.rm=T),
                                   sandy_beach_km=mean(under_dat$sandy_beach_km, na.rm=T),
                                   rocky_inter_km=mean(under_dat$rocky_inter_km, na.rm=T),
                                   n_parking_lots=x_lots), # mean(under_dat$n_parking_lots, na.rm=T)),
                  type="link",
                  se.fit = TRUE
)
df_lots <- tibble(variable="Number of parking lots",
                  x=x_lots,
                  fit_link=y_lots$fit,
                  se_link=y_lots$se.fit,
                  y=link(fit_link),
                  y_lo=link(fit_link - (1.96 * se_link)),
                  y_hi=link(fit_link + (1.96 * se_link))) 



# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_text(size=8),
                   plot.tag=element_blank(), #element_text(size=8),
                   plot.title =element_text(size=8, face="bold"),
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
  # Plot CI
  geom_ribbon(mapping=aes(x=x, ymin=y_lo, ymax=y_hi), fill="grey90") +
  # Median
  geom_line() +
  # Plot rug
  # geom_jitter(data=char_dat, mapping=aes(x=mpa_age, y=logit_y), pch=1, width=0.2, height=0.05, size=1) +
  # Plot p-value
  annotate(geom="text", x=0, y=1, label="p=0.007", hjust=0, size=2.2) +
  # Labels
  labs(x="MPA age (yr)\n", y="P(charismatic)", tag="A", title="Charismatic MPAs") +
  # Scale
  scale_y_continuous(lim=c(0,1)) +
  # Theme
  theme_bw() + my_theme
g1

# Plot
range(char_dat$sandy_beach_km)
g2 <- ggplot(df_beach, mapping=aes(x=x, y=y)) +
  # Plot CI
  geom_ribbon(mapping=aes(x=x, ymin=y_lo, ymax=y_hi), fill="grey90") +
  # Median
  geom_line() +
  # Plot rug
  # geom_jitter(data=char_dat, mapping=aes(x=sandy_beach_km, y=logit_y), pch=1, width=0.2, height=0.05, size=1) +
  # Plot p-value
  annotate(geom="text", x=0, y=1, label="p=0.022", hjust=0, size=2.2) +
  # Labels
  labs(x="Sandy beach (km)\n", y="P(charismatic)", tag="B", title=" ") +
  # Scale
  scale_y_continuous(lim=c(0,1)) +
  # Theme
  theme_bw() + my_theme
g2

# Plot
range(char_dat$nparks)
g3 <- ggplot(df_parks, mapping=aes(x=x, y=y)) +
  # Plot CI
  geom_ribbon(mapping=aes(x=x, ymin=y_lo, ymax=y_hi), fill="grey90") +
  # Median
  geom_line() +
  # Plot rug
  # geom_jitter(data=char_dat, mapping=aes(x=nparks, y=logit_y), pch=1, width=0.2, height=0.05, size=1) +
  # Plot p-value
  annotate(geom="text", x=0, y=1, label="p=0.006", hjust=0, size=2.2) +
  # Labels
  labs(x="Number of parks\nwithin 1 km", y="P(charismatic)", tag="C", title=" ") +
  # Scale
  scale_y_continuous(lim=c(0,1)) +
  # Theme
  theme_bw() + my_theme
g3

# Plot
range(under_dat$distance_to_port) /1000
g4 <- ggplot(df_ports, mapping=aes(x=x/1000, y=y)) +
  # Plot CI
  geom_ribbon(mapping=aes(x=x/1000, ymin=y_lo, ymax=y_hi), fill="grey90") +
  # Median
  geom_line() +
  # Plot rug
  # geom_jitter(data=under_dat, mapping=aes(x=distance_to_port/1000, y=logit_y), pch=1, width=0.2, height=0.05, size=1) +
  # Plot p-value
  annotate(geom="text", x=0, y=1, label="p<0.001", hjust=0, size=2.2) +
  # Labels
  labs(x="Distance to port (km)\n", 
       y="P(underutilized)", tag="D", title="Underutilized MPAs") +
  # Scale
  scale_y_continuous(lim=c(0,1)) +
  # Theme
  theme_bw() + my_theme
g4

# Plot
range(under_dat$sandy_beach_km)
g5 <- ggplot(df_beach2, mapping=aes(x=x, y=y)) +
  # Plot CI
  geom_ribbon(mapping=aes(x=x, ymin=y_lo, ymax=y_hi), fill="grey90") +
  # Median
  geom_line() +
  # Plot rug
  # geom_jitter(data=under_dat, mapping=aes(x=sandy_beach_km, y=logit_y), pch=1, width=0.2, height=0.05, size=1) +
  # Plot p-value
  annotate(geom="text", x=0, y=1, label="p=0.016", hjust=0, size=2.2) +
  # Labels
  labs(x="Sandy beach (km)\n", 
       y="P(underutilized)", tag="E", title=" ") +
  # Scale
  scale_y_continuous(lim=c(0,1)) +
  # Theme
  theme_bw() + my_theme
g5

# Plot
range(under_dat$n_parking_lots)
g6 <- ggplot(df_lots, mapping=aes(x=x, y=y)) +
  # Plot CI
  geom_ribbon(mapping=aes(x=x, ymin=y_lo, ymax=y_hi), fill="grey90") +
  # Median
  geom_line() +
  # Plot rug
  # geom_jitter(data=under_dat, mapping=aes(x=n_parking_lots, y=logit_y), pch=1, width=0.2, height=0.05, size=1) +
  # Plot p-value
  annotate(geom="text", x=0, y=1, label="p=0.019", hjust=0, size=2.2) +
  # Labels
  labs(x="Number of parking lots\nwithin 1 km", 
       y="P(underutilized)", tag="F", title=" ") +
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
       width=6.5, height=4.5, units="in", dpi=600)


