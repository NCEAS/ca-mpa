

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(rinat)
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data/"
indir <- file.path(basedir, "cdfw_engagement_survey/raw")
outdir <- file.path(basedir, "cdfw_engagement_survey/processed")
plotdir <- "data/cdfw_engagement_survey/figures/pretty"

# Read data
data_orig <- readRDS(file=file.path(outdir, "CDFW_engagement_survey_data_general.Rds")) %>% 
  select(-c(date_start, date_end))

# Number of respondents
n_respondents <- n_distinct(data_orig$respondent_id)

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   plot.tag=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.3, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Respondent demographics
################################################################################

# Age data
data_q15 <- data_orig %>% 
  filter(question_id=="15") %>% 
  group_by(answer) %>% 
  summarize(n=n()) %>% 
  ungroup()

# Sex data
data_q16 <- data_orig %>% 
  filter(question_id=="16") %>% 
  group_by(answer) %>% 
  summarize(n=n()) %>% 
  ungroup()

# Education data
data_q17 <- data_orig %>% 
  filter(question_id=="17") %>% 
  group_by(answer) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  mutate(answer=recode_factor(answer,
                              "A high school diploma or equivalent"="High school diploma",
                              "Some college"="Some college",
                              "A bachelor's degree"="Bachelor's degree",
                              "An associate's degree or equivalent"="Associate's degree",
                              "A master's degree or higher"="Master's degree or higher" ,
                              "Prefer not to answer"="Prefer not to answer"))

# County data
data_q18b <- data_orig %>% 
  filter(question_id=="18b") %>% 
  group_by(answer) %>% 
  summarize(n=n()) %>% 
  ungroup()

# County shapefile
counties <- tigris::counties(state = "California")
counties1 <- counties %>% 
  janitor::clean_names() %>% 
  left_join(data_q18b, by=c("name"="answer")) %>% 
  select(name, n)


# County plot
g0 <- ggplot() +
  geom_sf(data=counties1, mapping=aes(fill=n), color="grey30", lwd=0.2) +
  # Labels
  labs(x="", y="", tag="A") +
  # Legend
  scale_fill_gradientn(name="Number of\nrespondents",
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"), na.value="grey90") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Crop
  coord_sf(xlim=c(-124, -116)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        legend.position = c(0.8,0.8))
g0

# Gender plot
g1 <- ggplot(data_q16, aes(x="", y=n, fill=answer)) +
  geom_bar(stat="identity", col="grey30") +
  # Labels
  labs(x="", y="Number of respondents", tag="B") +
  scale_y_continuous(breaks=seq(0,100,10)) +
  # Legend
  scale_fill_discrete(name="Gender                            ") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.ticks.x=element_blank())
g1

# Age plot
g2 <- ggplot(data_q15, aes(x="", y=n, fill=answer)) +
  geom_bar(stat="identity", col="grey30") +
  # Labels
  labs(x="", y="Number of respondents", tag="C") +
  scale_y_continuous(breaks=seq(0,100,10)) +
  # Legend
  scale_fill_discrete(name="Age group                         ") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.ticks.x=element_blank())
g2

# Education plot
g3 <- ggplot(data_q17, aes(x="", y=n, fill=answer)) +
  geom_bar(stat="identity", col="grey30") +
  # Labels
  labs(x="", y="Number of respondents", tag="D") +
  scale_y_continuous(breaks=seq(0,100,10)) +
  # Legend
  scale_fill_discrete(name="Highest education             ") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.ticks.x=element_blank())
g3

# Merge data
layout_matrix <- matrix(c(1,2,
                          1,3,
                          1,4), ncol=2, byrow=T)
g <- gridExtra::grid.arrange(g0, g1, g2, g3, layout_matrix=layout_matrix, widths=c(0.6, 0.4))
g

# Export figures
ggsave(g, filename=file.path(plotdir, "Gen1_respondent_demographics.png"), 
       width=6.5, height=5.5, units="in", dpi=600)



# Survey source
################################################################################

# Data
data_q1 <- data_orig %>% 
  # Filter
  filter(question_id=="1") %>% 
  # Summarize
  filter(!is.na(answer)) %>%
  group_by(question) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  # Recode
  mutate(question=recode(question,
                         "California Department of Fish and Wildlife website"="CDFW website",
                         "Word of mouth/friend's referral"="Word of mouth")) %>% 
  # Order 
  arrange(desc(n)) %>% 
  mutate(question=factor(question,
                         levels=question))

# Plot
g <- ggplot(data_q1, aes(y=question, x=n)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of respondents", y="", 
       title="How did you learn about this survey?") +
  scale_x_continuous(breaks=seq(0,100,10)) +
  # Theme
  theme_bw() + my_theme 
g

# Export
ggsave(g, filename=file.path(plotdir, "Gen2_learn_of_survey.png"), 
       width=6.5, height=3.5, units="in", dpi=600)



# Covid impact
################################################################################

# Visits before pandemic
data_q2 <- data_orig %>% 
  # Filter
  filter(question_id==2) %>% 
  # Summarize
  group_by(answer) %>% 
  summarize(n=n()) %>% 
  ungroup()

# Visits after pandemic
data_q3 <- data_orig %>% 
  # Question 2
  filter(question_id==3) %>% 
  # Summarize
  group_by(answer) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  # Recode
  mutate(answer=recode(answer,
                       "I have not visited the California coast or coastal waters since the coronavirus pandemic began."="Not visited",                   
                       "I have visited the California coast or coastal waters about as often as I did before the coronavirus pandemic began."="As often",
                       "I have visited the California coast or coastal waters less often since the coronavirus pandemic began."="Less often",
                       "I have visited the California coast or coastal waters more often since the coronavirus pandemic began."="More often"))

# Plot
g1 <- ggplot(data_q2, aes(x="", y=n , fill=answer)) +
  geom_bar(stat="identity", color="grey30") +
  # Labels
  labs(x="", y="Number of respondents", tag="A",
       title="Before the coronavirus pandemic began,\nhow often did you visit California's coast?") +
  scale_y_continuous(breaks=seq(0,100,10)) +
  # Legend
  scale_fill_ordinal(name="# of annual visits") +
  # Theme
  theme_bw() + my_theme
g1

# Plot
g2 <- ggplot(data_q3, aes(x="", y=n , fill=answer)) +
  geom_bar(stat="identity", color="grey30") +
  # Labels
  labs(x="", y="Number of respondents", tag="B",
       title="Since the coronavirus pandemic,\nhow often have you visited California's coast?") +
  scale_y_continuous(breaks=seq(0,100,10)) +
  # Legend
  scale_fill_ordinal(name="Visits relative\nto pre-pandemic") +
  # Theme
  theme_bw() + my_theme
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1)


# Export
ggsave(g, filename=file.path(plotdir, "Gen3_n_coast_visits.png"), 
       width=6.5, height=3.5, units="in", dpi=600)


# Activity types
################################################################################

# Data
data_q4 <- data_orig %>% 
  # Question 4
  filter(question_id=="4") %>% 
  # Summarize
  group_by(question, answer) %>% 
  summarize(n=n()) %>% 
  ungroup()

# Plot
g <- ggplot(data_q4, aes(y=question, x=n, fill=answer)) +
  geom_bar(stat="identity", col="grey30", lwd=0.1) +
  # Labels
  labs(x="Number of respondents", y="", 
       title="When visiting coastal areas,\nhow often do you do the following activities in a typical year?") +
  scale_x_continuous(breaks=seq(0,100,10)) +
  # Legend
  scale_fill_ordinal(name="Annual frequency") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "Gen4_mpa_activities.png"), 
       width=6.5, height=3.5, units="in", dpi=600)


# MPA familiarity
################################################################################

# Data
data_q5 <- data_orig %>% 
  # Question 5
  filter(question_id=="5") %>% 
  # Summarize
  group_by(answer) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  # Format
  mutate(answer=gsub("\\s*\\([^\\)]+\\)", "", answer) %>% trimws()) %>% 
  mutate(answer=factor(answer, 
                       levels=c("Extremely familiar", "Very familiar", 
                                "Familiar", "Somewhat familiar")))

# Plot
g <- ggplot(data_q5, aes(fill=answer, y=n, x="")) +
  geom_bar(stat="identity", color="grey30") +
  # Labels
  labs(y="Number of respondents", x="", 
       title="How familiar are you with MPAs?") +
  scale_y_continuous(breaks=seq(0,100,10)) +
  # Legend
  scale_fill_ordinal(name="Familiarity") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "Gen5_mpa_familiarity.png"), 
       width=4.5, height=3.5, units="in", dpi=600)


# Question 6
################################################################################

# Data
data_q6 <- data_orig %>% 
  # Question 5b
  filter(question_id=="6") %>% 
  # Summarize
  filter(!is.na(answer)) %>% 
  group_by(question) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  # Order
  arrange(desc(n)) %>% 
  mutate(question=factor(question, levels=question))

# Plot
g <- ggplot(data_q6, aes(x=n, y=question, )) +
  geom_bar(stat="identity", color="grey30") +
  # Labels
  labs(x="Number of answers", y="", 
       title="What are the two most important outcomes of Californias MPAs?") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "bottom")
g

# Export
ggsave(g, filename=file.path(plotdir, "Gen6_mpa_outcomes.png"), 
       width=5.5, height=3.5, units="in", dpi=600)


# What region have you visited MPAs in?
################################################################################

# Data
data_q78910 <- data_orig %>% 
  # Filter
  filter(question_id %in% 7:10) %>% 
  # Summarize
  group_by(question_id, answer) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  # Region
  rename(region=question_id) %>% 
  mutate(region=recode(region,
                       "7"="north",
                       "8"="North Central", 
                       "9"="Central",
                       "10"="South")) %>% 
  # Answer
  mutate(answer=recode_factor(answer,
                       "I have not visited the coast in this area."="None",                              
                       "I have visited one MPA in this area."="One",  
                       "I have visited multiple MPAs in this area."="Multiple",
                       "I have visited the coast in this area, but am not sure if I visited an MPA."="Unsure"))
  

# Plot
g <- ggplot(data_q78910, aes(x=n, y=region , fill=answer)) +
  geom_bar(stat="identity", color="grey30") +
  # Labels
  labs(x="Number of respondents", y="", 
       title="How many MPAs have you visted in the following regions?") +
  # Legend
  scale_fill_ordinal(name="# of MPAs visited", na.value="grey80") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "bottom")
g

# Export
ggsave(g, filename=file.path(plotdir, "Gen7_mpa_visits_north.png"), 
       width=4.5, height=2.5, units="in", dpi=600)



# MPA beliefs
################################################################################

# Data
data_q1112 <- data_orig %>% 
  # Question 7
  filter(question_id %in% 11:12) %>% 
  # Summarize
  group_by(question, answer) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  # Order
  mutate(answer=factor(answer,
                       levels=c("Strongly disagree", "Disagree", 
                                "Neither agree nor disagree", "Agree", "Strongly agree", "I do not know")))

# Plot
g <- ggplot(data_q1112, aes(x=n, y=question, fill=answer)) +
  geom_bar(stat="identity", col="grey30") +
  # Labels
  labs(x="Number of respondents", y="", title="To what extent do you agree with the following:") +
  # Legeng
  scale_fill_manual(name="", values=c(RColorBrewer::brewer.pal(5, "RdBu"), "grey30"), na.value="grey90") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position="bottom")
g

# Export
ggsave(g, filename=file.path(plotdir, "Gen8_mpa_beliefs.png"), 
       width=7, height=3.5, units="in", dpi=600)


# Source of MPA information
################################################################################

# Data
data_q13 <- data_orig %>% 
  # Question 2
  filter(question_id==13) %>% 
  # Summarize
  filter(!is.na(answer)) %>% 
  group_by(question) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  # Arrange
  arrange(desc(n)) %>% 
  mutate(question=factor(question, levels=question))

# Plot
g <- ggplot(data_q13, aes(y=question, x=n)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of respondents", y="", 
       title="How did you learn about MPAs?") +
  # Theme
  theme_bw() + my_theme 
g

# Export
ggsave(g, filename=file.path(plotdir, "Gen9_how_learn_mpas.png"), 
       width=6.5, height=3.5, units="in", dpi=600)


# Best way to learn about MPAs
################################################################################

# Data
data_q14 <- data_orig %>% 
  # Question 2
  filter(question_id==14) %>% 
  # Summarize
  group_by(question, answer) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  # Remove other
  filter(question!="Other") %>% 
  # Recode
  mutate(answer=factor(answer,
                       levels=c("Not useful at all", "A little useful", "Moderately useful", "Very useful", "No opinion")))

# Plot
g <- ggplot(data_q14, aes(y=question, x=n, fill=answer)) +
  geom_bar(stat="identity", color="grey30", lwd=0.1) +
  # Labels
  labs(x="Number of respondents", y="", 
       title="How useful would the following be for learning about MPAs?") +
  scale_x_continuous(breaks=seq(0,100,10)) +
  # Legend
  scale_fill_discrete(name="Usefulness") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "Gen10_how_learn_mpas_future.png"), 
       width=6.5, height=2.5, units="in", dpi=600)


