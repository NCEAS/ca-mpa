

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
plotdir <- "data/cdfw_engagement_survey/figures"

# Read data
data_orig <- readRDS(file=file.path(outdir, "CDFW_engagement_survey_data_general.Rds")) %>% 
  select(-c(date_start, date_end))


# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.3, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Question 1
################################################################################

# Number of respondents
n_respondents <- n_distinct(data_orig$respondent_id)

# Data
data_q1 <- data_orig %>% 
  # Question 1
  filter(question_id=="1") %>% 
  # Summarize
  filter(!is.na(answer)) %>%
  group_by(question) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  # Order responses
  arrange(desc(n)) %>% 
  mutate(question=factor(question,
                         levels=question))

# Plot
g <- ggplot(data_q1, aes(y=question, x=n)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of respondents", y="", 
       title="Q1. How did you learn about this survey?") +
  # Theme
  theme_bw() + my_theme 
g

# Export
ggsave(g, filename=file.path(plotdir, "GeneralQ1_learn_of_survey.png"), 
       width=6.5, height=3.5, units="in", dpi=600)


# Question 2
################################################################################

# Data
data_q2 <- data_orig %>% 
  # Question 2
  filter(question_id==2) %>% 
  # Summarize
  group_by(answer) %>% 
  summarize(n=n()) %>% 
  ungroup()

# Plot
g <- ggplot(data_q2, aes(x=n, y="" , fill=answer)) +
  geom_bar(stat="identity", color="grey30") +
  # Labels
  labs(x="Number of respondents", y="", 
       title="Q2. In a typical year, before the coronavirus pandemic began, how often did you visit the California coast or coastal waters?") +
  # Legend
  # scale_fill_manual(name="", values=RColorBrewer::brewer.pal(5, "RdBu")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "bottom")
g

# Export
ggsave(g, filename=file.path(plotdir, "GeneralQ2_n_coast_visits.png"), 
       width=4.5, height=1.5, units="in", dpi=600)


# Question 3
################################################################################

# Data
data_q3 <- data_orig %>% 
  # Question 2
  filter(question_id==3) %>% 
  # Summarize
  group_by(answer) %>% 
  summarize(n=n()) %>% 
  ungroup()

# Plot
g <- ggplot(data_q3, aes(x=n, y="" , fill=answer)) +
  geom_bar(stat="identity", color="grey30") +
  # Labels
  labs(x="Number of respondents", y="", 
       title="Q3. Since the coronavirus pandemic, how often have you visited the California coast or coastal waters?") +
  # Legend
  scale_fill_discrete(name="") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "bottom")
g

# Export
ggsave(g, filename=file.path(plotdir, "GeneralQ3_post_pre_vists.png"), 
       width=4.5, height=1.5, units="in", dpi=600)


# Question 4
################################################################################

# Data
data_q4 <- data_orig %>% 
  # Question 4
  filter(question_id=="4") %>% 
  # Summarize
  filter(!is.na(answer)) %>% 
  group_by(question) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  # Order
  arrange(desc(n)) %>% 
  mutate(question=factor(question, levels=question))

# Plot
g <- ggplot(data_q4, aes(y=question, x=n)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of respondents", y="", 
       title="Q4a. When visiting coastal areas, how often do you do the following activities in a typical year?") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "GeneralQ4_mpa_activities.png"), 
       width=6.5, height=3.5, units="in", dpi=600)


# Question 5
################################################################################

# Data
data_q5 <- data_orig %>% 
  # Question 5
  filter(question_id=="5") %>% 
  # Summarize
  group_by(answer) %>% 
  summarize(n=n()) %>% 
  ungroup() 

# Plot
g <- ggplot(data_q5, aes(fill=answer, y="", x=n)) +
  geom_bar(stat="identity", color="grey30") +
  # Labels
  labs(x="Number of respondents", y="", 
       title="Q5. How familiar are you with MPAs?") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "bottom")
g

# Export
ggsave(g, filename=file.path(plotdir, "GeneralQ5_mpa_familiarity.png"), 
       width=4.5, height=1.5, units="in", dpi=600)


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
g <- ggplot(data_q6, aes(x=n, y="" , fill=question)) +
  geom_bar(stat="identity", color="grey30") +
  # Labels
  labs(x="Number of respondents", y="", 
       title="Q6. In your opinion, what are the two most important outcomes of Californias MPAs?") +
  # Legend
  # scale_fill_manual(name="", values=c(RColorBrewer::brewer.pal(3, "RdBu"), "grey50"), na.value="grey30") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "bottom")
g

# Export
ggsave(g, filename=file.path(plotdir, "GeneralQ6_mpa_outcomes.png"), 
       width=4.5, height=1.5, units="in", dpi=600)


# Question 7-10
################################################################################

# Data
data_q78910 <- data_orig %>% 
  # Question 5b
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
                       "10"="South"))
  

# Plot
g <- ggplot(data_q78910, aes(x=n, y=region , fill=answer)) +
  geom_bar(stat="identity", color="grey30") +
  # Labels
  labs(x="Number of respondents", y="", 
       title="Q7. How many MPAs have you visted in the following regions?") +
  # Legend
  # scale_fill_manual(name="", values=c(RColorBrewer::brewer.pal(3, "RdBu"), "grey50"), na.value="grey30") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "bottom")
g

# Export
ggsave(g, filename=file.path(plotdir, "GeneralQ7-10_mpa_visits_north.png"), 
       width=4.5, height=2.5, units="in", dpi=600)



# Question 11
################################################################################

# Data
data_q11 <- data_orig %>% 
  # Question 7
  filter(question_id==11) %>% 
  # Summarize
  group_by(question, answer) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  # Order
  mutate(answer=factor(answer,
                       levels=c("Strongly disagree", "Disagree", 
                                "Neither agree nor disagree", "Agree", "Strongly agree")))

# Plot
g <- ggplot(data_q11, aes(x=n, y=question, fill=answer)) +
  geom_bar(stat="identity", col="grey30") +
  # Labels
  labs(x="Number of respondents", y="", title="Q11. To what extent do you agree with the following:") +
  # Legeng
  scale_fill_manual(name="", values=RColorBrewer::brewer.pal(5, "RdBu")) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "GeneralQ11_mpa_beliefs.png"), 
       width=6.5, height=6.5, units="in", dpi=600)


# Question 12
################################################################################

# Data
data_q12 <- data_orig %>% 
  filter(question_id==12) %>% 
  # Summarize
  group_by(question, answer) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  # Order
  mutate(answer=factor(answer,
                       levels=c("Strongly disagree", "Disagree", 
                                "Neither agree nor disagree", "Agree", "Strongly agree", "I do not know")))

# Plot
g <- ggplot(data_q12, aes(x=n, y=question, fill=answer)) +
  geom_bar(stat="identity", col="grey30") +
  # Labels
  labs(x="Number of respondents", y="", title="Q12. To what extent do you agree with the following:") +
  # Legeng
  scale_fill_manual(name="", values=c(RColorBrewer::brewer.pal(5, "RdBu"), "grey80")) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "GeneralQ12_mpa_beliefs_more.png"), 
       width=6.5, height=6.5, units="in", dpi=600)


# Question 13
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
       title="Q13. From which of the following sources did you learn about MPAs?") +
  # Theme
  theme_bw() + my_theme 
g

# Export
ggsave(g, filename=file.path(plotdir, "GeneralQ13_how_learn_mpas.png"), 
       width=10.5, height=3.5, units="in", dpi=600)

# Question 14
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
  filter(question!="Other")

# Plot
g <- ggplot(data_q14, aes(y=question, x=n, fill=answer)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of respondents", y="", 
       title="Q14. In the future, how useful do you think the following would be for learning about MPAs?") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "bottom")
g

# Export
ggsave(g, filename=file.path(plotdir, "GeneralQ14_how_learn_mpas_future.png"), 
       width=10.5, height=3.5, units="in", dpi=600)


# Question 15
################################################################################

# Data
data_q15 <- data_orig %>% 
  # Question 8
  filter(question_id=="15") %>% 
  # Summarize
  group_by(answer) %>% 
  summarize(n=n()) %>% 
  ungroup()

# Plot
g <- ggplot(data_q15, aes(x=n, y="", fill=answer)) +
  geom_bar(stat="identity", col="grey30") +
  # Labels
  labs(x="Number of respondents", y="",
       title="Q15. What is your age?") +
  # Legend
  # scale_fill_manual(name="", values=RColorBrewer::brewer.pal(4, "RdBu")) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "GeneralQ15_age.png"), 
       width=4.5, height=1.5, units="in", dpi=600)


# Question 16
################################################################################

# Data
data_q16 <- data_orig %>% 
  # Question 8
  filter(question_id=="16") %>% 
  # Summarize
  group_by(answer) %>% 
  summarize(n=n()) %>% 
  ungroup()

# Plot
g <- ggplot(data_q16, aes(x=n, y="", fill=answer)) +
  geom_bar(stat="identity", col="grey30") +
  # Labels
  labs(x="Number of respondents", y="",
       title="Q16. Which do you identify with") +
  # Legend
  # scale_fill_manual(name="", values=RColorBrewer::brewer.pal(4, "RdBu")) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "GeneralQ16_gender.png"), 
       width=4.5, height=1.5, units="in", dpi=600)


# Question 17
################################################################################

# Data
data_q17 <- data_orig %>% 
  filter(question_id=="17") %>% 
  # Summarize
  group_by(answer) %>% 
  summarize(n=n()) %>% 
  ungroup()

# Plot
g <- ggplot(data_q17, aes(x=n, y="", fill=answer)) +
  geom_bar(stat="identity", col="grey30") +
  # Labels
  labs(x="Number of respondents", y="",
       title="Q17. What best describes the highest level of education that you have attained?") +
  # Legend
  # scale_fill_manual(name="", values=RColorBrewer::brewer.pal(4, "RdBu")) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "GeneralQ17_education.png"), 
       width=4.5, height=1.5, units="in", dpi=600)


# Question 18b
################################################################################

# Data
data_q18b <- data_orig %>% 
  filter(question_id=="18b") %>% 
  # Summarize
  group_by(answer) %>% 
  summarize(n=n()) %>% 
  ungroup()


# Plot
g <- ggplot(data_q18b, aes(x=n, y="", fill=answer)) +
  geom_bar(stat="identity", col="grey30") +
  # Labels
  labs(x="Number of respondents", y="",
       title="Q18b. County?") +
  # Legend
  # scale_fill_manual(name="", values=RColorBrewer::brewer.pal(4, "RdBu")) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "GeneralQ18b_county.png"), 
       width=4.5, height=1.5, units="in", dpi=600)                   





