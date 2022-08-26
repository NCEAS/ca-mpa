

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
data_orig <- readRDS(file=file.path(outdir, "CDFW_engagement_survey_data_business.Rds")) %>% 
  select(-c(date_start, date_end))


# Question 1
################################################################################

# Number of respondents
n_respondents <- n_distinct(data_orig$respondent_id)

# Data
data_q1 <- data_orig %>% 
  # Question 1
  filter(question_id=="1") %>% 
  # Simplify
  filter(!is.na(answer)) %>% 
  # Summarize
  group_by(question) %>% 
  summarize(n=n()) %>% 
  ungroup()

# Plot
ggplot(data_q1, aes(y=question, x=n)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of respondents", y="", 
       title="Q1. What type of services does your business provide?") +
  # Theme
  theme_bw()


# Question 2
################################################################################

# Data
data_q2 <- data_orig %>% 
  # Question 2
  filter(question_id==2) %>% 
  # Summarize
  group_by(answer) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  # Shorten
  mutate(answer_short=gsub("\\s*\\([^\\)]+\\)", "", answer)) %>% 
  # Order
  mutate(answer_short=factor(answer_short, 
                             levels=c("Not at all familiar", "Somewhat familiar", "Familiar", "Very familiar", 'Extremely familiar')))

# Plot
ggplot(data_q2, aes(x=n, y="" , fill=answer_short)) +
  geom_bar(stat="identity", color="grey30") +
  # Labels
  labs(x="Number of respondents", y="", 
       title="Q2. How familiar are you with California's Marine Protected Areas (MPAs)?") +
  # Legend
  scale_fill_manual(name="", values=RColorBrewer::brewer.pal(5, "RdBu")) +
  # Theme
  theme_bw() +
  theme(legend.position = "bottom")


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
ggplot(data_q3, aes(x=n, y="" , fill=answer)) +
  geom_bar(stat="identity", color="grey30") +
  # Labels
  labs(x="Number of respondents", y="", 
       title="Question 3. Does your business distribute or display information about MPAs to customers (either in stores or online)?") +
  # Legend
  scale_fill_discrete(name="") +
  # Theme
  theme_bw() +
  theme(legend.position = "bottom")


# Question 4a
################################################################################

# Data
data_q4a <- data_orig %>% 
  # Question 4
  filter(question_id=="4a") %>% 
  # Simplify
  filter(!is.na(answer)) %>% 
  # Summarize
  group_by(question) %>% 
  summarize(n=n()) %>% 
  ungroup()

# Plot
ggplot(data_q4a, aes(y=question, x=n)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of respondents", y="", 
       title="Q4a. What sort of information does your business distribute or display?") +
  # Theme
  theme_bw()


# Question 5a
################################################################################

# Data
data_q5a <- data_orig %>% 
  # Question 5
  filter(question_id=="5a") %>% 
  # Simplify
  filter(!is.na(answer)) %>% 
  # Summarize
  group_by(question) %>% 
  summarize(n=n()) %>% 
  ungroup()

# Plot
ggplot(data_q5a, aes(y=question, x=n)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of respondents", y="", 
       title="Q5a. What is the source of the information your business distributes or displays?") +
  # Theme
  theme_bw()


# Question 5b
################################################################################

# Data
data_q5b <- data_orig %>% 
  # Question 5b
  filter(question_id=="5b") %>% 
  # Summarize
  group_by(answer) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  # Format
  mutate(answer_short=recode(answer,
                             "I believe MPAs have negatively affected my business"="Negatively",            
                             "I do not believe MPAs have positively or negatively affected my business"="Neutral",
                             "I believe MPAs have positively affected my business"="Positively",            
                             "I am unsure whether MPAs have affected my business"="Unsure"))

# Plot
ggplot(data_q5b, aes(x=n, y="" , fill=answer_short)) +
  geom_bar(stat="identity", color="grey30") +
  # Labels
  labs(x="Number of respondents", y="", 
       title="Q5b. How have MPAs impacted your business?") +
  # Legend
  scale_fill_manual(name="", values=c(RColorBrewer::brewer.pal(3, "RdBu"), "grey50"), na.value="grey30") +
  # Theme
  theme_bw() +
  theme(legend.position = "bottom")


# Question 6
################################################################################

# Data
data_q6 <- data_orig %>% 
  # Question 5b
  filter(grepl("6", question_id, )) %>% 
  # Summarize
  group_by(question, answer) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  # Order
  mutate(answer=factor(answer,
                       levels=c("Strongly disagree", "Disagree", 
                                "Neither agree nor disagree", "Agree", "Strongly agree")))

# Plot
ggplot(data_q6, aes(x=n, y=question, fill=answer)) +
  geom_bar(stat="identity", col="grey30") +
  # Labels
  labs(x="Number of respondents", y="") +
  # Legeng
  scale_fill_manual(name="", values=RColorBrewer::brewer.pal(5, "RdBu")) +
  # Theme
  theme_bw()


# Question 7
################################################################################

# Data
data_q7 <- data_orig %>% 
  # Question 7
  filter(grepl("7", question_id, )) %>% 
  # Summarize
  group_by(question, answer) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  # Order
  mutate(answer=factor(answer,
                       levels=c("Strongly disagree", "Disagree", 
                                "Neither agree nor disagree", "Agree", "Strongly agree")))

# Plot
ggplot(data_q7, aes(x=n, y=question, fill=answer)) +
  geom_bar(stat="identity", col="grey30") +
  # Labels
  labs(x="Number of respondents", y="") +
  # Legeng
  scale_fill_manual(name="", values=RColorBrewer::brewer.pal(5, "RdBu")) +
  # Theme
  theme_bw()

# Question 8
################################################################################

# Data
data_q8 <- data_orig %>% 
  # Question 8
  filter(grepl("8", question_id, )) %>% 
  # Summarize
  group_by(question, answer) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  # Order
  mutate(answer=factor(answer,
                       levels=c("Not at all important", "Slightly important", 
                                "Moderately important", "Very important")))

# Plot
ggplot(data_q8, aes(x=n, y=question, fill=answer)) +
  geom_bar(stat="identity", col="grey30") +
  # Labels
  labs(x="Number of respondents", y="") +
  # Legend
  scale_fill_manual(name="", values=RColorBrewer::brewer.pal(4, "RdBu")) +
  # Theme
  theme_bw()


# Question 9
################################################################################







