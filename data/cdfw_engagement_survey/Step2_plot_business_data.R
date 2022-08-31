

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
  # Simplify
  filter(!is.na(answer)) %>% 
  # Summarize
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
       title="Q1. What type of services does your business provide?") +
  # Theme
  theme_bw() + my_theme 
g

# Export
ggsave(g, filename=file.path(plotdir, "BusinessQ1_type_of_servive_provided.png"), 
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
  ungroup() %>% 
  # Shorten
  mutate(answer_short=gsub("\\s*\\([^\\)]+\\)", "", answer)) %>% 
  # Order
  mutate(answer_short=factor(answer_short, 
                             levels=c("Not at all familiar", "Somewhat familiar", "Familiar", "Very familiar", 'Extremely familiar')))

# Plot
g <- ggplot(data_q2, aes(x=n, y="" , fill=answer_short)) +
  geom_bar(stat="identity", color="grey30") +
  # Labels
  labs(x="Number of respondents", y="", 
       title="Q2. How familiar are you with California's Marine Protected Areas (MPAs)?") +
  # Legend
  scale_fill_manual(name="", values=RColorBrewer::brewer.pal(5, "RdBu")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "bottom")
g

# Export
ggsave(g, filename=file.path(plotdir, "BusinessQ2_mpa_familiarity.png"), 
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
       title="Question 3. Does your business distribute or display information about MPAs to customers (either in stores or online)?") +
  # Legend
  scale_fill_discrete(name="") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "bottom")
g

# Export
ggsave(g, filename=file.path(plotdir, "BusinessQ3_display_mpa_info_yn.png"), 
       width=4.5, height=1.5, units="in", dpi=600)

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
  ungroup() %>% 
  # Order
  arrange(desc(n)) %>% 
  mutate(question=factor(question, levels=question))

# Plot
g <- ggplot(data_q4a, aes(y=question, x=n)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of respondents", y="", 
       title="Q4a. What sort of information does your business distribute or display?") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "BusinessQ4a_display_info_type.png"), 
       width=6.5, height=3.5, units="in", dpi=600)


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
  ungroup() %>% 
  # Order
  arrange(desc(n)) %>% 
  mutate(question=factor(question, levels=question))

# Plot
g <- ggplot(data_q5a, aes(y=question, x=n)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of respondents", y="", 
       title="Q5a. What is the source of the information your business distributes or displays?") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "BusinessQ5a_display_info_source.png"), 
       width=6.5, height=3.5, units="in", dpi=600)


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
g <- ggplot(data_q5b, aes(x=n, y="" , fill=answer_short)) +
  geom_bar(stat="identity", color="grey30") +
  # Labels
  labs(x="Number of respondents", y="", 
       title="Q5b. How have MPAs impacted your business?") +
  # Legend
  scale_fill_manual(name="", values=c(RColorBrewer::brewer.pal(3, "RdBu"), "grey50"), na.value="grey30") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "bottom")
g

# Export
ggsave(g, filename=file.path(plotdir, "BusinessQ5b_mpa_impact.png"), 
       width=4.5, height=1.5, units="in", dpi=600)


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
g <- ggplot(data_q6, aes(x=n, y=question, fill=answer)) +
  geom_bar(stat="identity", col="grey30") +
  # Labels
  labs(x="Number of respondents", y="") +
  # Legeng
  scale_fill_manual(name="", values=RColorBrewer::brewer.pal(5, "RdBu")) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "BusinessQ6_mpa_customer_interest.png"), 
       width=6.5, height=2.5, units="in", dpi=600)



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
g <- ggplot(data_q7, aes(x=n, y=question, fill=answer)) +
  geom_bar(stat="identity", col="grey30") +
  # Labels
  labs(x="Number of respondents", y="") +
  # Legeng
  scale_fill_manual(name="", values=RColorBrewer::brewer.pal(5, "RdBu")) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "BusinessQ7_mpa_beliefs.png"), 
       width=8.5, height=2.5, units="in", dpi=600)

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
g <- ggplot(data_q8, aes(x=n, y=question, fill=answer)) +
  geom_bar(stat="identity", col="grey30") +
  # Labels
  labs(x="Number of respondents", y="",
       title="Q8. For your business and customers,how important it is to get more information about:") +
  # Legend
  scale_fill_manual(name="", values=RColorBrewer::brewer.pal(4, "RdBu")) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "BusinessQ8_mpa_info_need.png"), 
       width=7.5, height=2.5, units="in", dpi=600)


# Question 9
################################################################################

# Data
data_q9 <- data_orig %>% 
  # Question 2
  filter(question_id==9) %>% 
  # Summarize
  filter(!is.na(answer)) %>% 
  group_by(answer) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  # Arrange
  arrange(desc(n)) %>% 
  mutate(answer=factor(answer, levels=answer))

# Plot
g <- ggplot(data_q9, aes(y=answer, x=n)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of respondents", y="", 
       title="Q9. How your business was impacted by the Covid-19 pandemic?") +
  # Theme
  theme_bw() + my_theme 
g

# Export
ggsave(g, filename=file.path(plotdir, "BusinessQ9_covid_impact.png"), 
       width=10.5, height=3.5, units="in", dpi=600)


# Question 10
################################################################################

# Data
data_q10 <- data_orig %>% 
  # Question 8
  filter(question_id=="10a") %>% 
  # Summarize
  group_by(answer) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  # Order
  mutate(answer=recode_factor(answer,
                              "Overall business is worse."="Worse",
                              "Overall business is better."="Better",
                              "Overall business has not changed significantly."="Unchanged",
                              "My business has been affected in a way not stated above"="Other"))

# Plot
g <- ggplot(data_q10, aes(x=n, y="", fill=answer)) +
  geom_bar(stat="identity", col="grey30") +
  # Labels
  labs(x="Number of respondents", y="",
       title="Q10. Since the COVID-19 pandemic how has your business fared?") +
  # Legend
  # scale_fill_manual(name="", values=RColorBrewer::brewer.pal(4, "RdBu")) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "BusinessQ10_covid_impact.png"), 
       width=4.5, height=1.5, units="in", dpi=600)


# Question 11
################################################################################

# Data
data_q11 <- data_orig %>% 
  # Question 8
  filter(question_id=="11") %>% 
  # Convert
  mutate(answer=as.numeric(answer))

# Plot
g <- ggplot(data_q11, aes(x="", y=answer)) +
  geom_boxplot(outlier.shape = NA, fill="grey90", color="grey50", lwd=0.5) +
  geom_jitter(width=0.05, height=0, size=1, pch=21, fill="grey30", color="black", alpha=0.5) +
  # Labels
  labs(x="", y="Years in business") +
  scale_y_continuous(breaks=seq(0,100,20)) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "BusinessQ11_yrs_in_business.png"), 
       width=2.5, height=2.5, units="in", dpi=600)


# Question 12
################################################################################

# Data
data_q12 <- data_orig %>% 
  # Question 12
  filter(question_id=="12") %>% 
  # Summarize
  filter(!is.na(answer)) %>% 
  group_by(answer) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  # Arrange
  arrange(desc(n)) %>% 
  mutate(answer=factor(answer, levels=answer))

# Plot
g <- ggplot(data_q12, aes(y=answer, x=n)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of respondents", y="", 
       title="Q12. Did you participate in the MPA design and planning efforts?") +
  # Theme
  theme_bw() + my_theme 
g

# Export
ggsave(g, filename=file.path(plotdir, "BusinessQ12_mlpa_participate.png"), 
       width=8.5, height=3.5, units="in", dpi=600)


# Question 13
################################################################################

# Data
data_q13 <- data_orig %>% 
  # Question 12
  filter(question_id=="13") %>% 
  # Summarize
  group_by(answer) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  # Shorten
  mutate(answer=recode_factor(answer,
                              "I do not have a strong opinion, or I do not have enough information, to say whether my support has changed."="No opinion or not enough information",
                              "My support for MPAs has decreased since I first learned about the program."="Decreased",
                              "My support for MPAs has increased since I first learned about the program."="Increased",                             
                              "My support for MPAs has neither increased nor decreased since I first learned about the program."="Neither increased nor decreased"))



# Plot
g <- ggplot(data_q13, aes(x=n, y="", fill=answer)) +
  geom_bar(stat="identity", col="grey30") +
  # Labels
  labs(x="Number of respondents", y="",
       title="Q13. How has your support, or opinion, about MPAs changed over time?") +
  # Legend
  # scale_fill_manual(name="", values=RColorBrewer::brewer.pal(4, "RdBu")) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "BusinessQ13_mpa_support_change.png"), 
       width=4.5, height=1.5, units="in", dpi=600)                        





