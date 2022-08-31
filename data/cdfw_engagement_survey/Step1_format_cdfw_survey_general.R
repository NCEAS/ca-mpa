

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
data_orig <- readxl::read_excel(file.path(indir, "HDSurveyResults_8.5.22_NCEAS (1).xlsx"), sheet=2, skip=1)


# Format data
################################################################################

# Column names
colnames <- c("respondent_id", "date_start", "date_end",
              "q1_California Department of Fish and Wildlife website",
              "q1_Other websites",
              "q1_Through email",
              "q1_Word of mouth/friend's referral",
              "q1_Organizational referral",
              "q1_MPA Collaborative Network",
              "q1_Facebook",
              "q1_Twitter",
              "q1_Instagram",
              "q1_Other social media platform",
              "q1_Other",
              "q2_In a typical year, before the coronavirus pandemic began, how often did you visit the California coast or coastal waters?",
              "q3_Since the coronavirus pandemic, how often have you visited the California coast or coastal waters?",
              "q4_Fishing or taking other marine resources",
              "q4_Boating or sailing",
              "q4_Kayaking or stand-up paddle boarding",	
              "q4_Swimming",
              "q4_Surfing or other water sports",	
              "q4_Bird or wildlife watching",	
              "q4_Tidepooling",
              "q4_Scuba diving or snorkeling",	
              "q4_Spending time on the beach",	
              "q4_Education or research",	
              "q4_Volunteering at the beach and/or coastal area",	
              "q5_How familiar are you with MPAs?",	
              "q6_Conservation",	
              "q6_Fisheries management",	
              "q6_Public education",	
              "q6_Scientific research",	
              "q6_Improved recreational experience",	
              "q6_I don't know/I don't have an opinion",	
              "q7_How many MPAs have you visted in the North Coast region?",	
              "q8_How many MPAs have you visted in the North Central Coast region?",	
              "q9_How many MPAs have you visted in the Central Coast region?",	
              "q10_How many MPAs have you visted in the South Coast region?",	
              "q11_California does a good job communicating about the MPA program.",	
              "q11_MPA education and outreach is adequate.",
              "q11_MPAs improve visitors wildlife experience.",	
              "q11_There are too many MPAs in California.",	
              "q11_There are too few MPAs in California.",	
              "q11_MPA rules are too strict.",
              "q12_MPA rules are hard to understand.",
              "q12_Public awareness about California's MPAs is high.",	
              "q12_Most of the other people I observe follow MPA rules.",	
              "q12_California is doing a good job of enforcing MPA rules.",	
              "q12_I know where to look if I need more information about MPAs.",	
              "q12_MPA designation increases the likelihood of people visiting the area.",	
              "q13_Friends, family, or word of mouth",
              "q13_California Department of Fish and Wildlife website",
              "q13_Other websites",
              "q13_Signs",
              "q13_Brochures, posters, regulation booklets",
              "q13_Social media", 
              "q13_Newspapers, television, magazine",	
              "q13_Visitor centers, museums, or aquariums",
              "q13_Schools or school related events (K-12)",	
              "q13_University or college classes",
              "q13_Parks Online Resources for Teachers and Resources (PORTs)",
              "q13_In person contact: Law Enforcement",
              "q13_In person contact: Docent/educator",	
              "q13_I did not know about MPAs before this survey",
              "q13_Other (please specify)",
              "q14_Friends, family, or word of mouth",
              "q14_California Department of Fish and Wildlife website",	
              "q14_Other governmental websites",
              "q14_Signs",
              "q14_Brochures, posters, regulation booklets",
              "q14_Social media",
              "q14_Newspapers, television, magazine",	
              "q14_Visitor centers",
              "q14_Schools or school related events (K-12)",
              "q14_University or college classes",
              "q14_Other",	
              "q15_What is your age?",	
              "q16_Do you identify as",
              "q17_What best describes the highest level of education that you have attained?",
              "q18a_Zip code",
              "q18b_County",
              "q18_Do you speak a language other than English at home?")
              
# Format data
data <- data_orig %>%
  # Rename
  setNames(colnames) %>% 
  # Gather questions/answers
  gather(key="question_full", "answer", 4:ncol(.)) %>%
  # Extract question id
  separate(col=question_full, into=c("question_id", "question"), sep="_") %>%
  # Format question id
  mutate(question_id=gsub("q", "", question_id))

# Inspect
table(data$question_id)

# Questions
question_key <- data %>% 
  select(question, question_id) %>% 
  unique()

# Export data
saveRDS(data, file=file.path(outdir, "CDFW_engagement_survey_data_general.Rds"))



