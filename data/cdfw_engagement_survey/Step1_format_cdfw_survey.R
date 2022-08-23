

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
business_orig <- readxl::read_excel(file.path(indir, "HDSurveyResults_8.5.22_NCEAS (1).xlsx"), sheet=1, skip=1)
general_orig <- readxl::read_excel(file.path(indir, "HDSurveyResults_8.5.22_NCEAS (1).xlsx"), sheet=2, skip=1)


# Format data
################################################################################

# Column names
colnames <- c("respondent_id", "date_start", "date_end",
              "q1_Cultural tours", 
              "q1_Outdoor and nature education",
              "q1_Whale watching",
              "q1_Kayak tours",
              "q1_Self-powered watercraft rentals (kayak/stand-up paddle boards)",
              "q1_Motorized watercraft rentals (boats/jet skis)",
              "q1_Recreational fishing charters",
              "q1_Fishing tackle rentals/sales",
              "q1_Scuba charters",
              "q1_Scuba gear rental and outfitting",
              "q1_Other (please specify)",
              "q2_How familiar are you with California's Marine Protected Areas (MPAs)?",
              "q3_Does your business distribute or display information about MPAs to customers (either in stores or online)?",
              "q4_MPA brochures in person",
              "q4_MPA brochures online",
              "q4_MPA guidebooks in person",
              "q4_MPA guidebooks online",
              "q4_MPA maps in person",
              "q4_MPA maps online",	
              "q4_Other in person resource (please specify in the box below)",
              "q4_Other online resource (please specify in the box below)",
              "q4_Other (please specify)",
              "q5a_California Department of Fish and Wildlife",
              "q5a_California State Parks",
              "q5a_National Park Service",
              "q5a_National Marine Sanctuaries",	
              "q5a_Marine Protected Areas Collaborative Network",
              "q5a_California Marine Sanctuary Foundation",
              "q5a_Other government agency (please specify in the box below)",
              "q5a_Other non-profit (please specify in the box below)",
              "q5a_I don't know or unsure",
              "q5a_Other (please specify)",
              "q5b_Not aware or familiar with MPAs.",
              "q5b_Do not have MPA information to distribute or display.",
              "q5b_Do not have the time to distribute or display MPA information.",
              "q5b_I am concerned that my customers are not interested in information or will not appreciate information.",
              "q5b_I do not think it is the role of my business to distribute or display this information.",
              "q5b_I do not currently have a way to display or distribute information due to the coronavirus epidemic restrictions.",
              "q5b_I do not support MPAs	Other (please specify)",
              "q5b_Other (please specify)",
              "q5c_Please choose the option that best describes your beliefs about how MPAs have affected your business:",
              "q6_MPAs have benefited marine resources.",
              "q6_I have adequate resources to inform customers about MPAs.",
              "q6_My customers ask about MPAs.",
              "q6_My customers seek to experience MPAs.",
              "q6_Customer interest in MPAs has increased over time.",
              "q7_California does a good job communicating about the MPA Management Program.",	
              "q7_MPAs improve visitors wildlife experience.",
              "q7_MPA rules are hard to understand.",
              "q7_The public should be more aware about California's MPA Network.",
              "q7_I know where to look if I need more information about MPAs.",
              "q7_MPA designation increases the likelihood of people visiting the area.",
              "q7_MPAs have benefited marine resources.",
              "q8_What MPAs are.",
              "q8_How MPAs work.",
              "q8_Where different MPAs are located.",	
              "q8_General MPAs rules and regulations.",
              "q8_Rules and regulations for specific MPAs.",	
              "q8_How individuals can learn more about the MPA network.",
              "q8_How individuals can learn more about how to get involved in the MPA network.",
              "q9_My business closed for a period of time.",
              "q9_My business had to reduce operating capacity, and is still operating at reduced capacity.",
              "q9_My business had to reduce operating capacity, but has since recovered to, or is exceeding, its traditional operating capacity.",	
              "q9_My business had to increase its use of online tools to communicate with customers and clients.",
              "q9_I have seen an increase in business due to customers and clients wanting to participate in outdoor recreation due to the pandemic.",
              "q10_Since the COVID-19 pandemic how has your business fared?",
              "q10_My business has been affected in a way not stated above",
              "q11_How many years have you been in business?",
              "q12_North Coast",	
              "q12_North Central Coast",	
              "q12_Central Coast",	
              "q12_South Coast",
              "q12_Did not participate",
              "q13_How has your support, or opinion, about MPAs changed over time?",
              "q14_Would you be willing to participate in a future survey to assess economic impacts of MPA implementation in California?")
              
# Format data
business <- business_orig %>%
  # Rename
  setNames(colnames) %>% 
  # Gather questions/answers
  gather(key="question_full", "answer", 4:ncol(.)) %>% 
  # Extract question id
  separate(col=question_full, into=c("question_id", "question"), sep="_") %>% 
  # Format question id
  mutate(question_id=gsub("q", "", question_id))

# Inspect
table(business$question_id)

# Questions
question_key <- business %>% 
  select(question, question_id) %>% 
  unique()
  
