
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data/"
indir <- file.path(basedir, "ecotrust/raw")
outdir <- file.path(basedir, "ecotrust/processed")
plotdir <- "data/ecotrust/figures"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "MPAHumanUses_FocusGroup_QuantitativeData_raw_090921.xlsx"),
                                sheet="Responses_WB+MPA_CPFV", col_types="text", na=c("-", "998.0", "999.0"))


# Read key
q_key <- readxl::read_excel(file.path(outdir, "question_key_cpfv.xlsx"))
r_key <- readxl::read_excel(file.path(outdir, "response_key_cpfv.xlsx"))

# Format data
################################################################################

# Format data
data <- data_orig %>%
  # Slice
  slice(1:22) %>%
  # Rename
  rename(port_complex_id=regionalportgroup) %>%
  # Add respondent id
  mutate(respondent_id=1:n()) %>% 
  select(respondent_id, port_complex_id, everything()) %>% 
  # Gather
  gather(key="question", value="response_id", 3:ncol(.)) %>%
  # Add port info
  mutate(port_complex_id=as.integer(port_complex_id) %>% as.character(),
         port_complex=recode(port_complex_id,
                             "1" = "North Coast Area Ports",
                             "2" = "Bodega Bay",
                             "3" = "San Francisco Area Ports",
                             "4" = "Monterey Bay",
                             "5" = "Santa Barbara and Ventura/Channel Islands Area Ports",
                             "6" = "Los Angeles/Long Beach Area Ports",
                             "7" = "Orange County/San Diego Area Ports")) %>% 
  # Format question
  mutate(question=recode(question,
                         "allocation_econ"="Allocation of resources",
                         "covid19"="Covid-19 impacts",
                         "ecological_mpa"="Ecological",
                         "enforcement_mpa"="Enforcement",
                         "income_econ"="Income from fishing",
                         "infrastructure_econ"="Infrastructure",
                         "jobsatisfaction_soc"="Job satisfaction",
                         "livelihood_mpa"="Livelihoods",
                         "management_mpa"="Management",
                         "marineresourcefuture_env"="Marine resource health-future",
                         "marineresourcepresent_env"="Marine resource health-present",
                         "markets_econ"="Markets",
                         "monitoring_mpa"="Monitoring",
                         "relationshipsexternal_soc"="Social relationships (external)",
                         "relationshipsinternal_soc"="Social relationships (internal)")) %>%
  # Add question
  left_join(q_key %>% select(-question_id, question_long), by="question") %>%
  # Add response
  mutate(response_id=as.numeric(response_id)) %>% 
  left_join(r_key, by=c("question", "response_id")) %>%
  # Arrange
  select(respondent_id, port_complex_id, port_complex, question, response_id, response, everything())

# Inspect
sort(unique(data$question))
freeR::complete(data)

# Export
saveRDS(data, file=file.path(outdir, "ecotrust_survey_data_cpfv.Rds"))

