
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
                                sheet="Responses_WB+MPA_Commercial", col_types="text", na=c("-", "998.0", "999.0"))

# Read key
q_key <- readxl::read_excel(file.path(outdir, "question_key.xlsx"))
r_key <- readxl::read_excel(file.path(outdir, "response_key.xlsx"))
p_key <- readxl::read_excel(file.path(outdir, "port_key.xlsx"))


# Format data
################################################################################

# Format data
data <- data_orig %>%
  # Slice
  slice(1:85) %>%
  # Rename
  rename(port_complex_id=port_portgroup) %>%
  # Add respondent id
  mutate(respondent_id=1:n()) %>% 
  select(respondent_id, port_complex_id, everything()) %>% 
  # Gather
  gather(key="question", value="response_id", 3:ncol(.)) %>%
  # Add port info
  mutate(port_complex_id=as.numeric(port_complex_id)) %>% 
  left_join(p_key, by="port_complex_id") %>%
  mutate(port_complex=factor(port_complex,
                             levels=p_key$port_complex),
         region=factor(region, levels=c("North", "North Central", "Central", "South"))) %>%
  # Format question
  mutate(question=recode(question,
                         "access_econ"="Access to harvestable resources",
                         "covid19"="Covid-19 impacts",
                         "ecological_mpa"="Ecological",
                         "enforcement_mpa"="Enforcement",
                         "income_econ"="Income from fishing",
                         "infrastructure_econ"="Infrastructure",
                         "jobsatisfaction_soc"="Job satisfaction",
                         "labor_soc"="Labor/new participants",
                         "livelihood_mpa"="Livelihoods",
                         "management_mpa"="Management",
                         "marineresourcefuture_env"="Marine resource health-future",
                         "marineresourcepresent_env"="Marine resource health-present",
                         "markets_econ"="Markets",
                         "monitoring_mpa"="Monitoring",
                         "relationshipsexternal_soc"="Social relationships (external)",
                         "relationshipsinternal_soc"="Social relationships (internal)")) %>%
  # Add question
  left_join(q_key, by="question") %>%
  # Add response
  mutate(response_id=as.numeric(response_id)) %>% 
  left_join(r_key, by=c("question", "response_id")) %>%
  # Arrange
  select(respondent_id, region, port_complex_id, port_complex, category, question, response_id, response, everything())

# Inspect
sort(unique(data$question))
freeR::complete(data)

# Export
saveRDS(data, file=file.path(outdir, "ecotrust_survey_data_comm.Rds"))

