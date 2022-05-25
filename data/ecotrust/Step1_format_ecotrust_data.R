
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


# Format data
################################################################################

# Format data
data <- data_orig %>%
  # Slice
  slice(1:85) %>%
  # Rename
  rename(port_complex=port_portgroup) %>%
  # Gather
  gather(key="question", value="response", 2:ncol(.)) %>%
  # Count
  count(port_complex, question, response) %>%
  # Format port complex
  mutate(port_complex=recode_factor(port_complex,
                                    "19"="San Diego",
                                    "18"="Oceanside",
                                    "17"="Orange County",
                                    "16"="Los Angeles/Long Beach",
                                    "15"="Ventura/Channel Islands",
                                    "14"="Santa Barbara",
                                    "13"="Morro Bay/Port San Luis",
                                    "12"="Monterey",
                                    "11"="Moss Landing",
                                    "10"="Santa Cruz",
                                    "9"="Princeton/Half Moon Bay",
                                    "8"="San Francisco",
                                    "7"="Bodega Bay",
                                    "6"="Point Arena",
                                    "5"="Fort Bragg/Albion",
                                    "4"="Shelter Cove",
                                    "3"="Eureka",
                                    "2"="Trinidad",
                                    "1"="Crescent City")) %>%
  # Format question
  mutate(question=recode(question,
                         "access_econ"="Access to\nharvestable resources",
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
                         "relationshipsexternal_soc"="Social relationships\n(external)",
                         "relationshipsinternal_soc"="Social relationships\n(internal)")) %>%
  # Arrange
  arrange(port_complex, question) %>%
  # Add prop
  group_by(port_complex, question) %>%
  mutate(prop=n/sum(n)) %>%
  ungroup() %>%
  # Add question
  left_join(q_key, by="question") %>%
  # Convert response
  mutate(response=as.numeric(response)) %>% 
  # Arrange
  select(port_complex, category, question, everything())

# Inspect
unique(data$response)
sort(unique(data$question))

# Export
saveRDS(data, file=file.path(outdir, "ecotrust_survey_data.Rds"))

