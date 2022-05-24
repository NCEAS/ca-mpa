
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
indir <- "data/ecotrust/raw"
outdir <- "data/ecotrust/processed"
plotdir <- "data/ecotrust/figures"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "MPAHumanUses_FocusGroup_QuantitativeData_raw_090921.xlsx"),
                                sheet="Responses_WB+MPA_Commercial", na=c("-", "998", "999"))

# Read key
q_key <- readxl::read_excel(file.path(indir, "question_key.xlsx"))

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
  # Arrange
  select(port_complex, category, question, everything())

# Inspect
sort(unique(data$question))


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=7),
                   plot.title=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = "bottom",
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot(data %>% filter(category=="MPA"),
            aes(x=prop, y=port_complex, fill=response %>% as.character())) +
  facet_wrap(~question, nrow=1) +
  geom_bar(stat="identity", col="grey30", lwd=0.1) +
  # Labels
  labs(x="Percent of respondents", y="",
       title="EcoTrust MPA questions") +
  scale_x_continuous(labels=scales::percent) +
  # Legend
  scale_fill_manual(name="Score", values=RColorBrewer::brewer.pal(5, "RdBu")) +
  guides(fill = guide_legend(nrow = 1)) +
  # Theme
  theme_bw() + my_theme
g1

ggsave(g1, filename=file.path(plotdir, "FigX_ecotrust_mpa_qs.png"),
       width=6.5, height=3, units="in", dpi=600)

# Plot data
g2 <- ggplot(data %>% filter(category=="Economic"),
            aes(x=prop, y=port_complex, fill=response %>% as.character())) +
  facet_wrap(~question, nrow=1) +
  geom_bar(stat="identity", col="grey30", lwd=0.1) +
  # Labels
  labs(x="Percent of respondents", y="",
       title="EcoTrust economic questions") +
  scale_x_continuous(labels=scales::percent) +
  # Legend
  scale_fill_manual(name="Score", values=RColorBrewer::brewer.pal(5, "RdBu")) +
  guides(fill = guide_legend(nrow = 1)) +
  # Theme
  theme_bw() + my_theme
g2

ggsave(g2, filename=file.path(plotdir, "FigX_ecotrust_economic_qs.png"),
       width=6.5, height=3, units="in", dpi=600)

# Plot data
g3 <- ggplot(data %>% filter(category=="Environmental"),
            aes(x=prop, y=port_complex, fill=response %>% as.character())) +
  facet_wrap(~question, nrow=1) +
  geom_bar(stat="identity", col="grey30", lwd=0.1) +
  # Labels
  labs(x="Percent of respondents", y="",
       title="EcoTrust environment questions") +
  scale_x_continuous(labels=scales::percent) +
  # Legend
  scale_fill_manual(name="Score", values=RColorBrewer::brewer.pal(5, "RdBu")) +
  guides(fill = guide_legend(nrow = 1)) +
  # Theme
  theme_bw() + my_theme
g3

ggsave(g3, filename=file.path(plotdir, "FigX_ecotrust_environmental_qs.png"),
       width=6.5, height=3, units="in", dpi=600)

# Plot data
g4 <- ggplot(data %>% filter(category=="Social"),
            aes(x=prop, y=port_complex, fill=response %>% as.character())) +
  facet_wrap(~question, nrow=1) +
  geom_bar(stat="identity", col="grey30", lwd=0.1) +
  # Labels
  labs(x="Percent of respondents", y="",
       title="EcoTrust social questions") +
  scale_x_continuous(labels=scales::percent) +
  # Legend
  scale_fill_manual(name="Score", values=RColorBrewer::brewer.pal(5, "RdBu")) +
  guides(fill = guide_legend(nrow = 1)) +
  # Theme
  theme_bw() + my_theme
g4

ggsave(g4, filename=file.path(plotdir, "FigX_ecotrust_social_qs.png"),
       width=6.5, height=3, units="in", dpi=600)



