# Report synthesis
# ---
# code by Jacob Eurich, Jan 2022


# Clear workspace
rm(list = ls())


### Initialization ----------------------------------------------------------

# Packages
library(ggplot2)
library(tidyverse)
# install.packages("janitor")
library(janitor)
library(stringr)
library(ggrepel)

# Read data - Aurora
data_path <- "/home/shares/ca-mpa" # 
input_file <- "Technical_report_synthesis_21Jan22.xlsx" 
# data_path <- "/home/shares/ca-mpa/GD_data"
# input_file <- "Technical_report_synthesis.xlsx"
data_raw <- readxl::read_excel(file.path(data_path, input_file), sheet=2, skip = 0, na="NA")

# Read data - JE local drive
setwd("~/Documents/ACTIVE Research/NCEAS Postdoc/Data/Technical report synthesis")
datadir <- "data"
datafile <- "Technical_report_synthesis_24Jan22.xlsx"
data_raw <- readxl::read_excel(file.path(datadir, datafile), sheet=2, skip = 0, na="NA")

# Read data - Google Drive [didn't work]
# install.packages("googlesheets4")
# library("googlesheets4")
# install.packages("devtools")
# library("devtools")
# install.packages("cli")
# library("cli")
# devtools::install_github("tidyverse/googlesheets4")
# read_sheet("https://docs.google.com/spreadsheets/d/11jLgsWK0MufVpPq3j1YZQn0ClBLep-twFu3mcgHZ87U/edit#gid=2010159463")


### Format data ----------------------------------------------------------

# Clean data
data <- data_raw %>% 
  # simplify
  select(Question_ID, DEWG_dimension, Question, Habitat, Indicator, Variable, Variable_simplified, Method, California, North, Central, N_Channel_Islands, South) %>% # note: not including north central  
  # arrange
  arrange(Habitat, Question_ID, Variable)

# Count column
data = cbind(data,1)
names(data)[14] = "Count"

# Remove notes - "NS; ..." to "NS"
data$Indicator <- gsub(";.*","",data$Indicator, perl=TRUE)
data$Variable <- gsub(";.*","",data$Variable, perl=TRUE)
data$California <- gsub(";.*","",data$California, perl=TRUE)
data$North <- gsub(";.*","",data$North, perl=TRUE)
data$Central <- gsub(";.*","",data$Central, perl=TRUE)
data$N_Channel_Islands <- gsub(";.*","",data$N_Channel_Islands, perl=TRUE)
data$South <- gsub(";.*","",data$South, perl=TRUE)


### Inspect  ----------------------------------------------------------
colnames(data)
str(data)
table(data$Habitat)

# DEWG dimension
table(data$DEWG_dimension)
DEWG_pie = aggregate(Count ~ DEWG_dimension, data, sum)
DEWG_pie = DEWG_pie %>% mutate(Percentage = Count/sum(Count)*100)
DEWG_pie$Percentage <- round(DEWG_pie$Percentage)

ggplot(DEWG_pie, aes(x = "", y = Count, fill = DEWG_dimension)) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  scale_fill_brewer() +
  theme_void() +
  geom_label_repel(data = DEWG_pie,
    aes(y = Percentage, label = paste0(Percentage, "%")),
    size = 3.5, nudge_x = 0, show.legend = FALSE) +
  labs(fill="DEWG dimension")


# DEWG questions
table(data$Question_ID)
Q_pie = aggregate(Count ~ Question_ID, data, sum)


# Variables
table(data$Variable)
Var_pie = aggregate(Count ~ Variable_simplified, data, sum)

ggplot(Var_pie, aes(x = "", y = Count, fill = Variable_simplified)) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  scale_fill_brewer() +
  theme_void() +
  labs(fill="Variables used")


# Method
table(data$Method)
Method_pie = aggregate(Count ~ Method, data, sum)
Method_pie[2, 1] <- "ROV/HOV/BRUV"
Method_pie[4, 1] <- "ROV/HOV/BRUV"
Method_pie[5, 1] <- "ROV/HOV/BRUV"
Method_pie[6, 1] <- "ROV/HOV/BRUV"
Method_pie[7, 1] <- "ROV/HOV/BRUV"
Method_pie = aggregate(Count ~ Method, Method_pie, sum)

Method_pie = Method_pie %>% mutate(Percentage = Count/sum(Count)*100)
Method_pie$Percentage <- round(Method_pie$Percentage)

ggplot(Method_pie, aes(x = "", y = Count, fill = Method)) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  scale_fill_brewer() +
  theme_void() +
  labs(fill="Methods used")


### Plot  ----------------------------------------------------------

### CA-wide
table(data$California)
# Habitat
CA_wide = aggregate(Count ~ Habitat + California, data, sum)
CA_wide$Habitat = factor(CA_wide$Habitat, levels = c("Beach", "Rocky intertidal", "Kelp forest", "Deep reef", "CCFRP", "Commerical and CPFV"))
CA_wide$California = factor(CA_wide$California, levels = c("NA", "NS decrease", "NS", "NS increase", "S decrease", "S difference", "S increase"))
CA_wide = CA_wide[!(CA_wide$California=="NA"),]

ggplot(CA_wide, aes(x = California, y = Count, fill = Habitat)) + geom_bar(position="stack", stat = "summary") +
  theme_classic() +
  labs(y = expression ("Count of indicators")) + xlab("") +
  scale_y_continuous(expand= c(0,0)) +
  theme(axis.text.x = element_text(angle=45,  hjust = 1)) +
  theme(axis.title.y = element_text(vjust=2.5)) +
  theme(axis.text = element_text(color="black")) +
  scale_fill_brewer(palette="Spectral", direction = -1, name = "Habitat group") + theme(text = element_text(size=15)) + ggtitle("California")

# Variables
CA_wide_var = aggregate(Count ~ Variable_simplified + California, data, sum)
CA_wide_var$Variable_simplified = factor(CA_wide_var$Variable_simplified, levels = c("Abundance", "Biomass", "Density", "Size structure", "Diversity", "Richness or evenness", "Community composition", "Angler opinion", "BPUE", "CPUE"))
CA_wide_var$California = factor(CA_wide_var$California, levels = c("NA", "NS decrease", "NS", "NS increase", "S decrease", "S difference", "S increase"))
CA_wide_var = CA_wide_var[!(CA_wide_var$California=="NA"),]

ggplot(CA_wide_var, aes(x = California, y = Count, fill = Variable_simplified)) + geom_bar(position="stack", stat = "summary") +
  theme_classic() +
  labs(y = expression ("Count of indicators")) + xlab("") +
  scale_y_continuous(expand= c(0,0)) +
  theme(axis.text.x = element_text(angle=45,  hjust = 1)) +
  theme(axis.title.y = element_text(vjust=2.5)) +
  theme(axis.text = element_text(color="black")) +
  scale_fill_brewer(palette="Spectral", direction = -1, name = "Variables") + theme(text = element_text(size=15)) + ggtitle("California")


### Northern region
table(data$North)

# Habitat
N_reg = aggregate(Count ~ Habitat + North, data, sum)
N_reg$Habitat = factor(N_reg$Habitat, levels = c("Beach", "Rocky intertidal", "Kelp forest", "Deep reef", "CCFRP", "Commerical and CPFV"))
N_reg$North = factor(N_reg$North, levels = c("NA", "NS decrease", "NS", "NS increase", "S decrease", "S difference", "S increase"))
N_reg = N_reg[!(N_reg$North=="NA"),]

ggplot(N_reg, aes(x = North, y = Count, fill = Habitat)) + geom_bar(position="stack", stat = "summary") +
  theme_classic() +
  labs(y = expression ("Count of indicators")) + xlab("") +
  scale_y_continuous(expand= c(0,0)) +
  theme(axis.text.x = element_text(angle=45,  hjust = 1)) +
  theme(axis.title.y = element_text(vjust=2.5)) +
  theme(axis.text = element_text(color="black")) +
  scale_fill_brewer(palette="Spectral", direction = -1, name = "Habitat group") + theme(text = element_text(size=15)) + ggtitle("North")

# Variables
N_reg_var = aggregate(Count ~ Variable_simplified + North, data, sum)
N_reg_var$Variable_simplified = factor(N_reg_var$Variable_simplified, levels = c("Abundance", "Biomass", "Density", "Size structure", "Diversity", "Richness or evenness", "Community composition", "Angler opinion", "BPUE", "CPUE"))
N_reg_var$North = factor(N_reg_var$North, levels = c("NA", "NS decrease", "NS", "NS increase", "S decrease", "S difference", "S increase"))
N_reg_var = N_reg_var[!(N_reg_var$North=="NA"),]

ggplot(N_reg_var, aes(x = North, y = Count, fill = Variable_simplified)) + geom_bar(position="stack", stat = "summary") +
  theme_classic() +
  labs(y = expression ("Count of indicators")) + xlab("") +
  scale_y_continuous(expand= c(0,0)) +
  theme(axis.text.x = element_text(angle=45,  hjust = 1)) +
  theme(axis.title.y = element_text(vjust=2.5)) +
  theme(axis.text = element_text(color="black")) +
  scale_fill_brewer(palette="Spectral", direction = -1, name = "Variables") + theme(text = element_text(size=15)) + ggtitle("North")


### Central region
table(data$Central)

# Habitat
C_reg = aggregate(Count ~ Habitat + Central, data, sum)
C_reg$Habitat = factor(C_reg$Habitat, levels = c("Beach", "Rocky intertidal", "Kelp forest", "Deep reef", "CCFRP", "Commerical and CPFV"))
C_reg$Central = factor(C_reg$Central, levels = c("NA", "NS decrease", "NS", "NS increase", "S decrease", "S difference", "S increase"))
C_reg = C_reg[!(C_reg$Central=="NA"),]

ggplot(C_reg, aes(x = Central, y = Count, fill = Habitat)) + geom_bar(position="stack", stat = "summary") +
  theme_classic() +
  labs(y = expression ("Count of indicators")) + xlab("") +
  scale_y_continuous(expand= c(0,0)) +
  theme(axis.text.x = element_text(angle=45,  hjust = 1)) +
  theme(axis.title.y = element_text(vjust=2.5)) +
  theme(axis.text = element_text(color="black")) +
  scale_fill_brewer(palette="Spectral", direction = -1, name = "Habitat group") + theme(text = element_text(size=15)) + ggtitle("Central")

# Variables
C_reg_var = aggregate(Count ~ Variable_simplified + Central, data, sum)
C_reg_var$Variable_simplified = factor(C_reg_var$Variable_simplified, levels = c("Abundance", "Biomass", "Density", "Size structure", "Diversity", "Richness or evenness", "Community composition", "Angler opinion", "BPUE", "CPUE"))
C_reg_var$Central = factor(C_reg_var$Central, levels = c("NA", "NS decrease", "NS", "NS increase", "S decrease", "S difference", "S increase"))
C_reg_var = C_reg_var[!(C_reg_var$Central=="NA"),]

ggplot(C_reg_var, aes(x = Central, y = Count, fill = Variable_simplified)) + geom_bar(position="stack", stat = "summary") +
  theme_classic() +
  labs(y = expression ("Count of indicators")) + xlab("") +
  scale_y_continuous(expand= c(0,0)) +
  theme(axis.text.x = element_text(angle=45,  hjust = 1)) +
  theme(axis.title.y = element_text(vjust=2.5)) +
  theme(axis.text = element_text(color="black")) +
  scale_fill_brewer(palette="Spectral", direction = -1, name = "Variables") + theme(text = element_text(size=15)) + ggtitle("Central")


### Southern region
table(data$South)

# Habitat
S_reg = aggregate(Count ~ Habitat + South, data, sum)
S_reg$Habitat = factor(S_reg$Habitat, levels = c("Beach", "Rocky intertidal", "Kelp forest", "Deep reef", "CCFRP", "Commerical and CPFV"))
S_reg$South = factor(S_reg$South, levels = c("NA", "NS decrease", "NS", "NS increase", "S decrease", "S difference", "S increase"))
S_reg = S_reg[!(S_reg$South=="NA"),]

ggplot(S_reg, aes(x = South, y = Count, fill = Habitat)) + geom_bar(position="stack", stat = "summary") +
  theme_classic() +
  labs(y = expression ("Count of indicators")) + xlab("") +
  scale_y_continuous(expand= c(0,0)) +
  theme(axis.text.x = element_text(angle=45,  hjust = 1)) +
  theme(axis.title.y = element_text(vjust=2.5)) +
  theme(axis.text = element_text(color="black")) +
  scale_fill_brewer(palette="Spectral", direction = -1, name = "Habitat group") + theme(text = element_text(size=15)) + ggtitle("South")

# Variables
S_reg_var = aggregate(Count ~ Variable_simplified + South, data, sum)
S_reg_var$Variable_simplified = factor(S_reg_var$Variable_simplified, levels = c("Abundance", "Biomass", "Density", "Size structure", "Diversity", "Richness or evenness", "Community composition", "Angler opinion", "BPUE", "CPUE"))
S_reg_var$South = factor(S_reg_var$South, levels = c("NA", "NS decrease", "NS", "NS increase", "S decrease", "S difference", "S increase"))
S_reg_var = S_reg_var[!(S_reg_var$South=="NA"),]

ggplot(S_reg_var, aes(x = South, y = Count, fill = Variable_simplified)) + geom_bar(position="stack", stat = "summary") +
  theme_classic() +
  labs(y = expression ("Count of indicators")) + xlab("") +
  scale_y_continuous(expand= c(0,0)) +
  theme(axis.text.x = element_text(angle=45,  hjust = 1)) +
  theme(axis.title.y = element_text(vjust=2.5)) +
  theme(axis.text = element_text(color="black")) +
  scale_fill_brewer(palette="Spectral", direction = -1, name = "Variables") + theme(text = element_text(size=15)) + ggtitle("South")
