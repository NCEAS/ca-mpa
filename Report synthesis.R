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

# Read data - Aurora
data_path <- "/home/shares/ca-mpa" # JB, I had to add the file manually via cyberduck, can you get the GD_data to auto sync?]
input_file <- "Technical_report_synthesis_20Jan22.xlsx" # added manually, date to note that it's not our live version
# data_path <- "/home/shares/ca-mpa/GD_data" # JB, ideally this would work
# input_file <- "Technical_report_synthesis.xlsx" # JB, ideally this would work
data_raw <- readxl::read_excel(file.path(data_path, input_file), sheet=2, skip = 0, na="NA")

# Read data - JE local drive
setwd("~/Documents/ACTIVE Research/NCEAS Postdoc/Data/Technical report synthesis")
datadir <- "data"
datafile <- "Technical_report_synthesis_20Jan22.xlsx"
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
  select(Question_ID, DEWG_dimension, Question, Habitat, Indicator, Variable, Method, California, North, Central, N_Channel_Islands, South) %>% # note: not including north central  
  # arrange
  arrange(Habitat, Question_ID, Variable)

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
table(data$Question_ID)
table(data$DEWG_dimension)

