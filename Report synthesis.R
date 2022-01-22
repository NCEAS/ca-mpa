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
datafile <- "Technical_report_synthesis_21Jan22.xlsx"
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

# Count column
data = cbind(data,1)
names(data)[13] = "Count"

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
Var_pie = aggregate(Count ~ Variable, data, sum)

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




