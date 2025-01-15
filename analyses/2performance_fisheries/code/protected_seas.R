## Process Protected Seas Data
## Cori Lopazanski
## 10 Aug 2023

# About ------------
# This script explores and cleans the Protected Seas data for California.
# https://navigatormap.org/
#
# The shapefile data were downloaded from the website directly on 27 July 2023.
# -- /protected-seas/ps-download-23072023
#
# The current version of the regulatory data was provided by Protected Seas
# Timothe Vincent on 9 Aug 2023.
# --/protected-seas/ProSeas_CA_data_history_CORI
#     File 1: ProtectedSeas_California_regulatory_data_08092023_Cori_Lopazanski
#        Regulatory data for ProtectedSeas' California sites as of 08.09.20203
#
#     File 2: ProtectedSeas_California_regulatory_data_HISTORY_08092023_Cori_Lopazanski
#       Regulatory data history of over 14,000 entries, ordered by site ID and 
#       last update date, of our California data since 2015. We document changes 
#       in the last_update, change, change_source or change_notes fields. Please 
#       note that anything marked URL check, url update, grammar, etc is unlikely 
#       to be a regulatory change.

# Setup ------------

# Packages
library(tidyverse)
library(janitor)
library(sf)


# Directories
datadir <- "/Users/lopazanski/Documents/github/nceas/protected-seas"

# Read Data
ps_current <- read_csv(file.path(datadir, "ProSeas_CA_data_history_CORI", 
                                 "ProtectedSeas_California_regulatory_data_08092023_Cori_Lopazanski.csv")) %>% clean_names()
ps_history <- read_csv(file.path(datadir, "ProSeas_CA_data_history_CORI", 
                                 "ProtectedSeas_California_regulatory_data_HISTORY_08092023_Cori_Lopazanski.csv"))%>% clean_names()
ps_shapes  <- read_sf(file.path(datadir, "ps-download-23072023", "mpa_ca_shp",
                                "shapefile", "aicampas_attributes"))%>% clean_names()















