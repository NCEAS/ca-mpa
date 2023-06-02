#title: "Site table shint"
#author: "Joshua G. Smith"
#date: "5/30/2023"

rm(list=ls())

#required packages
librarian::shelf(ggplot2, tidyverse, tmap, sf, shiny)

#Directories
basedir <- "/home/shares/ca-mpa/data/sync-data"
gisdir <- file.path(basedir, "gis_data/processed")

# Read data
mpas_orig <- readRDS(file.path(gisdir, "CA_MPA_polygons.Rds")) 

# Read site locations
sites <- read.csv(file.path(basedir, "monitoring/site_tables/processed/monitoring_site_table.csv"))

################################################################################
#process data

mlpa_mpas <- mpas_orig %>% filter(type == "SMR" | type == "SMCA" | type == "SMCA (No-Take)") %>%
              dplyr::rename("MPA type" = type) 


################################################################################
#tmap

sites <- sites %>% ungroup() %>% data.frame() %>% 
  mutate(group = factor(group, levels = c("Sandy beach","Surf zone","Intertidal long-term","Intertidal biodiversity",
                                          "Kelp forest","CCFRP","Deep reef")))

# Make spatial
sites_spat <- sites %>% 
  drop_na(lon_wgs84, lat_wgs84) %>% 
  st_as_sf(., coords = c("lon_wgs84", "lat_wgs84"), crs = 4326)


ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(".leaflet-container { height: 800px !important; }"),
      HTML("HTML, body {background-color: transparent;}")
    )
  ),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Select Year:", min = 2007, max = 2020, value = 2007, sep = ""),
      selectInput("group", "Select Group:", choices = c("All", "Sandy beach", "Surf zone", "Intertidal long-term", "Intertidal biodiversity", "Kelp forest", "CCFRP", "Deep reef"), multiple = TRUE)
      # Add the selectInput for the 'group' variable with the available choices
      # Set multiple = TRUE to allow selecting multiple groups
    ),
    mainPanel(
      tmapOutput("map"),
      br(),
      p("Create by: Joshua G. Smith - National Center for Ecological Analysis and Synthesis"),
    )
  )
)


server <- function(input, output) {
  sf_use_s2(FALSE)  # Disable S2 geometry library
  
  # Read the data frame
  sites_spat 
  
  # Convert necessary columns to numeric and factor
  sites_spat$year <- as.integer(sites_spat$year)
  sites_spat$group <- as.factor(sites_spat$group)  # Convert 'group' to factor
  
  # Create a reactive expression for filtered data based on the selected year and groups
  filteredData <- reactive({
    year <- input$year
    groups <- input$group
    
    if (is.null(groups) || length(groups) == 0 || "All" %in% groups) {
      # If no groups are selected or "All" is selected, return all data
      sites_spat[sites_spat$year == year, ]
    } else {
      filtered <- sites_spat[sites_spat$year == year & sites_spat$group %in% groups, ]
      if (nrow(filtered) == 0) {
        # If the filtered data is empty, return a data frame with no rows
        data.frame(lon_wgs84 = numeric(), lat_wgs84 = numeric(), group = factor())
      } else {
        filtered
      }
    }
  })
  
  # Render the map
  output$map <- renderTmap({
    if (nrow(filteredData()) == 0) {
      # If the filtered data is empty, display a message
      tm_shape(sites_spat) +
        tm_text("Please make a selection.")
    } else {
      tm_shape(mlpa_mpas) +
        tm_polygons(col = "indianred")+
      tm_shape(filteredData()) +
        tm_symbols(col = "group", palette = "Set1", size = 0.01) +
        tm_legend(show = TRUE)
    }
  })
}

shinyApp(ui, server)

################################################################################
#publish
library(rsconnect)



rsconnect::deployApp(here::here("data","monitoring_data","monitoring_site_table","site_table_shiny.R"))










