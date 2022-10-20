
# Clear
rm(list = ls())
options(dplyr.summarise.inform=F)

# Setup
################################################################################

# Packages
library(wcfish)
library(shiny)
library(shinythemes)
library(tidyverse)
library(RColorBrewer)

# Directories
datadir <- "data" # for actual app
codedir <- "code"  # for actual app
# datadir <- "data/habitat_cdfw/shiny_app/data" # when testing
# codedir <- "data/habitat_cdfw/shiny_app/code" # when testing

# Source code
sapply(list.files(codedir), function(x) source(file.path(codedir, x)))

# Read bottom substrate
substrate_ras <- raster::raster(file.path(datadir, "CA_bottom_substrate_10m.tiff")) 

# Get MPAs
mpas_sf <- wcfish::mpas_ca %>% 
  sf::st_transform(crs=raster::crs(substrate_ras))

# Get land
# usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
land <- rnaturalearth::ne_countries(country=c("United States of America", "Mexico"),
                                    scale="large", returnclass = "sf") %>% 
  sf::st_transform(crs=raster::crs(substrate_ras))

# Get state waters
state_waters_line <- readRDS(file.path(datadir, "CA_state_waters_polyline.Rds")) %>% 
  sf::st_transform(crs=raster::crs(substrate_ras))

# Parameters
################################################################################

# Parameters
mpas <- sort(unique(mpas_sf$name))


# User interface
################################################################################

# User interface
ui <- fluidPage(
  
  # Title
  titlePanel("California MPA habitat explorer"),

  # Select by nutrient
  selectInput(inputId = "mpa", label = "Select MPA:",
             choices = mpas,  multiple = F, selected=mpas[1]),
  plotOutput(outputId = "plot_map", width=600, height=425)
     
)


# Server
################################################################################

# Server
server <- function(input, output, session){

  # Plot coverage
  output$plot_map <- renderPlot({
    g <- plot_map(mpa = input$mpa)
    g
  })

}

shinyApp(ui = ui, server = server)
