
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
# datadir <- "data/habitat_merge/shiny_app/data" # when testing
# codedir <- "data/habitat_merge/shiny_app/code" # when testing

# Projection
nad83_utm <- "+proj=aea +lat_0=0 +lon_0=-120 +lat_1=34 +lat_2=40.5 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +no_defs"

# Source code
sapply(list.files(codedir), function(x) source(file.path(codedir, x)))

# Read bottom substrate
substrate_by_mpa <- readRDS(file.path(datadir, "bottom_substrate_by_mpa_long.Rds")) 
substrate_orig <- readRDS(file.path(datadir, "mpa_substrate_rasters.Rds")) 

# Get MPAs
mpas_sf <- readRDS(file.path(datadir, "ca_mpas.Rds"))

# Get land
land <- readRDS(file.path(datadir, "land.Rds"))

# Get state waters
state_waters_line <- readRDS(file.path(datadir, "CA_state_waters_polyline.Rds")) %>% 
  sf::st_transform(crs=nad83_utm)


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

  # Select by MPA
  selectInput(inputId = "mpa", label = "Select MPA:",
             choices = mpas,  multiple = F, selected=mpas[1]),
  
  # Plot map
  plotOutput(outputId = "plot_map", height=500),
  br()
  
  # Plot coverage
  # plotOutput(outputId = "plot_barplot", width=750, height=200),
     
)


# Server
################################################################################

# Server
server <- function(input, output, session){

  # Plot map
  output$plot_map <- renderPlot({
    g <- plot_map(mpa = input$mpa,
                  mpas_sf = mpas_sf,
                  substrate_orig = substrate_orig,
                  land = land,
                  state_waters_line = state_waters_line)
    g
  })
  
  # Plot bar plot
  output$plot_barplot <- renderPlot({
    g <- plot_barplot(mpa = input$mpa)
    g
  })

}

shinyApp(ui = ui, server = server)
